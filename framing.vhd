-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

-- MAC sublayer functionality (en-/decapsulation, FCS, IPG)

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.ethernet_types.all;
use work.framing_common.all;
use work.crc32.all;
use work.utility.all;

entity framing is
	port(
		tx_reset_i             : in  std_ulogic;
		tx_clock_i             : in  std_ulogic;
		rx_reset_i             : in  std_ulogic;
		rx_clock_i             : in  std_ulogic;

		-- For details on the signals, see the port list of mii_gmii

		-- TX from client logic
		-- The length/type field is considered part of the data!
		-- It is not interpreted by the framing layer at all.
		tx_enable_i            : in  std_ulogic;
		tx_data_i              : in  t_ethernet_data;
		tx_byte_sent_o         : out std_ulogic;
		-- Do not start new frames while asserted
		-- (continuing the previous one is alright)
		tx_busy_o              : out std_ulogic;

		-- RX to client logic 
		rx_frame_o             : out std_ulogic;
		rx_data_o              : out t_ethernet_data;
		rx_byte_received_o     : out std_ulogic;
		rx_error_o             : out std_ulogic;

		-- TX to MII
		mii_tx_enable_o        : out std_ulogic;
		mii_tx_data_o          : out t_ethernet_data;
		mii_tx_byte_sent_i     : in  std_ulogic;
		mii_tx_gap_o           : out std_ulogic;

		-- RX from MII
		mii_rx_frame_i         : in  std_ulogic;
		mii_rx_data_i          : in  t_ethernet_data;
		mii_rx_byte_received_i : in  std_ulogic;
		mii_rx_error_i         : in  std_ulogic
	);
end entity;

architecture rtl of framing is
	-- Transmission
	type t_tx_state is (
		TX_IDLE,
		TX_PREAMBLE2,
		TX_PREAMBLE3,
		TX_PREAMBLE4,
		TX_PREAMBLE5,
		TX_PREAMBLE6,
		TX_PREAMBLE7,
		TX_START_FRAME_DELIMITER,
		TX_CLIENT_DATA,
		TX_PAD,
		TX_FRAME_CHECK_SEQUENCE2,
		TX_FRAME_CHECK_SEQUENCE3,
		TX_FRAME_CHECK_SEQUENCE4,
		TX_INTERPACKET_GAP
	);

	signal tx_state                   : t_tx_state                                      := TX_IDLE;
	signal tx_padding_required        : natural range 0 to MIN_FRAME_DATA_BYTES + 4 + 1 := 0;
	signal tx_interpacket_gap_counter : integer range 0 to INTERPACKET_GAP_BYTES;

	signal tx_frame_check_sequence : t_crc32;

	-- Reception
	type t_rx_state is (
		RX_WAIT_START_FRAME_DELIMITER,
		RX_DATA,
		RX_ERROR,
		RX_SKIP_FRAME
	);

	signal rx_state                : t_rx_state := RX_WAIT_START_FRAME_DELIMITER;
	signal rx_frame_check_sequence : t_crc32;
	subtype t_rx_frame_size is natural range 0 to MAX_FRAME_DATA_BYTES + CRC32_BYTES + 1;
	signal rx_frame_size : t_rx_frame_size;

begin
	-- Pass mii_tx_byte_sent_i through directly as long as data is being transmitted
	-- to avoid having to prefetch data in the synchronous process
	tx_byte_sent_o <= '1' when (tx_state = TX_CLIENT_DATA and mii_tx_byte_sent_i = '1') else '0';

	-- Transmission state machine
	tx_fsm_sync : process(tx_reset_i, tx_clock_i)
	begin
		if tx_reset_i = '1' then
			tx_state        <= TX_IDLE;
			mii_tx_enable_o <= '0';
			tx_busy_o       <= '1';
		elsif rising_edge(tx_clock_i) then
			mii_tx_enable_o <= '0';
			tx_busy_o       <= '0';
			if tx_state = TX_IDLE then
				if tx_enable_i = '1' then
					-- Jump straight into preamble to save a clock cycle of latency
					tx_state        <= TX_PREAMBLE2;
					mii_tx_data_o   <= PREAMBLE_DATA;
					mii_tx_enable_o <= '1';
					mii_tx_gap_o    <= '0';
					tx_busy_o       <= '1';
				end if;
			else
				-- Keep TX enable and busy asserted at all times
				mii_tx_enable_o <= '1';
				tx_busy_o       <= '1';

				-- Use mii_tx_byte_sent_i as clock enable
				if mii_tx_byte_sent_i = '1' then
					mii_tx_gap_o <= '0';

					case tx_state is
						when TX_IDLE =>
							-- Handled above, cannot happen here
							null;
						when TX_PREAMBLE2 | TX_PREAMBLE3 | TX_PREAMBLE4 | TX_PREAMBLE5 | TX_PREAMBLE6 =>
							tx_state      <= t_tx_state'succ(tx_state);
							mii_tx_data_o <= PREAMBLE_DATA;
						when TX_PREAMBLE7 =>
							tx_state      <= TX_START_FRAME_DELIMITER;
							mii_tx_data_o <= PREAMBLE_DATA;
						when TX_START_FRAME_DELIMITER =>
							tx_state                <= TX_CLIENT_DATA;
							mii_tx_data_o           <= START_FRAME_DELIMITER_DATA;
							-- Load padding register
							tx_padding_required     <= MIN_FRAME_DATA_BYTES;
							-- Load FCS
							-- Initial value is 0xFFFFFFFF which is equivalent to inverting the first 32 bits of the frame
							-- as required in clause 3.2.9 a
							tx_frame_check_sequence <= (others => '1');
						when TX_CLIENT_DATA =>
							mii_tx_data_o <= tx_data_i;
							if tx_enable_i = '0' then
								-- No more user data was available, next value has to be sent
								-- in this clock cycle already
								if tx_padding_required = 0 then
									-- Send FCS byte 1 now, byte 2 in next cycle
									tx_state      <= TX_FRAME_CHECK_SEQUENCE2;
									mii_tx_data_o <= fcs_output_byte(tx_frame_check_sequence, 0);
								else
									tx_state                <= TX_PAD;
									mii_tx_data_o           <= PADDING_DATA;
									-- Be sure to update FCS with one padding byte!
									tx_frame_check_sequence <= update_crc32(tx_frame_check_sequence, PADDING_DATA);
								end if;
							else
								-- Update FCS only as long as user data was available
								tx_frame_check_sequence <= update_crc32(tx_frame_check_sequence, tx_data_i);
							end if;
						when TX_PAD =>
							mii_tx_data_o <= PADDING_DATA;
							if tx_padding_required = 0 then
								-- When required=0, previous one was the last one -> send FCS
								tx_state      <= TX_FRAME_CHECK_SEQUENCE2;
								mii_tx_data_o <= fcs_output_byte(tx_frame_check_sequence, 0);
							else
								tx_frame_check_sequence <= update_crc32(tx_frame_check_sequence, PADDING_DATA);
							end if;
						when TX_FRAME_CHECK_SEQUENCE2 =>
							tx_state      <= t_tx_state'succ(tx_state);
							mii_tx_data_o <= fcs_output_byte(tx_frame_check_sequence, 1);
						when TX_FRAME_CHECK_SEQUENCE3 =>
							tx_state      <= t_tx_state'succ(tx_state);
							mii_tx_data_o <= fcs_output_byte(tx_frame_check_sequence, 2);
						when TX_FRAME_CHECK_SEQUENCE4 =>
							tx_state                   <= TX_INTERPACKET_GAP;
							mii_tx_data_o              <= fcs_output_byte(tx_frame_check_sequence, 3);
							-- Load IPG counter with initial value
							tx_interpacket_gap_counter <= 0;
						when TX_INTERPACKET_GAP =>
							-- Only state where the MAC is still busy but no data is actually sent
							mii_tx_gap_o <= '1';
							if tx_interpacket_gap_counter = INTERPACKET_GAP_BYTES - 1 then
								-- Last IPG byte is transmitted in this cycle
								tx_state <= TX_IDLE;
							else
								tx_interpacket_gap_counter <= tx_interpacket_gap_counter + 1;
							end if;
					end case;

					if tx_state = TX_CLIENT_DATA or tx_state = TX_PAD then
						-- Decrement required padding
						if tx_padding_required > 0 then
							tx_padding_required <= tx_padding_required - 1;
						end if;
					end if;
				end if;
			end if;
		end if;
	end process;

	-- Reception state machine
	rx_fsm_sync : process(rx_reset_i, rx_clock_i)
	begin
		if rx_reset_i = '1' then
			rx_state <= RX_WAIT_START_FRAME_DELIMITER;
		elsif rising_edge(rx_clock_i) then
			rx_error_o         <= '0';
			rx_data_o          <= mii_rx_data_i;
			rx_byte_received_o <= '0';
			rx_frame_o         <= '0';

			case rx_state is
				when RX_WAIT_START_FRAME_DELIMITER =>
					-- Reset frame size and FCS
					rx_frame_size           <= 0;
					-- Initial value is 0xFFFFFFFF which is equivalent to inverting the first 32 bits of the frame
					-- as required in clause 3.2.9 a
					rx_frame_check_sequence <= (others => '1');

					if mii_rx_frame_i = '1' then
						if mii_rx_byte_received_i = '1' then
							case mii_rx_data_i is
								when START_FRAME_DELIMITER_DATA =>
									rx_state <= RX_DATA;
								when PREAMBLE_DATA =>
									-- Do nothing, wait for end of preamble
									null;
								when others =>
									-- The frame needs to be thrown away, but there is no need to 
									-- inform the higher layer since nothing of value was actually "received" anyway.
									rx_state <= RX_SKIP_FRAME;
							end case;
						end if;
						if mii_rx_error_i = '1' then
							-- Same here						
							rx_state <= RX_SKIP_FRAME;
						end if;
					end if;
				when RX_DATA =>
					rx_frame_o         <= '1';
					rx_byte_received_o <= mii_rx_byte_received_i;
					if mii_rx_frame_i = '0' then
						rx_state <= RX_WAIT_START_FRAME_DELIMITER;
						-- Remaining FCS after parsing whole packet + FCS needs to be a specific value
						if mii_rx_error_i = '1' or rx_frame_check_sequence /= CRC32_POSTINVERT_MAGIC or rx_frame_size < MIN_FRAME_DATA_BYTES + CRC32_BYTES or rx_frame_size > MAX_FRAME_DATA_BYTES + CRC32_BYTES then
							rx_error_o <= '1';
						end if;
					else
						if mii_rx_byte_received_i = '1' then
							-- Update FCS check
							rx_frame_check_sequence <= update_crc32(rx_frame_check_sequence, mii_rx_data_i);
							-- Increase frame size
							if rx_frame_size < t_rx_frame_size'high then
								rx_frame_size <= rx_frame_size + 1;
							end if;
						end if;
						if mii_rx_error_i = '1' then
							-- Skip the rest of the frame and tell the higher layer
							rx_state <= RX_ERROR;
						end if;
					end if;
				when RX_SKIP_FRAME =>
					-- Skip the currently receiving frame without signaling the higher layer
					if mii_rx_frame_i = '0' then
						rx_state <= RX_WAIT_START_FRAME_DELIMITER;
					end if;
				when RX_ERROR =>
					-- Skip the currently receiving frame and signal the higher layer
					rx_frame_o <= '1';
					rx_error_o <= '1';
					if mii_rx_frame_i = '0' then
						rx_state <= RX_WAIT_START_FRAME_DELIMITER;
					end if;
			end case;
		end if;
	end process;

end architecture;
