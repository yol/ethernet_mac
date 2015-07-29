-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


use work.ethernet_types.all;
use work.framing_types.all;
use work.crc32.all;
use work.utility.all;

entity eth_framing is
	port(
		reset_i               : in  std_logic;

		tx_clock_i            : in  std_ulogic;
		rx_clock_i            : in  std_ulogic;

		-- TX from client logic
		-- The length/type field is considered part of the data!
		-- It is not interpreted by the framing layer at all.
		tx_enable_i           : in  std_ulogic;
		tx_data_i             : in  ethernet_data_t;
		tx_byte_sent_o        : out std_ulogic;
		tx_busy_o             : out std_ulogic;

		-- RX to client logic 
		rx_frame_o            : out std_ulogic;
		rx_data_o             : out ethernet_data_t;
		rx_byte_received_o    : out std_ulogic;
		rx_error_o            : out std_ulogic;

		-- TX to RS		
		rs_tx_enable_o        : out std_ulogic;
		rs_tx_data_o          : out ethernet_data_t;
		rs_tx_byte_sent_i     : in  std_ulogic;
		rs_tx_busy_i          : in  std_ulogic;

		-- RX from RS
		rs_rx_frame_i         : in  std_ulogic;
		rs_rx_data_i          : in  ethernet_data_t;
		rs_rx_byte_received_i : in  std_ulogic;
		rs_rx_error_i         : in  std_ulogic
	);
end entity;

architecture rtl of eth_framing is
	type tx_state_t is (
		TX_IDLE,
		TX_PREAMBLE1,
		TX_PREAMBLE2,
		TX_PREAMBLE3,
		TX_PREAMBLE4,
		TX_PREAMBLE5,
		TX_PREAMBLE6,
		TX_PREAMBLE7,
		TX_START_FRAME_DELIMITER,
		TX_CLIENT_DATA,
		TX_PAD,
		TX_FRAME_CHECK_SEQUENCE1,
		TX_FRAME_CHECK_SEQUENCE2,
		TX_FRAME_CHECK_SEQUENCE3,
		TX_FRAME_CHECK_SEQUENCE4
	);

	type rx_state_t is (
		RX_WAIT_START_FRAME_DELIMITER,
		RX_DATA,
		RX_ERROR,
		RX_SKIP_FRAME
	);

	signal tx_state      : tx_state_t      := TX_IDLE;
	signal tx_next_state : tx_state_t      := TX_IDLE;
	signal tx_enable     : std_ulogic      := '0';
	signal tx_data       : ethernet_data_t := (others => '0');
	signal rs_tx_data    : ethernet_data_t := (others => '0');

	signal tx_padding_required : natural range 0 to MIN_FRAME_DATA_BYTES + 4 + 1 := 0;

	signal tx_frame_check_sequence : crc32_result_t := (others => '0');

	impure function fcs_output_byte(byte : integer) return std_ulogic_vector is
	begin
		return not reverse_vector(tx_frame_check_sequence)((((byte + 1) * 8) - 1) downto byte * 8);
	end function fcs_output_byte;

	signal rx_state                : rx_state_t     := RX_WAIT_START_FRAME_DELIMITER;
	signal rx_frame_check_sequence : crc32_result_t := (others => '0');
	subtype rx_frame_size_t is natural range 0 to MAX_FRAME_DATA_BYTES + CRC32_BYTES + 1;
	signal rx_frame_size : rx_frame_size_t := 0;

begin
	rs_tx_data_o <= rs_tx_data;
	--tx_busy_o <= rs_tx_busy_i;

	--	tx_output : process(tx_state, tx_next_state, tx_data_i, rs_tx_busy_i, rs_tx_byte_sent_i, frame_check_sequence)
	--	begin
	--		case tx_state is
	--			when TX_IDLE =>
	--				null;
	--			when TX_PREAMBLE1 | TX_PREAMBLE2 | TX_PREAMBLE3 | TX_PREAMBLE4 | TX_PREAMBLE5 | TX_PREAMBLE6 | TX_PREAMBLE7 =>
	--				
	--			when TX_START_FRAME_DELIMITER =>
	--				
	--			when TX_DATA =>
	--				-- Passing the data through directly means a look-ahead is necessary for the next state!
	--				-- See below
	--
	--			when TX_PAD =>
	--				-- Overwrite busy state
	--				--tx_busy_o    <= '1';
	--				--rs_tx_data_o <= (others => '0');
	--				null;
	--			when TX_FRAME_CHECK_SEQUENCE1 =>
	--				-- Overwrite busy state
	--				--tx_busy_o    <= '1';
	--				--rs_tx_data_o <= frame_check_sequence(31 downto 24);
	--				null;
	--			when TX_FRAME_CHECK_SEQUENCE2 =>
	--				
	--
	--		--when others => null;
	--		end case;
	--
	--		-- Look-ahead in state TX_DATA
	--		if tx_state = TX_PAD or tx_next_state = TX_PAD then
	--			
	--		end if;
	--		
	--		if tx_state = TX_FRAME_CHECK_SEQUENCE1 or (tx_state = TX_DATA and tx_next_state = TX_FRAME_CHECK_SEQUENCE1) then
	--			
	--		end if;
	--	end process;

	tx_fsm_output : process(tx_state, tx_enable, tx_enable_i, tx_padding_required, tx_data, rs_tx_busy_i, rs_tx_byte_sent_i, tx_frame_check_sequence)
	begin
		tx_next_state <= tx_state;

		tx_byte_sent_o <= '0';
		rs_tx_data     <= (others => '0');
		rs_tx_enable_o <= '0';
		-- Mirror RS busy state by default
		tx_busy_o      <= rs_tx_busy_i;

		if tx_state /= TX_IDLE then
			rs_tx_enable_o <= '1';
		end if;

		-- State transition is guarded by rs_tx_byte_sent_i
		case tx_state is
			when TX_IDLE =>
				if tx_enable = '1' then
					tx_next_state <= TX_PREAMBLE1;
				end if;
			when TX_PREAMBLE1 | TX_PREAMBLE2 | TX_PREAMBLE3 | TX_PREAMBLE4 | TX_PREAMBLE5 | TX_PREAMBLE6 =>
				tx_next_state <= tx_state_t'succ(tx_state);
				rs_tx_data    <= PREAMBLE_DATA;
			when TX_PREAMBLE7 =>
				tx_next_state <= TX_START_FRAME_DELIMITER;
				rs_tx_data    <= PREAMBLE_DATA;
			when TX_START_FRAME_DELIMITER =>
				tx_next_state  <= TX_CLIENT_DATA;
				rs_tx_data     <= START_FRAME_DELIMITER_DATA;
				-- When the next clock reaches the receiver, the first byte is already
				-- captured by the tx_data FF
				tx_byte_sent_o <= rs_tx_byte_sent_i;
			when TX_CLIENT_DATA =>
				rs_tx_data     <= tx_data;
				tx_byte_sent_o <= rs_tx_byte_sent_i;
				if tx_enable_i = '0' then
					if tx_padding_required = 0 then
						tx_next_state <= TX_FRAME_CHECK_SEQUENCE1;
					else
						tx_next_state <= TX_PAD;
					end if;
				end if;
			when TX_PAD =>
				rs_tx_data <= PADDING_DATA;
				if tx_padding_required = 0 then
					-- When required=1, this was the last one
					tx_next_state <= TX_FRAME_CHECK_SEQUENCE1;
				end if;
			when TX_FRAME_CHECK_SEQUENCE1 =>
				tx_next_state <= tx_state_t'succ(tx_state);
				rs_tx_data    <= fcs_output_byte(0);
			when TX_FRAME_CHECK_SEQUENCE2 =>
				tx_next_state <= tx_state_t'succ(tx_state);
				rs_tx_data    <= fcs_output_byte(1);
			when TX_FRAME_CHECK_SEQUENCE3 =>
				tx_next_state <= tx_state_t'succ(tx_state);
				rs_tx_data    <= fcs_output_byte(2);
			when TX_FRAME_CHECK_SEQUENCE4 =>
				tx_next_state <= TX_IDLE;
				rs_tx_data    <= fcs_output_byte(3);
		end case;
	end process;

	tx_fsm_sync : process(reset_i, tx_clock_i)
	begin
		-- Asynchronous reset, tx_clock is not guaranteed to be running
		if reset_i = '1' then
			tx_state <= TX_IDLE;
		elsif rising_edge(tx_clock_i) then
			if tx_state = TX_IDLE or rs_tx_byte_sent_i = '1' then
				tx_state  <= tx_next_state;
				tx_data   <= tx_data_i;
				tx_enable <= tx_enable_i;

				case tx_next_state is
					when TX_START_FRAME_DELIMITER =>
						-- Load padding register
						tx_padding_required     <= MIN_FRAME_DATA_BYTES;
						-- Load FCS
						-- Initial value is 0xFFFFFFFF which is equivalent to inverting the first 32 bits of the frame
						-- as required in clause 3.2.9 a
						tx_frame_check_sequence <= (others => '1');
					when TX_CLIENT_DATA | TX_PAD =>
						-- Decrement required padding
						if tx_padding_required > 0 then
							tx_padding_required <= tx_padding_required - 1;
						end if;
						-- Update FCS
						if tx_next_state = TX_CLIENT_DATA then
							tx_frame_check_sequence <= update_crc32(tx_frame_check_sequence, tx_data_i);
						else
							tx_frame_check_sequence <= update_crc32(tx_frame_check_sequence, PADDING_DATA);
						end if;
					when others =>
						null;
				end case;
			end if;
		end if;
	end process;

	rx_fsm_sync : process(reset_i, rx_clock_i)
	begin
		if reset_i = '1' then
			rx_state <= RX_WAIT_START_FRAME_DELIMITER;
		elsif rising_edge(rx_clock_i) then
			rx_error_o         <= '0';
			rx_data_o          <= rs_rx_data_i;
			rx_byte_received_o <= '0';
			rx_frame_o         <= '0';

			case rx_state is
				when RX_WAIT_START_FRAME_DELIMITER =>
					-- Reset frame size and FCS
					rx_frame_size           <= 0;
					-- Initial value is 0xFFFFFFFF which is equivalent to inverting the first 32 bits of the frame
					-- as required in clause 3.2.9 a
					rx_frame_check_sequence <= (others => '1');

					if rs_rx_frame_i = '1' then
						if rs_rx_byte_received_i = '1' then
							case rs_rx_data_i is
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
						if rs_rx_error_i = '1' then
							-- Same here						
							rx_state <= RX_SKIP_FRAME;
						end if;
					end if;
				when RX_DATA =>
					rx_frame_o         <= '1';
					rx_byte_received_o <= rs_rx_byte_received_i;
					if rs_rx_frame_i = '0' then
						rx_state <= RX_WAIT_START_FRAME_DELIMITER;
						-- Remaining FCS after parsing whole packet + FCS needs to be a specific value
						-- TODO Activate frame size check again!!
						if rs_rx_error_i = '1' or rx_frame_check_sequence /= CRC32_POSTINVERT_MAGIC or rx_frame_size < MIN_FRAME_DATA_BYTES + CRC32_BYTES or rx_frame_size > MAX_FRAME_DATA_BYTES + CRC32_BYTES then
							rx_error_o <= '1';
						--rx_next_state <= RX_ERROR;
						end if;
					else
						if rs_rx_byte_received_i = '1' then
							-- Update FCS check
							rx_frame_check_sequence <= update_crc32(rx_frame_check_sequence, rs_rx_data_i);
							-- Increase frame size
							if rx_frame_size < rx_frame_size_t'high then
								rx_frame_size <= rx_frame_size + 1;
							end if;
						end if;
						if rs_rx_error_i = '1' then
							-- Skip the rest of the frame and tell the higher layer
							rx_state <= RX_ERROR;
						end if;
					end if;
				when RX_SKIP_FRAME =>
					-- Skip the currently receiving frame without signaling the higher layer
					if rs_rx_frame_i = '0' then
						rx_state <= RX_WAIT_START_FRAME_DELIMITER;
					end if;
				when RX_ERROR =>
					-- Skip the currently receiving frame and signal the higher layer
					rx_frame_o <= '1';
					rx_error_o <= '1';
					if rs_rx_frame_i = '0' then
						rx_state <= RX_WAIT_START_FRAME_DELIMITER;
					end if;
			end case;
		end if;
	end process;

end architecture;
