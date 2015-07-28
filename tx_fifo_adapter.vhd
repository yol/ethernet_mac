-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ethernet_mac;
use ethernet_mac.ethernet_types.all;
use ethernet_mac.fifo_types.all;

entity tx_fifo_adapter is
	port(
		reset_i            : in  std_ulogic;

		-- Interface to framing layer
		mac_tx_clock_i     : in  std_ulogic;
		mac_tx_enable_o    : out std_ulogic;
		mac_tx_data_o      : out ethernet_data_t;
		mac_tx_byte_sent_i : in  std_ulogic;
		mac_tx_busy_i      : in  std_ulogic;

		-- Data FIFO interface
		data_rd_en_o       : out std_ulogic;
		data_i             : in  ethernet_data_t;
		data_empty_i       : in  std_ulogic;
		data_read_count_i  : in  unsigned

	-- Size FIFO interface
	--		size_rd_en_o       : out std_ulogic;
	--		size_i             : in  tx_size_fifo_data_t;
	--		size_empty_i       : in  std_ulogic
	);
end entity;

architecture rtl of tx_fifo_adapter is
	type state_t is (
		READ_SIZE_HIGH,
		WAIT_READ_SIZE_LOW,
		READ_SIZE_LOW,
		WAIT_DATA_COUNT1,
		WAIT_DATA_COUNT2,
		WAIT_PACKET,
		WAIT_DATA_READ,
		READ_DATA,
		SEND_DATA
	);
	signal state                 : state_t         := READ_SIZE_HIGH;
	signal remaining_packet_size : unsigned(TX_PACKET_SIZE_BITS - 1 downto 0);
	signal next_data             : ethernet_data_t := (others => '0');
	signal data_rd_en            : std_ulogic      := '0';

begin
	data_rd_en_o <= data_rd_en;

	send_proc : process(reset_i, mac_tx_clock_i)
	--		variable first : boolean := TRUE;
	begin
		if reset_i = '1' then
			state           <= READ_SIZE_HIGH;
			data_rd_en      <= '0';
			mac_tx_enable_o <= '0';
		elsif rising_edge(mac_tx_clock_i) then
			data_rd_en      <= '0';
			mac_tx_enable_o <= '0';

			case state is
				when READ_SIZE_HIGH =>
					-- Wait for FIFO nonempty
					if data_empty_i = '0' then --and (data_read_count_i > 20 or first = FALSE) then
						-- Read packet size high byte
						remaining_packet_size(TX_PACKET_SIZE_BITS - 1 downto 8) <= unsigned(data_i(TX_PACKET_SIZE_BITS - 1 - 8 downto 0));
						-- Move FIFO to next byte
						data_rd_en                                              <= '1';
						state                                                   <= WAIT_READ_SIZE_LOW;
					end if;
				when WAIT_READ_SIZE_LOW =>
					-- Wait for EMPTY flag and data to update
					state <= READ_SIZE_LOW;
				when READ_SIZE_LOW =>
					-- Wait for FIFO nonempty
					if data_empty_i = '0' then
						-- Read packet size low byte
						remaining_packet_size(7 downto 0) <= unsigned(data_i);
						-- Move FIFO to next byte
						data_rd_en                        <= '1';
						state                             <= WAIT_DATA_COUNT1;
					end if;
				when WAIT_DATA_COUNT1 =>
					-- The FIFO read data count can over-report for up to two clock cycles according
					-- to XILINX documentation. Make sure this doesn't happen here.
					state <= WAIT_DATA_COUNT2;
				when WAIT_DATA_COUNT2 =>
					state <= WAIT_PACKET;
				when WAIT_PACKET =>
					-- Check for obviously wrong frame size to avoid lock-up
					if remaining_packet_size = 0 then
						-- Try again
						state <= READ_SIZE_HIGH;
					else
						-- Wait for all data available and TX idle
						if data_read_count_i >= remaining_packet_size and mac_tx_busy_i = '0' then
							-- Remember the first byte
							mac_tx_data_o   <= data_i;
							-- Start transmission already, delay through framing is long enough to not miss the first tx_byte_sent
							mac_tx_enable_o <= '1';
							-- Move FIFO on to the second byte					
							data_rd_en      <= '1';
							state           <= WAIT_DATA_READ;
						end if;
					end if;
				when WAIT_DATA_READ =>
					-- Third byte
					data_rd_en      <= '1';
					state           <= READ_DATA;
					mac_tx_enable_o <= '1';
				when READ_DATA =>
					next_data       <= data_i;
					state           <= SEND_DATA;
					mac_tx_enable_o <= '1';
				when SEND_DATA =>
					--next_data <= data_i;
					mac_tx_enable_o <= '1';
					--mac_tx_data_o <= next_data;
					if mac_tx_byte_sent_i = '1' then
						if remaining_packet_size = 1 then
							-- This was the last byte
							mac_tx_enable_o <= '0';
							state           <= READ_SIZE_HIGH;
						--first           := FALSE;
						else
							if data_rd_en = '1' then
								-- The buffer is exhausted if we've supplied its value
								-- in the previous clock cycle, now supply data directly from the FIFO
								mac_tx_data_o <= data_i;
							--next_data <= data
							else
								-- Pass the buffered byte on
								mac_tx_data_o <= next_data;
								next_data     <= data_i;
							end if;
							-- Get one byte out of FIFO
							if remaining_packet_size >= 3 then
								data_rd_en <= '1';
							end if;
							remaining_packet_size <= remaining_packet_size - 1;
						end if;
					end if;
			end case;
		end if;
	end process;

end architecture;
