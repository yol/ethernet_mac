library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ethernet_mac;
use ethernet_mac.ethernet_types.all;
use ethernet_mac.fifo_types.all;
use ethernet_mac.crc32.all;

entity rx_fifo is
	port(
		clock_i                : in  std_ulogic;
		reset_i                : in  std_ulogic;

		mac_rx_clock_i         : in  std_ulogic;
		mac_rx_frame_i         : in  std_ulogic;
		mac_rx_data_i          : in  ethernet_data_t;
		mac_rx_byte_received_i : in  std_ulogic;
		mac_rx_error_i         : in  std_ulogic;

		rx_empty_o             : out std_ulogic;
		rx_rd_en_i             : in  std_ulogic;
		rx_data_o              : out ethernet_data_t
	);
end entity;

architecture rtl of rx_fifo is
	type write_state_t is (
		WRITE_WAIT,
		WRITE_PACKET,
		WRITE_LENGTH_HIGH,
		WRITE_LENGTH_LOW,
		WRITE_PACKET_INVALID,
		WRITE_SKIP_FRAME
	);
	signal write_state : write_state_t := WRITE_PACKET_INVALID;

	type read_state_t is (
		READ_WAIT_PACKET,
		READ_WAIT_ACK,
		READ_LENGTH_LOW,
		READ_PACKET
	);
	signal read_state : read_state_t := READ_WAIT_PACKET;

	constant PACKET_LENGTH_BYTES : positive := 2;

	type memory_t is array (0 to (RX_MEMORY_SIZE - 1)) of ethernet_data_t;

	-- Use unsigned here instead of simple integer variables to guarantee identical behavior in behavioral and post-translate simulation
	-- Integer variables will wrap around only on reaching 2^32 in behavioral simulation, unsigned will always wrap around correctly.
	subtype memory_address_t is unsigned((RX_MEMORY_SIZE_BITS - 1) downto 0);
	-- Frame header is two bytes and one bit is used to indicate frame validity, so 15 bits are left for packet length.
	subtype packet_length_t is unsigned(14 downto 0);

	signal memory              : memory_t;
	signal write_start_address : memory_address_t := (others => '0');
	signal write_address       : memory_address_t := (others => '0');
	--signal write_data          : ethernet_data_t := (others => '0');

	--signal read_address       : memory_address_t      := 0;
	signal read_data : ethernet_data_t := (others => '0');
	--signal read_packet_length : unsigned(14 downto 0) := (others => '0');

	-- Counts the elements in [a, b) in a ring buffer of given size
	-- a and b should have the same range
	function pointer_difference(a : unsigned; b : unsigned; size : positive) return unsigned is
		-- Make sure the result has a matching range
		variable result : unsigned(a'range);
	begin
		if b >= a then
			result := b - a;
		else
			result := size - a + b;
		end if;
		return result;
	end function;

	-- Address of the last byte that is still part of the currently processed frame in the read process
	signal read_end_address : memory_address_t := (others => '0');

begin
	write_memory : process(reset_i, mac_rx_clock_i)
		variable write_address_now : memory_address_t := (others => '0');
		variable write_data        : ethernet_data_t  := (others => '0');
		variable write_enable      : boolean          := FALSE;
		variable packet_length     : packet_length_t  := (others => '0');
	begin
		if reset_i = '1' then
			write_state         <= WRITE_PACKET_INVALID;
			write_start_address <= (others => '0');
			write_address       <= (others => '0');
		elsif rising_edge(mac_rx_clock_i) then
			write_address_now := write_address;
			write_enable      := FALSE;

			case write_state is
				when WRITE_WAIT =>
					if mac_rx_frame_i = '1' then
						if mac_rx_error_i = '1' then
							write_state <= WRITE_SKIP_FRAME;
						else
							if mac_rx_byte_received_i = '1' then
								-- Write first byte
								-- Leave 2 bytes room for data valid indication and packet length
								write_address_now := write_start_address + PACKET_LENGTH_BYTES;
								write_data        := mac_rx_data_i;
								write_enable      := TRUE;
								write_address     <= write_address_now + 1;

								write_state <= WRITE_PACKET;
							end if;
						end if;
					end if;
				when WRITE_PACKET =>
					if mac_rx_error_i = '1' then
						write_state <= WRITE_SKIP_FRAME;
					end if;
					-- Calculate packet length
					-- Resize as packet_length'length can be greater than memory_address_t'length for small memories
					packet_length := resize(pointer_difference(write_start_address, write_address, RX_MEMORY_SIZE), packet_length'length);

					if mac_rx_frame_i = '1' then
						if mac_rx_byte_received_i = '1' then
							-- Write data
							write_data    := mac_rx_data_i;
							write_enable  := TRUE;
							write_address <= write_address + 1;
						end if;

						-- Packet length will overflow after this clock cycle
						if packet_length = (packet_length'range => '1') then
							-- Throw frame away
							write_state <= WRITE_SKIP_FRAME;
						end if;
					-- Error flag is irrelevant if rx_frame_i is low already
					else
						if packet_length <= (CRC32_BYTES + PACKET_LENGTH_BYTES) then
							-- Frame is way too short, ignore
							write_state <= WRITE_WAIT;
						end if;
						packet_length := packet_length - CRC32_BYTES - PACKET_LENGTH_BYTES;

						-- Write next packet invalid before doing anything else
						-- Read process could read past this packet faster than we can flag the following packet invalid otherwise
						-- for very low network speed/system clock speed ratios
						write_address_now := write_start_address + PACKET_LENGTH_BYTES + to_integer(packet_length);
						write_data        := (others => '0');
						write_enable      := TRUE;

						-- Write data length low byte next
						write_state <= WRITE_LENGTH_LOW;
					end if;
				when WRITE_LENGTH_LOW =>
					-- Write length low byte
					write_address_now := write_start_address + 1;
					write_data        := std_ulogic_vector(packet_length(7 downto 0));
					write_enable      := TRUE;

					-- Write high byte next
					write_state <= WRITE_LENGTH_HIGH;
				when WRITE_LENGTH_HIGH =>
					-- Write length high byte and packet valid flag
					write_address_now := write_start_address;
					write_data        := "1" -- mark valid
						& std_ulogic_vector(packet_length(14 downto 8));
					write_enable        := TRUE;
					-- Packet received correctly and completely, move start pointer for next packet ->
					-- Move write pointer past the data of the current frame (excluding FCS) so the next
					-- packet can be written
					-- The FCS will get overwritten as it is not needed for operation of the higher layers.
					write_start_address <= write_start_address + PACKET_LENGTH_BYTES + to_integer(packet_length);
					write_state         <= WRITE_WAIT;
				when WRITE_PACKET_INVALID =>
					-- Mark current/first packet as invalid
					-- This cannot be merged into the WRITE_WAIT state, as WRITE_WAIT needs to write an incoming data byte
					-- into the buffer immediately.
					write_address_now := write_start_address;
					write_data        := (others => '0');
					write_enable      := TRUE;
					write_state       <= WRITE_WAIT;
				when WRITE_SKIP_FRAME =>
					if mac_rx_frame_i = '0' then
						write_state <= WRITE_WAIT;
					end if;
			end case;

			if write_enable then
				memory(to_integer(write_address_now)) <= write_data;
			end if;
		end if;
	end process;

	read_memory_data_out : process(read_data, read_state)
	begin
		rx_data_o <= read_data;
		if read_state = READ_WAIT_PACKET or read_state = READ_WAIT_ACK then
			-- Mask validity bit
			rx_data_o <= "0" & read_data(6 downto 0);
		end if;
	end process;

	read_memory : process(clock_i)
		variable read_address       : memory_address_t      := (others => '0');
		variable read_packet_length : unsigned(14 downto 0) := (others => '0');
	begin
		if rising_edge(clock_i) then
			-- Default output value
			rx_empty_o <= '1';

			if reset_i = '1' then
				read_state         <= READ_WAIT_PACKET;
				read_address       := (others => '0');
				read_end_address   <= (others => '0');
				read_data          <= (others => '0');
				read_packet_length := (others => '0');
			else
				case read_state is
				when READ_WAIT_PACKET =>
					-- TODO!! GEHT NICHT SO EINFACH!!
					-- Laut User Guide darf man nicht dieselbe Adresse schreiben und lesen gleichzeitig
					
						-- Wait until data is ready and nobody is trying to get data out (in case the previous state was READ_PACKET)
						-- to prevent read overruns
						if read_data(7) = '1' then -- and rx_rd_en_i = '0' then
							-- Tell the receiver that something is here
							rx_empty_o <= '0';
							read_state <= READ_WAIT_ACK;
						end if;
					when READ_WAIT_ACK =>
						rx_empty_o <= '0';
						if rx_rd_en_i = '1' then
							-- Read address high byte
							read_address                    := read_address + 1;
							read_state                      <= READ_LENGTH_LOW;
							read_packet_length(14 downto 8) := unsigned(read_data(6 downto 0));
						end if;
					when READ_LENGTH_LOW =>
						rx_empty_o <= '0';
						if rx_rd_en_i = '1' then
							-- Read address low byte
							read_address                   := read_address + 1;
							read_packet_length(7 downto 0) := unsigned(read_data);
							read_state                     <= READ_PACKET;
							-- The end address is the address of the last byte that is still valid, so one byte
							-- needs to be subtracted.
							read_end_address               <= read_address + to_integer(read_packet_length) - 1;
						end if;
					when READ_PACKET =>
						rx_empty_o <= '0';

						if read_address = read_end_address then
							-- Wait for the last byte to be read out
							if rx_rd_en_i = '1' then
								-- Guarantee that rx_empty_o is high for at least one clock cycle (needed so that the 
								-- receiver can sense the end of the packet)
								rx_empty_o <= '1';
								read_state <= READ_WAIT_PACKET;
							end if;
						end if;

						if rx_rd_en_i = '1' then
							-- If this was the last byte: go to the first byte following this packet (-> header of the next packet)
							-- No need to skip the FCS here, this is already taken care of in the write process.
							read_address := read_address + 1;
						end if;
				end case;

				read_data <= memory(to_integer(read_address));
			end if;
		end if;
	end process;

end architecture;
