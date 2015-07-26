library ieee;
use ieee.std_logic_1164.all;

package fifo_types is
	constant RX_MEMORY_SIZE_BITS : positive := 12;
	-- This needs to be a power of 2 for the rx memory code to work correctly!
	constant RX_MEMORY_SIZE      : positive := 2 ** RX_MEMORY_SIZE_BITS;

	subtype tx_size_fifo_data_t is std_ulogic_vector(15 downto 0);
	constant TX_PACKET_SIZE_BITS : positive := 12;
	constant TX_MAX_PACKET_SIZE : positive := ((2 ** TX_PACKET_SIZE_BITS) - 1);
end package;