-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package mii_types is
	constant INTER_PACKET_GAP : positive := 12; -- bytes
	--constant MAX_ENVELOPE_FRAME_SIZE : positive := 2000; -- bytes

	constant PACKET_LENGTH_BITS : positive := 11;
	constant MAX_PACKET_LENGTH  : positive := (2 ** PACKET_LENGTH_BITS) - 1;
	subtype packet_length_t is unsigned((PACKET_LENGTH_BITS - 1) downto 0);
end package;
