-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

library ieee;
use ieee.std_logic_1164.all;

package ethernet_types is
	subtype ethernet_data_t is std_ulogic_vector(7 downto 0);
	subtype ethernet_speed_t is std_ulogic_vector(1 downto 0);

	constant SPEED_1000MBPS : ethernet_speed_t := "10";
	constant SPEED_100MBPS  : ethernet_speed_t := "01";
	constant SPEED_10MBPS   : ethernet_speed_t := "00";
end package;