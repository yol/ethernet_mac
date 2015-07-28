-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity clock_output is
	port(
		clock_i     : in  std_ulogic;
		clock_inv_i : in  std_ulogic;
		output_o    : out std_ulogic
	);
end entity clock_output;
