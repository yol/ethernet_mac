-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

library ieee;
use ieee.std_logic_1164.all;

-- Synchronize a single bit from an arbitrary clock domain
-- into the clock_target domain
-- Uses two flip-flops
entity single_signal_synchronizer is
	port(
		clock_target_i : in  std_ulogic;
		signal_i       : in  std_ulogic;
		signal_o       : out std_ulogic
	);
end entity;