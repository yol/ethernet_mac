-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

library ieee;
use ieee.std_logic_1164.all;

architecture simple of single_signal_synchronizer is
	signal signal_tmp : std_ulogic := '0';
begin
	process(clock_target_i)
	begin
		if rising_edge(clock_target_i) then
			signal_tmp <= signal_i;
			signal_o   <= signal_tmp;
		end if;
	end process;
end architecture;
