-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

library ieee;
use ieee.std_logic_1164.all;

library unisim;
use unisim.vcomponents.all;

architecture spartan6 of single_signal_synchronizer is
	signal signal_tmp : std_ulogic := '0';
	
	-- Constrain registers
	attribute ASYNC_REG : string;
	attribute ASYNC_REG of FDRE_tmp_inst : label is "TRUE";
	attribute ASYNC_REG of FDRE_out_inst : label is "TRUE";
	-- Do not allow conversion into a shift register
	attribute shreg_extract : string;
	attribute shreg_extract of signal_tmp : signal is "no";
begin
	FDRE_tmp_inst : FDRE
		generic map(
			INIT => '0')                -- Initial value of register ('0' or '1')  
		port map(
			Q  => signal_tmp,           -- Data output
			C  => clock_target_i,       -- Clock input
			CE => '1',                  -- Clock enable input
			R  => '0',                  -- Synchronous reset input
			D  => signal_i              -- Data input
		);

	FDRE_out_inst : FDRE
		generic map(
			INIT => '0')                -- Initial value of register ('0' or '1')  
		port map(
			Q  => signal_o,             -- Data output
			C  => clock_target_i,       -- Clock input
			CE => '1',                  -- Clock enable input
			R  => '0',                  -- Synchronous reset input
			D  => signal_tmp            -- Data input
		);

end architecture;