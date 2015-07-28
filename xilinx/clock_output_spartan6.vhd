-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

library ieee;
use ieee.std_logic_1164.all;

library unisim;
use unisim.vcomponents.all;

architecture spartan_6 of clock_output is
begin

	-- ODDR2: Output Double Data Rate Output Register with Set, Reset
	--        and Clock Enable. 
	--        Spartan-6
	-- Xilinx HDL Language Template, version 14.7

	ODDR2_inst : ODDR2
		generic map(
			DDR_ALIGNMENT => "NONE",    -- Sets output alignment to "NONE", "C0", "C1" 
			INIT          => '0',       -- Sets initial state of the Q output to '0' or '1'
			SRTYPE        => "SYNC")    -- Specifies "SYNC" or "ASYNC" set/reset
		port map(
			Q  => output_o,             -- 1-bit output data
			C0 => clock_i,              -- 1-bit clock input
			C1 => clock_inv_i,          -- 1-bit clock input
			CE => '1',                  -- 1-bit clock enable input
			D0 => '1',                  -- 1-bit data input (associated with C0)
			D1 => '0',                  -- 1-bit data input (associated with C1)
			R  => '0',                  -- 1-bit reset input
			S  => '0'                   -- 1-bit set input
		);

end architecture;
