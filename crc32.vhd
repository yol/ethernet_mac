library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package crc32 is

	-- As defined in IEEE 802.3 clause 3.2.9
	-- x^32 + x^26 + x^23 + x^22 + x^16 + x^12 + x^11 + x^10 + x^8 + x^7 + x^5 + x^4 + x^2 + x + 1
	constant CRC32_POLYNOMIAL : std_ulogic_vector(32 downto 0) := (
		32 | 26 | 23 | 22 | 16 | 12 | 11 | 10 | 8 | 7 | 5 | 4 | 2 | 1 | 0 => '1',
		others => '0'
	);

	subtype crc32_result_t is std_ulogic_vector(31 downto 0);

	-- Value that remains as CRC value when incoming data including the original FCS is piped through update_crc32 
	-- and the FCS is correct.
	-- Usually this would be zero, but the inversion of the FCS in clause 3.2.9 e changes it to this magic value.
	constant CRC32_POSTINVERT_MAGIC : crc32_result_t := X"C704DD7B";

	function update_crc32(old_crc : crc32_result_t; input : std_ulogic) return crc32_result_t;
	function update_crc32(old_crc : crc32_result_t; input : std_ulogic_vector) return crc32_result_t;

	constant CRC32_BYTES : positive := (crc32_result_t'length / 8);
end package;

package body crc32 is
	function update_crc32(old_crc : crc32_result_t; input : std_ulogic) return crc32_result_t is
		variable new_crc  : crc32_result_t;
		variable feedback : std_ulogic;
	begin
		new_crc  := old_crc;
		feedback := new_crc(new_crc'high) xor input;
		new_crc  := crc32_result_t(unsigned(new_crc) sll 1);
		if (feedback = '1') then
			new_crc := new_crc xor CRC32_POLYNOMIAL(31 downto 0);
		end if;

		return new_crc;
	end function;

	function update_crc32(old_crc : crc32_result_t; input : std_ulogic_vector) return crc32_result_t is
		variable new_crc : crc32_result_t;
	begin
		new_crc := old_crc;

		-- Start with LSB
		for i in input'low to input'high loop
			new_crc := update_crc32(new_crc, input(i));
		end loop;

		return new_crc;
	end function;

end package body;
