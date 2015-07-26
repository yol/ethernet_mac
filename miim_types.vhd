library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package miim_types is
	subtype register_address_t is unsigned(4 downto 0);
	subtype phy_address_t is unsigned(4 downto 0);
	subtype data_t is std_ulogic_vector(15 downto 0);

	function to_register_address(c : natural) return register_address_t;

	constant DEFAULT_POLL_WAIT_TICKS : natural := 10000000;

	constant CONTROL_REG                    : register_address_t := to_register_address(0);
	constant STATUS_REG                     : register_address_t := to_register_address(1);
	constant PHY_ID1_REG                    : register_address_t := to_register_address(2);
	constant PHY_ID2_REG                    : register_address_t := to_register_address(3);
	constant AUTONEG_ADVERTISEMENT_REG      : register_address_t := to_register_address(4);
	constant AUTONEG_LP_BASEPAGEABILITY_REG : register_address_t := to_register_address(5);
	constant AUTONEG_EXPANSION_REG          : register_address_t := to_register_address(6);
	constant AUTONEG_NEXTPAGETX_REG         : register_address_t := to_register_address(7);
	constant AUTONEG_LP_NEXTPAGERECV_REG    : register_address_t := to_register_address(8);
	constant MASTERSLAVE_CTRL_REG           : register_address_t := to_register_address(9);
	constant MASTERSLAVE_STATUS_REG         : register_address_t := to_register_address(10);
	constant PSE_CONTROL_REG                : register_address_t := to_register_address(11);
	constant PSE_STATUS_REG                 : register_address_t := to_register_address(12);
	constant MMD_ACCESSCONTROL_REG          : register_address_t := to_register_address(13);
	constant MMD_ACCESSADDRESSDATA_REG      : register_address_t := to_register_address(14);
	constant EXTENDED_STATUS_REG            : register_address_t := to_register_address(15);
	constant VENDOR_SPECIFIC_REG_BASE       : register_address_t := to_register_address(16);
end package;

package body miim_types is
	function to_register_address(c : natural) return register_address_t is
	begin
		return to_unsigned(c, register_address_t'length);
	end function;
end package body;