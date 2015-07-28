-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

library ieee;
use ieee.std_logic_1164.all;

library ethernet_mac;
use ethernet_mac.ethernet_types.all;
use ethernet_mac.miim_types.all;

package miim_control_types is
	subtype control_register_speed_t is std_ulogic_vector(1 downto 0);
	constant SPEED_1000MBPS : control_register_speed_t := "10";
	constant SPEED_100MBPS  : control_register_speed_t := "01";
	constant SPEED_10MBPS   : control_register_speed_t := "00";

	type control_register_t is record
		reset                    : std_ulogic;
		loopback                 : std_ulogic;
		speed                    : control_register_speed_t;
		auto_negotiation_enable  : std_ulogic;
		power_down               : std_ulogic;
		isolate                  : std_ulogic;
		restart_auto_negotiation : std_ulogic;
		duplex_mode              : std_ulogic;
		enable_collision_test    : std_ulogic;
		unidirectional_enable    : std_ulogic;
	end record;

	type status_register_t is record
		can_100base_t4            : std_ulogic;
		can_100base_x_fd          : std_ulogic;
		can_100base_x_hd          : std_ulogic;
		can_10mbps_fd             : std_ulogic;
		can_10mbps_hd             : std_ulogic;
		can_100base_t2_fd         : std_ulogic;
		can_100base_t2_hd         : std_ulogic;
		extended_status           : std_ulogic;
		undirectional_ability     : std_ulogic;
		mf_preamble_suppression   : std_ulogic;
		auto_negotiation_complete : std_ulogic;
		remote_fault              : std_ulogic;
		auto_negotiation_ability  : std_ulogic;
		link_status               : std_ulogic;
		jabber_detect             : std_ulogic;
		extended_capability       : std_ulogic;
	end record;

	type auto_negotiation_advertisement_register_802_3_t is record
		next_page               : std_ulogic;
		remote_fault            : std_ulogic;
		extended_next_page      : std_ulogic;
		asymmetric_pause        : std_ulogic;
		pause                   : std_ulogic;
		advertise_100base_t4    : std_ulogic;
		advertise_100base_tx_fd : std_ulogic;
		advertise_100base_tx_hd : std_ulogic;
		advertise_10base_t_fd   : std_ulogic;
		advertise_10base_t_hd   : std_ulogic;
	end record;

	type master_slave_control_register_t is record
		test_mode_bits                    : std_ulogic_vector(2 downto 0);
		master_slave_manual_config_enable : std_ulogic;
		master_slave_manual_config_value  : std_ulogic;
		port_type_is_multiport            : std_ulogic;
		advertise_1000base_t_fd           : std_ulogic;
		advertise_1000base_t_hd           : std_ulogic;
	end record;

	constant AUTO_NEGOTATION_802_3_SELECTOR : std_ulogic_vector(4 downto 0) := "00001";

	function control_register_speed_to_ethernet_speed(speed : in control_register_speed_t) return ethernet_speed_t;

	function control_register_to_data(reg : in control_register_t) return data_t;
	function auto_negotiation_advertisement_register_802_3_to_data(reg : in auto_negotiation_advertisement_register_802_3_t) return data_t;
	function master_slave_control_register_to_data(reg : in master_slave_control_register_t) return data_t;

	function data_to_status_register(data : in data_t) return status_register_t;

end package;

package body miim_control_types is
	function control_register_to_data(reg : in control_register_t) return data_t is
		variable data : data_t;
	begin
		data := (
				15     => reg.reset,
				14     => reg.loopback,
				13     => reg.speed(0),
				12     => reg.auto_negotiation_enable,
				11     => reg.power_down,
				10     => reg.isolate,
				9      => reg.restart_auto_negotiation,
				8      => reg.duplex_mode,
				7      => reg.enable_collision_test,
				6      => reg.speed(1),
				5      => reg.unidirectional_enable,
				others => '0'
			);
		return data;
	end function;

	function auto_negotiation_advertisement_register_802_3_to_data(reg : in auto_negotiation_advertisement_register_802_3_t) return data_t is
		variable data : data_t;
	begin
		data := (
				15     => reg.next_page,
				13     => reg.remote_fault,
				12     => reg.extended_next_page,
				11     => reg.asymmetric_pause,
				10     => reg.pause,
				9      => reg.advertise_100base_t4,
				8      => reg.advertise_100base_tx_fd,
				7      => reg.advertise_100base_tx_hd,
				6      => reg.advertise_10base_t_fd,
				5      => reg.advertise_10base_t_hd,
				others => '0'
			);
		data(4 downto 0) := AUTO_NEGOTATION_802_3_SELECTOR;
		return data;
	end function;

	function master_slave_control_register_to_data(reg : in master_slave_control_register_t) return data_t is
		variable data : data_t;
	begin
		data := (
				12     => reg.master_slave_manual_config_enable,
				11     => reg.master_slave_manual_config_value,
				10     => reg.port_type_is_multiport,
				9      => reg.advertise_1000base_t_fd,
				8      => reg.advertise_1000base_t_hd,
				others => '0'
			);
		data(15 downto 13) := reg.test_mode_bits;
		return data;
	end function;

	function data_to_status_register(data : in data_t) return status_register_t is
		variable status : status_register_t;
	begin
		status := (
				can_100base_t4            => data(15),
				can_100base_x_fd          => data(14),
				can_100base_x_hd          => data(13),
				can_10mbps_fd             => data(12),
				can_10mbps_hd             => data(11),
				can_100base_t2_fd         => data(10),
				can_100base_t2_hd         => data(9),
				extended_status           => data(8),
				undirectional_ability     => data(7),
				mf_preamble_suppression   => data(6),
				auto_negotiation_complete => data(5),
				remote_fault              => data(4),
				auto_negotiation_ability  => data(3),
				link_status               => data(2),
				jabber_detect             => data(1),
				extended_capability       => data(0)
			);
		return status;
	end function;

	function control_register_speed_to_ethernet_speed(speed : in control_register_speed_t) return ethernet_speed_t is
	begin
		-- Conversion is 1:1 at the moment
		return speed;
	end function;

end package body;
