-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

library ieee;
use ieee.std_logic_1164.all;

library ethernet_mac;
use ethernet_mac.mii_types.all;
use ethernet_mac.ethernet_types.all;

entity mii_gmii_io is
	port(
		clock_125_i            : in  std_ulogic;
		clock_125_inv_i        : in  std_ulogic;
		clock_125_unbuffered_i : in  std_ulogic;

		clock_tx_o             : out std_ulogic;
		clock_rx_o             : out std_ulogic;

		speed_select_i         : in  ethernet_speed_t;

		-- Signals connected directly to external ports
		-- MII
		mii_tx_clk_i           : in  std_ulogic;
		mii_tx_er_o            : out std_ulogic;
		mii_tx_en_o            : out std_ulogic;
		mii_txd_o              : out ethernet_data_t;
		mii_rx_clk_i           : in  std_ulogic;
		mii_rx_er_i            : in  std_ulogic;
		mii_rx_dv_i            : in  std_ulogic;
		mii_rxd_i              : in  ethernet_data_t;

		-- GMII
		gmii_gtx_clk_o         : out std_ulogic;

		-- Signals connected to the MII module
		-- MII
		int_mii_tx_er_i        : in  std_ulogic;
		int_mii_tx_en_i        : in  std_ulogic;
		int_mii_txd_i          : in  ethernet_data_t;
		int_mii_rx_er_o        : out std_ulogic;
		int_mii_rx_dv_o        : out std_ulogic;
		int_mii_rxd_o          : out ethernet_data_t

	-- GMII
	--int_gmii_gtx_clk_i     : in  std_ulogic;
	--int_gmii_gtx_clk_inv_i : in  std_ulogic
	);
end entity;

