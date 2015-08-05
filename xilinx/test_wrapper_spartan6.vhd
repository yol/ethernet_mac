-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.ethernet_types.all;
use work.miim_types.all;

entity test_wrapper_spartan6 is
	generic(
		MIIM_PHY_ADDRESS      : phy_address_t      := "00000";
		MIIM_RESET_WAIT_TICKS : natural            := 0;
		MIIM_POLL_WAIT_TICKS  : natural            := 0;
		MIIM_SPEED_REGISTER   : register_address_t := "00000";
		MIIM_SPEED_HIGH_BIT   : integer            := 0;
		MIIM_SPEED_LOW_BIT    : integer            := 0
	);
	port(
		clock_125_i            : in    std_ulogic;
		clock_125_inv_i        : in    std_ulogic;
		clock_125_unbuffered_i : in    std_ulogic;
		reset_i                : in    std_ulogic;

		mii_tx_clk_i           : in    std_ulogic;
		mii_tx_er_o            : out   std_ulogic;
		mii_tx_en_o            : out   std_ulogic;
		mii_txd_o              : out   std_ulogic_vector(7 downto 0);
		mii_rx_clk_i           : in    std_ulogic;
		mii_rx_er_i            : in    std_ulogic;
		mii_rx_dv_i            : in    std_ulogic;
		mii_rxd_i              : in    std_ulogic_vector(7 downto 0);

		gmii_gtx_clk_o         : out   std_ulogic;

		rgmii_tx_ctl_o         : out   std_ulogic;
		rgmii_rx_ctl_i         : in    std_ulogic;

		mdc_o                  : out   std_ulogic;
		mdio_io                : inout std_ulogic;

		rx_clock_i             : in    std_ulogic;
		rx_empty_o             : out   std_ulogic;
		rx_rd_en_i             : in    std_ulogic;
		rx_data_o              : out   ethernet_data_t;

		tx_clock_i             : in    std_ulogic;
		tx_data_i              : in    ethernet_data_t;
		tx_data_wr_en_i        : in    std_ulogic;
		tx_data_full_o         : out   std_ulogic;

		link_up_o              : out   std_ulogic;
		speed_o                : out   ethernet_speed_t;
		speed_override_i       : in    ethernet_speed_t := SPEED_UNSPECIFIED
	);
end entity;

architecture behavioral of test_wrapper_spartan6 is
	signal mii_txd : std_logic_vector(7 downto 0);
	signal rx_data : std_logic_vector(7 downto 0);
	signal speed   : std_logic_vector(1 downto 0);
begin
	mii_txd_o <= std_ulogic_vector(mii_txd);
	rx_data_o <= std_ulogic_vector(rx_data);
	speed_o   <= ethernet_speed_t(speed);

	test_instance_inst : entity work.test_instance_spartan6
		port map(
			clock_125_i      => clock_125_i,
			reset_i          => reset_i,
			mii_tx_clk_i     => mii_tx_clk_i,
			mii_tx_er_o      => mii_tx_er_o,
			mii_tx_en_o      => mii_tx_en_o,
			mii_txd_o        => mii_txd,
			mii_rx_clk_i     => mii_rx_clk_i,
			mii_rx_er_i      => mii_rx_er_i,
			mii_rx_dv_i      => mii_rx_dv_i,
			mii_rxd_i        => std_logic_vector(mii_rxd_i),
			gmii_gtx_clk_o   => gmii_gtx_clk_o,
			rgmii_tx_ctl_o   => rgmii_tx_ctl_o,
			rgmii_rx_ctl_i   => rgmii_rx_ctl_i,
			mdc_o            => mdc_o,
			mdio_io          => mdio_io,
			rx_clock_i       => rx_clock_i,
			rx_empty_o       => rx_empty_o,
			rx_rd_en_i       => rx_rd_en_i,
			rx_data_o        => rx_data,
			tx_clock_i       => tx_clock_i,
			tx_data_i        => std_logic_vector(tx_data_i),
			tx_data_wr_en_i  => tx_data_wr_en_i,
			tx_data_full_o   => tx_data_full_o,
			link_up_o        => link_up_o,
			speed_o          => speed,
			speed_override_i => std_logic_vector(speed_override_i)
		);

end architecture;

