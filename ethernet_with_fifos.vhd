-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

library ieee;
use ieee.std_logic_1164.all;

library ethernet_mac;
use ethernet_mac.ethernet_types.all;
use ethernet_mac.fifo_types.all;
use ethernet_mac.miim_types.all;

entity ethernet_with_fifos is
	generic(
		MIIM_PHY_ADDRESS      : phy_address_t := (others => '0');
		MIIM_RESET_WAIT_TICKS : natural       := 0;
		MIIM_POLL_WAIT_TICKS  : natural       := DEFAULT_POLL_WAIT_TICKS;
		MIIM_SPEED_REGISTER   : register_address_t;
		MIIM_SPEED_HIGH_BIT   : integer range data_t'range;
		MIIM_SPEED_LOW_BIT    : integer range data_t'range
	);
	port(
		clock_125_i            : in    std_ulogic;
		clock_125_inv_i        : in    std_ulogic;
		clock_125_unbuffered_i : in    std_ulogic;
		reset_i                : in    std_ulogic;

		-- MII (Media-independent interface)
		mii_tx_clk_i           : in    std_ulogic;
		mii_tx_er_o            : out   std_ulogic;
		mii_tx_en_o            : out   std_ulogic;
		mii_txd_o              : out   std_ulogic_vector(7 downto 0);
		mii_rx_clk_i           : in    std_ulogic;
		mii_rx_er_i            : in    std_ulogic;
		mii_rx_dv_i            : in    std_ulogic;
		mii_rxd_i              : in    std_ulogic_vector(7 downto 0);

		-- GMII (Gigabit media-independent interface)
		gmii_gtx_clk_o         : out   std_ulogic;

		-- RGMII (Reduced pin count gigabit media-independent interface)
		rgmii_tx_ctl_o         : out   std_ulogic;
		rgmii_rx_ctl_i         : in    std_ulogic;

		-- MII Management Interface
		mdc_o                  : out   std_ulogic;
		mdio_io                : inout std_ulogic;

		-- RX FIFO
		rx_clock_i             : in    std_ulogic;
		rx_empty_o             : out   std_ulogic;
		rx_rd_en_i             : in    std_ulogic;
		rx_data_o              : out   ethernet_data_t;

		-- TX FIFO
		tx_clock_i             : in    std_ulogic;
		tx_data_i              : in    ethernet_data_t;
		tx_data_wr_en_i        : in    std_ulogic;
		tx_data_full_o         : out   std_ulogic;

--		tx_size_i              : in    tx_size_fifo_data_t;
--		tx_size_wr_en_i        : in    std_ulogic;
--		tx_size_full_o         : out   std_ulogic;

		-- Status
		link_up_o              : out   std_ulogic;
		speed_o                : out   ethernet_speed_t;
		speed_override_i       : in    ethernet_speed_t := SPEED_UNSPECIFIED
	);
end entity;

architecture rtl of ethernet_with_fifos is
	signal mac_tx_clock         : std_ulogic      := '0';
	signal mac_tx_enable        : std_ulogic      := '0';
	signal mac_tx_data          : ethernet_data_t := (others => '0');
	signal mac_tx_byte_sent     : std_ulogic      := '0';
	signal mac_tx_busy          : std_ulogic      := '0';
	signal mac_rx_clock         : std_ulogic      := '0';
	signal mac_rx_frame         : std_ulogic      := '0';
	signal mac_rx_data          : ethernet_data_t := (others => '0');
	signal mac_rx_byte_received : std_ulogic      := '0';
	signal mac_rx_error         : std_ulogic      := '0';

begin
	ethernet_inst : entity ethernet_mac.ethernet
		generic map(
			MIIM_PHY_ADDRESS      => MIIM_PHY_ADDRESS,
			MIIM_RESET_WAIT_TICKS => MIIM_RESET_WAIT_TICKS,
			MIIM_POLL_WAIT_TICKS  => MIIM_POLL_WAIT_TICKS,
			MIIM_SPEED_REGISTER   => MIIM_SPEED_REGISTER,
			MIIM_SPEED_HIGH_BIT   => MIIM_SPEED_HIGH_BIT,
			MIIM_SPEED_LOW_BIT    => MIIM_SPEED_LOW_BIT
		)
		port map(
			clock_125_i            => clock_125_i,
			clock_125_inv_i        => clock_125_inv_i,
			clock_125_unbuffered_i => clock_125_unbuffered_i,
			reset_i                => reset_i,
			mii_tx_clk_i           => mii_tx_clk_i,
			mii_tx_er_o            => mii_tx_er_o,
			mii_tx_en_o            => mii_tx_en_o,
			mii_txd_o              => mii_txd_o,
			mii_rx_clk_i           => mii_rx_clk_i,
			mii_rx_er_i            => mii_rx_er_i,
			mii_rx_dv_i            => mii_rx_dv_i,
			mii_rxd_i              => mii_rxd_i,
			gmii_gtx_clk_o         => gmii_gtx_clk_o,
			rgmii_tx_ctl_o         => rgmii_tx_ctl_o,
			rgmii_rx_ctl_i         => rgmii_rx_ctl_i,
			mdc_o                  => mdc_o,
			mdio_io                => mdio_io,
			tx_clock_o             => mac_tx_clock,
			tx_enable_i            => mac_tx_enable,
			tx_data_i              => mac_tx_data,
			tx_byte_sent_o         => mac_tx_byte_sent,
			tx_busy_o              => mac_tx_busy,
			rx_clock_o             => mac_rx_clock,
			rx_frame_o             => mac_rx_frame,
			rx_data_o              => mac_rx_data,
			rx_byte_received_o     => mac_rx_byte_received,
			rx_error_o             => mac_rx_error,
			link_up_o              => link_up_o,
			speed_o                => speed_o,
			speed_override_i       => speed_override_i
		);

	rx_memory_inst : entity ethernet_mac.rx_fifo
		port map(
			clock_i                => rx_clock_i,
			reset_i                => reset_i,
			mac_rx_clock_i         => mac_rx_clock,
			mac_rx_frame_i         => mac_rx_frame,
			mac_rx_data_i          => mac_rx_data,
			mac_rx_byte_received_i => mac_rx_byte_received,
			mac_rx_error_i         => mac_rx_error,
			rx_empty_o             => rx_empty_o,
			rx_rd_en_i             => rx_rd_en_i,
			rx_data_o              => rx_data_o
		);

	tx_fifo_inst : entity ethernet_mac.tx_fifo
		port map(
			clock_i            => tx_clock_i,
			reset_i            => reset_i,
			data_i             => tx_data_i,
			data_wr_en_i       => tx_data_wr_en_i,
			data_full_o        => tx_data_full_o,
--			size_i             => tx_size_i,
--			size_wr_en_i       => tx_size_wr_en_i,
--			size_full_o        => tx_size_full_o,
			mac_tx_clock_i     => mac_tx_clock,
			mac_tx_enable_o    => mac_tx_enable,
			mac_tx_data_o      => mac_tx_data,
			mac_tx_byte_sent_i => mac_tx_byte_sent,
			mac_tx_busy_i      => mac_tx_busy
		);

end architecture;

