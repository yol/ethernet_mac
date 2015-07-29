-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

library ieee;
use ieee.std_logic_1164.all;


use work.ethernet_types.all;
use work.mii_types.all;
use work.miim_types.all;

entity ethernet is
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

		-- TX from client logic
		tx_clock_o             : out   std_ulogic;
		tx_enable_i            : in    std_ulogic;
		tx_data_i              : in    ethernet_data_t;
		tx_byte_sent_o         : out   std_ulogic;
		tx_busy_o              : out   std_ulogic;

		-- RX to client logic 
		rx_clock_o             : out   std_ulogic;
		rx_frame_o             : out   std_ulogic;
		rx_data_o              : out   ethernet_data_t;
		rx_byte_received_o     : out   std_ulogic;
		rx_error_o             : out   std_ulogic;

		-- Status
		link_up_o              : out   std_ulogic;
		speed_o                : out   ethernet_speed_t;
		speed_override_i       : in    ethernet_speed_t := SPEED_UNSPECIFIED
	);
end entity;

architecture rtl of ethernet is
	signal rs_tx_enable        : std_ulogic;
	signal rs_tx_data          : ethernet_data_t;
	signal rs_tx_clock         : std_ulogic;
	signal rs_tx_byte_sent     : std_ulogic;
	signal rs_tx_busy          : std_ulogic;
	signal rs_rx_clock         : std_ulogic;
	signal rs_rx_frame         : std_ulogic;
	signal rs_rx_data          : ethernet_data_t;
	signal rs_rx_byte_received : std_ulogic;
	signal rs_rx_error         : std_ulogic;

	signal miim_register_address : register_address_t;
	signal miim_phy_address_sig  : phy_address_t;
	signal miim_data_read        : data_t;
	signal miim_data_write       : data_t;
	signal miim_req              : std_ulogic;
	signal miim_ack              : std_ulogic;
	signal miim_write_enable     : std_ulogic;
	signal miim_speed            : ethernet_speed_t;
	signal speed_select          : ethernet_speed_t;
	signal link_up               : std_ulogic;
begin
	tx_clock_o           <= rs_tx_clock;
	rx_clock_o           <= rs_rx_clock;
	link_up_o            <= link_up;
	speed_o              <= speed_select;
	miim_phy_address_sig <= MIIM_PHY_ADDRESS;

	with speed_override_i select speed_select <=
		miim_speed when SPEED_UNSPECIFIED,
		speed_override_i when others;

	mii_rs_inst : entity work.mii_gmii
		port map(
			clock_125_i            => clock_125_i,
			clock_125_inv_i        => clock_125_inv_i,
			clock_125_unbuffered_i => clock_125_unbuffered_i,
			reset_i                => reset_i,

			-- MII (Media-independent interface)
			mii_tx_clk_i           => mii_tx_clk_i,
			mii_tx_er_o            => mii_tx_er_o,
			mii_tx_en_o            => mii_tx_en_o,
			mii_txd_o              => mii_txd_o,
			mii_rx_clk_i           => mii_rx_clk_i,
			mii_rx_er_i            => mii_rx_er_i,
			mii_rx_dv_i            => mii_rx_dv_i,
			mii_rxd_i              => mii_rxd_i,

			-- GMII (Gigabit media-independent interface)
			gmii_gtx_clk_o         => gmii_gtx_clk_o,

			-- RGMII (Reduced pin count gigabit media-independent interface)
			rgmii_tx_ctl_o         => open,
			rgmii_rx_ctl_i         => '0',

			-- Interface control signals
			speed_select_i         => speed_select,
			tx_enable_i            => rs_tx_enable,
			tx_data_i              => rs_tx_data,
			tx_clock_o             => rs_tx_clock,
			tx_byte_sent_o         => rs_tx_byte_sent,
			tx_busy_o              => rs_tx_busy,
			rx_clock_o             => rs_rx_clock,
			rx_frame_o             => rs_rx_frame,
			rx_data_o              => rs_rx_data,
			rx_byte_received_o     => rs_rx_byte_received,
			rx_error_o             => rs_rx_error
		);

	eth_framing_inst : entity work.eth_framing
		port map(
			reset_i               => reset_i,
			tx_clock_i            => rs_tx_clock,
			rx_clock_i            => rs_rx_clock,
			tx_enable_i           => tx_enable_i,
			tx_data_i             => tx_data_i,
			tx_byte_sent_o        => tx_byte_sent_o,
			tx_busy_o             => tx_busy_o,
			rx_frame_o            => rx_frame_o,
			rx_data_o             => rx_data_o,
			rx_byte_received_o    => rx_byte_received_o,
			rx_error_o            => rx_error_o,
			rs_tx_enable_o        => rs_tx_enable,
			rs_tx_data_o          => rs_tx_data,
			rs_tx_byte_sent_i     => rs_tx_byte_sent,
			rs_tx_busy_i          => rs_tx_busy,
			rs_rx_frame_i         => rs_rx_frame,
			rs_rx_data_i          => rs_rx_data,
			rs_rx_byte_received_i => rs_rx_byte_received,
			rs_rx_error_i         => rs_rx_error
		);

	miim_inst : entity work.miim
		generic map(
			clock_divider => 50
		)
		port map(
			reset_i            => reset_i,
			clock_i            => clock_125_i,
			register_address_i => miim_register_address,
			phy_address_i      => miim_phy_address_sig,
			data_read_o        => miim_data_read,
			data_write_i       => miim_data_write,
			req_i              => miim_req,
			ack_o              => miim_ack,
			we_i               => miim_write_enable,
			mdc_o              => mdc_o,
			mdio_io            => mdio_io
		);

	miim_control_inst : entity work.miim_control
		generic map(
			RESET_WAIT_TICKS => MIIM_RESET_WAIT_TICKS,
			POLL_WAIT_TICKS  => MIIM_POLL_WAIT_TICKS,
			SPEED_REGISTER   => MIIM_SPEED_REGISTER,
			SPEED_HIGH_BIT   => MIIM_SPEED_HIGH_BIT,
			SPEED_LOW_BIT    => MIIM_SPEED_LOW_BIT,
			DEBUG_OUTPUT     => FALSE
		)
		port map(
			reset_i                 => reset_i,
			clock_i                 => clock_125_i,
			miim_register_address_o => miim_register_address,
			miim_data_read_i        => miim_data_read,
			miim_data_write_o       => miim_data_write,
			miim_req_o              => miim_req,
			miim_ack_i              => miim_ack,
			miim_we_o               => miim_write_enable,
			link_up_o               => link_up,
			speed_o                 => miim_speed,
			debug_fifo_we_o         => open,
			debug_fifo_write_data_o => open
		);
end architecture;

