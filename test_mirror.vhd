-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

-- ethernet_with_fifos wrapped with an application that just loops all incoming packets back

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.ethernet_types.all;

entity test_mirror is
	port(
		clock_125_i      : in  std_ulogic;
		-- Clock used for FIFOs and MIIM
		user_clock_i     : in  std_ulogic;
		reset_i          : in  std_ulogic;

		mii_tx_clk_i     : in  std_ulogic;
		mii_tx_er_o      : out std_ulogic;
		mii_tx_en_o      : out std_ulogic;
		mii_txd_o        : out std_ulogic_vector(7 downto 0);
		mii_rx_clk_i     : in  std_ulogic;
		mii_rx_er_i      : in  std_ulogic;
		mii_rx_dv_i      : in  std_ulogic;
		mii_rxd_i        : in  std_ulogic_vector(7 downto 0);

		gmii_gtx_clk_o   : out std_ulogic;

		rgmii_tx_ctl_o   : out std_ulogic;
		rgmii_rx_ctl_i   : in  std_ulogic;

		speed_override_i : in  t_ethernet_speed;

		-- Enable/disable loopback
		enable_mirror_i  : in  std_ulogic
	);
end entity;

architecture rtl of test_mirror is
	signal rx_empty : std_ulogic;
	signal rx_rd_en : std_ulogic := '0';
	signal rx_data  : t_ethernet_data;
	signal tx_data  : t_ethernet_data;
	signal tx_wr_en : std_ulogic := '0';
	signal tx_full  : std_ulogic;
begin
	ethernet_with_fifos_inst : entity work.ethernet_with_fifos
		generic map(
			MIIM_DISABLE => TRUE
		)
		port map(
			clock_125_i      => clock_125_i,
			reset_i          => reset_i,
			mii_tx_clk_i     => mii_tx_clk_i,
			mii_tx_er_o      => mii_tx_er_o,
			mii_tx_en_o      => mii_tx_en_o,
			mii_txd_o        => mii_txd_o,
			mii_rx_clk_i     => mii_rx_clk_i,
			mii_rx_er_i      => mii_rx_er_i,
			mii_rx_dv_i      => mii_rx_dv_i,
			mii_rxd_i        => mii_rxd_i,
			gmii_gtx_clk_o   => gmii_gtx_clk_o,
			rgmii_tx_ctl_o   => rgmii_tx_ctl_o,
			rgmii_rx_ctl_i   => rgmii_rx_ctl_i,
			miim_clock_i     => user_clock_i,
			speed_override_i => speed_override_i,
			rx_clock_i       => user_clock_i,
			rx_empty_o       => rx_empty,
			rx_rd_en_i       => rx_rd_en,
			rx_data_o        => rx_data,
			tx_clock_i       => user_clock_i,
			tx_data_i        => tx_data,
			tx_wr_en_i       => tx_wr_en,
			tx_full_o        => tx_full
		);

	-- Process for mirroring packets from the RX FIFO to the TX FIFO
	-- Asynchronous to avoid complicated buffering on full/empty conditions
	fifo_mirror : process(rx_empty, tx_full, enable_mirror_i, rx_data)
	begin
		tx_wr_en <= '0' after 1 ns;
		rx_rd_en <= '0' after 1 ns;
		tx_data  <= (others => '0') after 1 ns;
		if enable_mirror_i = '1' then
			tx_data  <= rx_data after 1 ns;
			tx_wr_en <= not rx_empty and not tx_full after 1 ns;
			rx_rd_en <= not rx_empty and not tx_full after 1 ns;
		end if;
	end process;

end architecture;
