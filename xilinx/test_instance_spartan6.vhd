library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ethernet_mac;
use ethernet_mac.ethernet_types.all;

library unisim;
use unisim.vcomponents.all;

entity test_instance_spartan6 is
	port (
		clock_125_i            : in    std_ulogic;
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
		speed_override_i       : in    ethernet_speed_t
	);
end entity;

architecture rtl of test_instance_spartan6 is
	signal clock_125 : std_ulogic;
	signal clock_125_inv : std_ulogic;
	signal clock_125_inv_unbuffered : std_ulogic;
	signal clock_125_unbuffered : std_ulogic;
	signal locked: std_ulogic;
	signal reset : std_ulogic;
begin
	
	reset <= reset_i or (not locked);

	clock_125_BUFG_inst : BUFG
		port map(
			O => clock_125, -- 1-bit output: Clock buffer output
			I => clock_125_unbuffered          -- 1-bit input: Clock buffer input
		);

	clock_125_inv_BUFG_inst : BUFG
		port map(
			O => clock_125_inv,       -- 1-bit output: Clock buffer output
			I => clock_125_inv_unbuffered      -- 1-bit input: Clock buffer input
		);

	DCM_SP_inst : DCM_SP
		generic map(
			CLKDV_DIVIDE          => 5.0, -- CLKDV divide value
			-- (1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,9,10,11,12,13,14,15,16).
			CLKFX_DIVIDE          => 5, -- Divide value on CLKFX outputs - D - (1-32)
			CLKFX_MULTIPLY        => 2, -- Multiply value on CLKFX outputs - M - (2-32)
			CLKIN_DIVIDE_BY_2     => FALSE, -- CLKIN divide by two (TRUE/FALSE)
			CLKIN_PERIOD          => 8.0, -- Input clock period specified in nS
			CLKOUT_PHASE_SHIFT    => "NONE", -- Output phase shift (NONE, FIXED, VARIABLE)
			CLK_FEEDBACK          => "NONE", -- Feedback source (NONE, 1X, 2X)
			DESKEW_ADJUST         => "SYSTEM_SYNCHRONOUS", -- SYSTEM_SYNCHRNOUS or SOURCE_SYNCHRONOUS
			DFS_FREQUENCY_MODE    => "LOW", -- Unsupported - Do not change value
			DLL_FREQUENCY_MODE    => "LOW", -- Unsupported - Do not change value
			DSS_MODE              => "NONE", -- Unsupported - Do not change value
			DUTY_CYCLE_CORRECTION => TRUE, -- Unsupported - Do not change value
			FACTORY_JF            => X"c080", -- Unsupported - Do not change value
			PHASE_SHIFT           => 0, -- Amount of fixed phase shift (-255 to 255)
			STARTUP_WAIT          => FALSE -- Delay config DONE until DCM_SP LOCKED (TRUE/FALSE)
		)
		port map(
			CLK0     => clock_125_unbuffered,  -- 1-bit output: 0 degree clock output
			CLK180   => clock_125_inv_unbuffered, -- 1-bit output: 180 degree clock output
			CLK270   => open,           -- 1-bit output: 270 degree clock output
			CLK2X    => open,           -- 1-bit output: 2X clock frequency clock output
			CLK2X180 => open,           -- 1-bit output: 2X clock frequency, 180 degree clock output
			CLK90    => open,           -- 1-bit output: 90 degree clock output
			CLKDV    => open,           -- 1-bit output: Divided clock output
			CLKFX    => open,   -- 1-bit output: Digital Frequency Synthesizer output (DFS)
			CLKFX180 => open,           -- 1-bit output: 180 degree CLKFX output
			LOCKED   => locked,       -- 1-bit output: DCM_SP Lock Output
			PSDONE   => open,           -- 1-bit output: Phase shift done output
			STATUS   => open,           -- 8-bit output: DCM_SP status output
			CLKFB    => '0', -- 1-bit input: Clock feedback input
			CLKIN    => clock_125_i,    -- 1-bit input: Clock input
			DSSEN    => '0',            -- 1-bit input: Unsupported, specify to GND.
			PSCLK    => '0',            -- 1-bit input: Phase shift clock input
			PSEN     => '0',            -- 1-bit input: Phase shift enable
			PSINCDEC => '0',            -- 1-bit input: Phase shift increment/decrement input
			RST      => '0'             -- 1-bit input: Active high reset input
		);
	
	ethernet_with_fifos_inst : entity work.ethernet_with_fifos
		generic map(
			MIIM_SPEED_REGISTER   => "00000",
			MIIM_SPEED_HIGH_BIT   => 0,
			MIIM_SPEED_LOW_BIT    => 0
		)
		port map(
			clock_125_i            => clock_125,
			clock_125_inv_i        => clock_125_inv,
			clock_125_unbuffered_i => clock_125_unbuffered,
			reset_i                => reset,
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
			rx_clock_i             => rx_clock_i,
			rx_empty_o             => rx_empty_o,
			rx_rd_en_i             => rx_rd_en_i,
			rx_data_o              => rx_data_o,
			tx_clock_i             => tx_clock_i,
			tx_data_i              => tx_data_i,
			tx_data_wr_en_i        => tx_data_wr_en_i,
			tx_data_full_o         => tx_data_full_o,
			link_up_o              => link_up_o,
			speed_o                => speed_o,
			speed_override_i       => speed_override_i
		);
	
end architecture;

