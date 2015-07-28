-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ethernet_mac;
use ethernet_mac.ethernet_types.all;
use ethernet_mac.framing_types.all;
use ethernet_mac.utility.all;

entity ethernet_mac_tb is
end entity;

architecture behavioral of ethernet_mac_tb is

	-- ethernet_with_fifos signals
	signal clock_125            : std_ulogic := '0';
	signal clock_125_inv        : std_ulogic;
	signal clock_125_unbuffered : std_ulogic;
	signal reset                : std_ulogic;
	signal mii_tx_clk           : std_ulogic := '0';
	signal mii_tx_er            : std_ulogic;
	signal mii_tx_en            : std_ulogic;
	signal mii_txd              : std_ulogic_vector(7 downto 0);
	signal mii_rx_clk           : std_ulogic := '0';
	signal mii_rx_er            : std_ulogic;
	signal mii_rx_dv            : std_ulogic;
	signal mii_rxd              : std_ulogic_vector(7 downto 0);
	signal gmii_gtx_clk         : std_ulogic;
	signal rx_clock             : std_ulogic;
	signal rx_empty             : std_ulogic;
	signal rx_rd_en             : std_ulogic;
	signal rx_data              : ethernet_data_t;
	signal tx_clock             : std_ulogic;
	signal tx_data              : ethernet_data_t;
	signal tx_data_wr_en        : std_ulogic;
	signal tx_data_full         : std_ulogic;
	signal link_up              : std_ulogic;
	signal speed                : ethernet_speed_t;

	-- Testbench signals
	signal speed_internal   : ethernet_speed_t;
	signal send_packet_req  : boolean := FALSE;
	signal send_packet_ack  : boolean := FALSE;
	signal send_packet_size : integer := 0;
	type t_packet_data is array (0 to 2000) of ethernet_data_t;
	signal send_packet_data : t_packet_data;

	-- Clock period definitions
	constant clock_125_period : time := 8 ns;
	constant clock_25_period  : time := 40 ns;
	constant clock_2_5_period : time := 400 ns;
	constant mii_rx_setup     : time := 2 ns;
	constant mii_rx_hold      : time := 0 ns;

	-- Functions
	impure function mii_rx_clk_period return time is
	begin
		case speed_internal is
			when SPEED_10MBPS =>
				return clock_2_5_period;
			when SPEED_100MBPS =>
				return clock_25_period;
			when others =>
				return clock_125_period;
		end case;
	end function;

	-- "Known good" CRC32 function for comparison from chips example project
	-- polynomial: (0 1 2 4 5 7 8 10 11 12 16 22 23 26 32)
	-- data width: 8
	-- convention: the first serial bit is D[0]
	function NEXTCRC32_D8(DATA : std_ulogic_vector(7 downto 0);
		                  CRC  : std_ulogic_vector(31 downto 0)) return std_ulogic_vector is
		variable D      : std_ulogic_vector(7 downto 0);
		variable C      : std_ulogic_vector(31 downto 0);
		variable NEWCRC : std_ulogic_vector(31 downto 0);

	begin
		D          := DATA;
		C          := CRC;
		NewCRC(0)  := C(24) xor C(30) xor D(1) xor D(7);
		NewCRC(1)  := C(25) xor C(31) xor D(0) xor D(6) xor C(24) xor C(30) xor D(1) xor D(7);
		NewCRC(2)  := C(26) xor D(5) xor C(25) xor C(31) xor D(0) xor D(6) xor C(24) xor C(30) xor D(1) xor D(7);
		NewCRC(3)  := C(27) xor D(4) xor C(26) xor D(5) xor C(25) xor C(31) xor D(0) xor D(6);
		NewCRC(4)  := C(28) xor D(3) xor C(27) xor D(4) xor C(26) xor D(5) xor C(24) xor C(30) xor D(1) xor D(7);
		NewCRC(5)  := C(29) xor D(2) xor C(28) xor D(3) xor C(27) xor D(4) xor C(25) xor C(31) xor D(0) xor D(6) xor C(24) xor C(30) xor D(1) xor D(7);
		NewCRC(6)  := C(30) xor D(1) xor C(29) xor D(2) xor C(28) xor D(3) xor C(26) xor D(5) xor C(25) xor C(31) xor D(0) xor D(6);
		NewCRC(7)  := C(31) xor D(0) xor C(29) xor D(2) xor C(27) xor D(4) xor C(26) xor D(5) xor C(24) xor D(7);
		NewCRC(8)  := C(0) xor C(28) xor D(3) xor C(27) xor D(4) xor C(25) xor D(6) xor C(24) xor D(7);
		NewCRC(9)  := C(1) xor C(29) xor D(2) xor C(28) xor D(3) xor C(26) xor D(5) xor C(25) xor D(6);
		NewCRC(10) := C(2) xor C(29) xor D(2) xor C(27) xor D(4) xor C(26) xor D(5) xor C(24) xor D(7);
		NewCRC(11) := C(3) xor C(28) xor D(3) xor C(27) xor D(4) xor C(25) xor D(6) xor C(24) xor D(7);
		NewCRC(12) := C(4) xor C(29) xor D(2) xor C(28) xor D(3) xor C(26) xor D(5) xor C(25) xor D(6) xor C(24) xor C(30) xor D(1) xor D(7);
		NewCRC(13) := C(5) xor C(30) xor D(1) xor C(29) xor D(2) xor C(27) xor D(4) xor C(26) xor D(5) xor C(25) xor C(31) xor D(0) xor D(6);
		NewCRC(14) := C(6) xor C(31) xor D(0) xor C(30) xor D(1) xor C(28) xor D(3) xor C(27) xor D(4) xor C(26) xor D(5);
		NewCRC(15) := C(7) xor C(31) xor D(0) xor C(29) xor D(2) xor C(28) xor D(3) xor C(27) xor D(4);
		NewCRC(16) := C(8) xor C(29) xor D(2) xor C(28) xor D(3) xor C(24) xor D(7);
		NewCRC(17) := C(9) xor C(30) xor D(1) xor C(29) xor D(2) xor C(25) xor D(6);
		NewCRC(18) := C(10) xor C(31) xor D(0) xor C(30) xor D(1) xor C(26) xor D(5);
		NewCRC(19) := C(11) xor C(31) xor D(0) xor C(27) xor D(4);
		NewCRC(20) := C(12) xor C(28) xor D(3);
		NewCRC(21) := C(13) xor C(29) xor D(2);
		NewCRC(22) := C(14) xor C(24) xor D(7);
		NewCRC(23) := C(15) xor C(25) xor D(6) xor C(24) xor C(30) xor D(1) xor D(7);
		NewCRC(24) := C(16) xor C(26) xor D(5) xor C(25) xor C(31) xor D(0) xor D(6);
		NewCRC(25) := C(17) xor C(27) xor D(4) xor C(26) xor D(5);
		NewCRC(26) := C(18) xor C(28) xor D(3) xor C(27) xor D(4) xor C(24) xor C(30) xor D(1) xor D(7);
		NewCRC(27) := C(19) xor C(29) xor D(2) xor C(28) xor D(3) xor C(25) xor C(31) xor D(0) xor D(6);
		NewCRC(28) := C(20) xor C(30) xor D(1) xor C(29) xor D(2) xor C(26) xor D(5);
		NewCRC(29) := C(21) xor C(31) xor D(0) xor C(30) xor D(1) xor C(27) xor D(4);
		NewCRC(30) := C(22) xor C(31) xor D(0) xor C(28) xor D(3);
		NewCRC(31) := C(23) xor C(29) xor D(2);

		return NEWCRC;
	end function;

	function fcs_output_byte(fcs : std_ulogic_vector(31 downto 0); byte : integer) return std_ulogic_vector is
	begin
		return not reverse_vector(fcs)((((byte + 1) * 8) - 1) downto byte * 8);
	end function fcs_output_byte;

begin
	clock_125_unbuffered <= clock_125;
	clock_125_inv        <= not clock_125;

	rx_clock <= clock_125;
	tx_clock <= clock_125;

	-- Instantiate component
	ethernet_mac_inst : entity ethernet_mac.ethernet_with_fifos
		generic map(
			MIIM_SPEED_REGISTER => "00000",
			MIIM_SPEED_HIGH_BIT => 0,
			MIIM_SPEED_LOW_BIT  => 0
		)
		port map(
			clock_125_i            => clock_125,
			clock_125_inv_i        => clock_125_inv,
			clock_125_unbuffered_i => clock_125_unbuffered,
			reset_i                => reset,
			mii_tx_clk_i           => mii_tx_clk,
			mii_tx_er_o            => mii_tx_er,
			mii_tx_en_o            => mii_tx_en,
			mii_txd_o              => mii_txd,
			mii_rx_clk_i           => mii_rx_clk,
			mii_rx_er_i            => mii_rx_er,
			mii_rx_dv_i            => mii_rx_dv,
			mii_rxd_i              => mii_rxd,
			gmii_gtx_clk_o         => gmii_gtx_clk,
			rgmii_tx_ctl_o         => open,
			rgmii_rx_ctl_i         => '0',
			mdc_o                  => open,
			mdio_io                => '0',
			rx_clock_i             => rx_clock,
			rx_empty_o             => rx_empty,
			rx_rd_en_i             => rx_rd_en,
			rx_data_o              => rx_data,
			tx_clock_i             => tx_clock,
			tx_data_i              => tx_data,
			tx_data_wr_en_i        => tx_data_wr_en,
			tx_data_full_o         => tx_data_full,
			link_up_o              => link_up,
			speed_o                => speed
		);

	-- Generate clocks
	clock_125_process : process
	begin
		clock_125 <= not clock_125;
		wait for clock_125_period / 2;
	end process;

	mii_tx_clk_process : process
	begin
		case speed_internal is
			when SPEED_10MBPS =>
				mii_tx_clk <= not mii_tx_clk;
				wait for clock_2_5_period / 2;
			when SPEED_100MBPS =>
				mii_tx_clk <= not mii_tx_clk;
				wait for clock_25_period / 2;
			when others =>
				-- MII TX_CLK is inactive in 1 Gbps mode
				wait until ((speed_internal = SPEED_10MBPS) or (speed_internal = SPEED_100MBPS));
		end case;
	end process;

	-- Process for stimulating the MII RX interface
	packet_send_process : process is
		-- lolisim
		-- crashes if (others => '0') is used instead of "00000000"
		procedure mii_rx_cycle(data : in std_ulogic_vector(7 downto 0) := "XXXXXXXX";
			                   dv   : in std_ulogic                    := '1';
			                   er   : in std_ulogic                    := '0') is
		begin
			mii_rx_clk <= '0';
			wait for (mii_rx_clk_period / 2) - mii_rx_setup;
			mii_rx_dv <= dv;
			mii_rx_er <= er;
			mii_rxd   <= data;
			wait for mii_rx_setup;
			mii_rx_clk <= '1';
			wait for mii_rx_hold;
			mii_rxd   <= (others => 'X');
			mii_rx_dv <= '0';
			mii_rx_er <= '0';
			wait for (mii_rx_clk_period / 2) - mii_rx_hold;
		end procedure;

		procedure mii_rx_put(
			data : in std_ulogic_vector(7 downto 0) := "XXXXXXXX";
			dv   : in std_ulogic                    := '1';
			er   : in std_ulogic                    := '0') is
		begin
			if speed_internal = SPEED_1000MBPS then
				mii_rx_cycle(data, dv, er);
			else
				mii_rx_cycle("XXXX" & data(3 downto 0), dv, er);
				mii_rx_cycle("XXXX" & data(7 downto 4), dv, er);
			end if;
		end procedure;

		procedure mii_rx_toggle is
		begin
			mii_rx_put(dv => '0', er => '0', data => open);
		end procedure;

		variable fcs : std_ulogic_vector(32 downto 0);
	begin
		while not send_packet_req loop
			mii_rx_toggle;
		end loop;

		-- Preamble
		for i in 0 to 3 loop
			mii_rx_put(PREAMBLE_DATA);
		end loop;
		-- SFD
		mii_rx_put(START_FRAME_DELIMITER_DATA);
		-- Data
		fcs := (others => '1');
		for i in 0 to send_packet_size - 1 loop
			mii_rx_put(send_packet_data(i));
			fcs := NEXTCRC32_D8(send_packet_data(i), fcs);
		end loop;
		-- FCS
		mii_rx_put(fcs_output_byte(fcs, 0));
		mii_rx_put(fcs_output_byte(fcs, 1));
		mii_rx_put(fcs_output_byte(fcs, 2));
		mii_rx_put(fcs_output_byte(fcs, 3));
		-- IFG
		for i in 0 to 11 loop
			mii_rx_toggle;
		end loop;

		send_packet_ack <= TRUE;
		wait until not send_packet_req;
	end process;
	
	-- Process for mirroring packets from the RX FIFO to the TX FIFO
	fifo_mirror_process : process is
	begin
		if rising_edge(clock) then
			bscan_uart_fifo_write_enable <= '0';
			tx_data_wr_en <= '0';
			--tx_size_wr_en <= '0';
			rx_re         <= '0';
			if reset = '1' then
				is_receiving := FALSE;
				recv_cnt     := 0;
			else
				if is_receiving then
					rx_re <= '1';
					if rx_empty = '0' then
						if rx_re = '1' then
							-- Put only sequence number on JTAG out
							if recv_cnt >= 17 and recv_cnt <= 17 + 3 then
								bscan_uart_fifo_write_data   <= rx_data;
								bscan_uart_fifo_write_enable <= '1';
							end if;
							--if recv_cnt >= 2 then
								tx_data_wr_en <= '1';
								tx_data       <= rx_data;
								-- Swap direction flag
								if recv_cnt = 16 then
									tx_data <= "00000001";
								-- Replace some data
							elsif recv_cnt >= 50 then
								tx_data <= "10001100";
								end if;
							--end if;
							recv_cnt := recv_cnt + 1;
						end if;
					else
--						tx_size_wr_en <= '1';
--						tx_size       <= std_ulogic_vector(to_unsigned(recv_cnt - 2, 16));
						is_receiving  := FALSE;
					end if;
				else
					recv_cnt := 0;
					rx_re    <= '0';
					if rx_empty = '0' then
						is_receiving := TRUE;
					end if;
				end if;
			end if;
		end if;
	end process;

-- Generate reset


end architecture;
