-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

-- Adaption layer for data transfer with MII and GMII

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.framing_common.all;
use work.ethernet_types.all;

entity mii_gmii is
	port(
		tx_reset_i         : in  std_ulogic;
		tx_clock_i         : in  std_ulogic;
		rx_reset_i         : in  std_ulogic;
		rx_clock_i         : in  std_ulogic;

		-- MII (Media-independent interface)
		mii_tx_en_o        : out std_ulogic;
		mii_txd_o          : out std_ulogic_vector(7 downto 0);
		mii_rx_er_i        : in  std_ulogic;
		mii_rx_dv_i        : in  std_ulogic;
		mii_rxd_i          : in  std_ulogic_vector(7 downto 0);

		-- RGMII (Reduced pin count gigabit media-independent interface)
		-- Leave open if RGMII is not used
		rgmii_tx_ctl_o     : out std_ulogic;
		rgmii_rx_ctl_i     : in  std_ulogic;
		-- Other pins:
		-- mii_gtx_clk_o is TXC
		-- mii_txd_o[3:0] is TD[3:0]
		-- mii_rx_clk_i is RXC 
		-- mii_rxd_i[3:0] is RD[3:0]

		-- Interface control signals
		-- Must stay stable after tx_reset_i or rx_reset_i is deasserted
		speed_select_i     : in  t_ethernet_speed;

		-- TX/RX control
		-- TX signals synchronous to tx_clock
		tx_enable_i        : in  std_ulogic;
		tx_data_i          : in  t_ethernet_data;
		tx_byte_sent_o     : out std_ulogic;
		tx_busy_o          : out std_ulogic;

		-- RX signals synchronous to rx_clock
		-- Valid as long as one continuous frame is being received
		rx_frame_o         : out std_ulogic;
		rx_data_o          : out t_ethernet_data;
		rx_byte_received_o : out std_ulogic;
		rx_error_o         : out std_ulogic
	);
end entity;

architecture rtl of mii_gmii is
	type t_mii_gmii_tx_state is (
		TX_IDLE,
		TX_GMII,
		TX_MII_LO_QUAD,
		TX_MII_HI_QUAD,
		TX_INTER_PACKET_GAP,
		-- MII needs two clock cycles per inter-packet gap byte
		TX_INTER_PACKET_GAP_MII_HI_QUAD
	);
	signal tx_state      : t_mii_gmii_tx_state := TX_IDLE;
	signal tx_next_state : t_mii_gmii_tx_state := TX_IDLE;

	type t_mii_gmii_rx_state is (
		RX_INIT,
		RX_GMII,
		RX_MII_LO_QUAD,
		RX_MII_HI_QUAD
	);
	signal rx_state : t_mii_gmii_rx_state := RX_INIT;

	signal inter_packet_gap_counter : integer range 0 to INTER_PACKET_GAP_BYTES;

	signal tx_data : t_ethernet_data;

begin

	-- Use asynchronous reset, clock_tx is not guaranteed to be running during system initialization
	mii_gmii_tx_fsm_sync : process(tx_reset_i, tx_clock_i)
	begin
		if tx_reset_i = '1' then
			tx_state    <= TX_IDLE;
		elsif rising_edge(tx_clock_i) then
			-- Capture data on clock edges
			if tx_next_state = TX_GMII or tx_next_state = TX_MII_LO_QUAD then
				tx_data <= tx_data_i;
			end if;

			-- Remember last speed to detect speed changes
			--last_speed_select_tx <= speed_select_tx;
			-- Ignored for now, nothing terrible should happen anyway (except for maybe
			-- one corrupted frame). On the next frame, the correct speed setting is automatically chosen.

			-- Advance state
			tx_state <= tx_next_state;

			-- Increase counter
			inter_packet_gap_counter <= 0;

			case tx_next_state is
				when TX_INTER_PACKET_GAP =>
					inter_packet_gap_counter <= inter_packet_gap_counter + 1;
				when TX_INTER_PACKET_GAP_MII_HI_QUAD =>
					-- Retain counter value
					inter_packet_gap_counter <= inter_packet_gap_counter;
				when others =>
					null;
			end case;
		end if;
	end process;

	mii_gmii_tx_output : process(tx_state, tx_next_state, tx_data)
	begin
		mii_tx_en_o    <= '0';
		mii_txd_o      <= (others => '0');
		tx_busy_o      <= '0';
		tx_byte_sent_o <= '0';

		if tx_state /= TX_IDLE then
			tx_busy_o <= '1';
		end if;

		case tx_state is
			when TX_IDLE =>
				-- Look ahead to have tx_bytesent already set in the TX_GMII clock cycle
				if tx_next_state = TX_GMII then
					tx_byte_sent_o <= '1';
				end if;
			when TX_GMII =>
				mii_tx_en_o <= '1';
				mii_txd_o   <= tx_data;
				-- Look ahead again
				if tx_next_state /= TX_GMII then
					tx_byte_sent_o <= '0';
				else
					tx_byte_sent_o <= '1';
				end if;
			when TX_MII_LO_QUAD =>
				mii_tx_en_o    <= '1';
				mii_txd_o      <= "0000" & tx_data(3 downto 0);
				-- Set tx_bytesent here already so the sender can output new data when this 
				-- FSM is in TX_MII_HI_QUAD state
				tx_byte_sent_o <= '1';
			when TX_MII_HI_QUAD =>
				mii_tx_en_o <= '1';
				mii_txd_o   <= "0000" & tx_data(7 downto 4);
			when TX_INTER_PACKET_GAP | TX_INTER_PACKET_GAP_MII_HI_QUAD =>
				tx_busy_o <= '1';
		end case;
	end process;

	mii_gmii_tx_next_state : process(tx_state, tx_enable_i, speed_select_i, inter_packet_gap_counter)
	begin
		-- Retain state by default
		tx_next_state <= tx_state;

		case tx_state is
			when TX_IDLE =>
				if tx_enable_i = '1' then
					case speed_select_i is
						when SPEED_1000MBPS =>
							tx_next_state <= TX_GMII;
						when others =>
							tx_next_state <= TX_MII_LO_QUAD;
					end case;
				end if;
			when TX_GMII =>
				if tx_enable_i = '0' then
					tx_next_state <= TX_INTER_PACKET_GAP;
				end if;
			when TX_MII_LO_QUAD =>
				tx_next_state <= TX_MII_HI_QUAD;
			when TX_MII_HI_QUAD =>
				if tx_enable_i = '0' then
					tx_next_state <= TX_INTER_PACKET_GAP;
				else
					tx_next_state <= TX_MII_LO_QUAD;
				end if;
			when TX_INTER_PACKET_GAP =>
				if inter_packet_gap_counter = INTER_PACKET_GAP_BYTES - 1 then
					tx_next_state <= TX_IDLE;
				elsif speed_select_i /= SPEED_1000MBPS then
					tx_next_state <= TX_INTER_PACKET_GAP_MII_HI_QUAD;
				end if;
			when TX_INTER_PACKET_GAP_MII_HI_QUAD =>
				tx_next_state <= TX_INTER_PACKET_GAP;
		end case;
	end process;

	-- MII/GMII packet reception
	mii_gmii_rx_fsm : process(rx_clock_i, rx_reset_i)
	begin
		if rx_reset_i = '1' then
			rx_state           <= RX_INIT;
			rx_byte_received_o <= '0';
		elsif rising_edge(rx_clock_i) then
			rx_frame_o         <= '0';
			rx_byte_received_o <= '0';
			rx_error_o         <= '0';

			if rx_state /= RX_INIT then
				rx_error_o <= mii_rx_er_i;
				rx_frame_o <= mii_rx_dv_i;
			end if;

			case rx_state is
				when RX_INIT =>
					-- Wait for a pause in reception
					if mii_rx_dv_i = '0' then
						case speed_select_i is
							when SPEED_1000MBPS =>
								rx_state <= RX_GMII;
							when others =>
								rx_state <= RX_MII_LO_QUAD;
						end case;
					end if;
				when RX_GMII =>
					-- Just pass the data through
					rx_data_o          <= mii_rxd_i;
					rx_byte_received_o <= mii_rx_dv_i;
				when RX_MII_LO_QUAD =>
					-- Wait until start of reception
					if mii_rx_dv_i = '1' then
						rx_state <= RX_MII_HI_QUAD;
					end if;
					-- Capture low quad
					rx_data_o(3 downto 0) <= mii_rxd_i(3 downto 0);
				when RX_MII_HI_QUAD =>
					-- Capture high quad and mark it valid
					rx_data_o(7 downto 4) <= mii_rxd_i(3 downto 0);
					rx_byte_received_o    <= '1';
					rx_frame_o            <= '1';
					if mii_rx_dv_i = '0' then
						-- Frame ended prematurely on a half-byte
						rx_error_o <= '1';
					end if;
					rx_state <= RX_MII_LO_QUAD;
			end case;
		end if;
	end process;

end architecture;
