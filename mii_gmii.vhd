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
		reset_i            : in  std_ulogic;
		rx_clock_i         : in  std_ulogic;
		tx_clock_i         : in  std_ulogic;

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
		-- Synchronous to clock_125_i
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
		-- MII needs two clock cycles per interframe gap byte
		TX_INTER_PACKET_GAP_MII_HI_QUAD
	);
	signal tx_state      : t_mii_gmii_tx_state := TX_IDLE;
	signal tx_next_state : t_mii_gmii_tx_state := TX_IDLE;

	type t_mii_gmii_rx_state is (
		RX_INIT,
		RX_GMII,
		RX_MII_LO_QUAD,
		RX_MII_HI_QUAD,
		RX_SPEED_CHANGE
	);
	signal rx_state      : t_mii_gmii_rx_state := RX_INIT;
	signal rx_next_state : t_mii_gmii_rx_state := RX_INIT;

	signal inter_packet_gap_counter : integer range 0 to INTER_PACKET_GAP_BYTES;

	signal tx_data             : t_ethernet_data;
	-- Only the MII low 4 bits are buffered, the other 4 bits are 
	-- forwarded directly from the input FIFO.
	-- For GMII, the whole byte is just forwarded without buffering.
	signal rx_data_mii_lo_quad : std_ulogic_vector(3 downto 0);

	signal speed_select_tx_ff : t_ethernet_speed;
	signal speed_select_tx    : t_ethernet_speed;

	signal speed_select_rx_ff   : t_ethernet_speed;
	signal speed_select_rx      : t_ethernet_speed;
	signal last_speed_select_rx : t_ethernet_speed;

begin

	-- Bring signals from clock_125_i to clock_rx clock domain
	clock_125_to_clock_rx : process(rx_clock_i)
	begin
		if rising_edge(rx_clock_i) then
			speed_select_rx_ff <= speed_select_i;
			speed_select_rx    <= speed_select_rx_ff;
		end if;
	end process;

	-- Bring signals from clock_125_i to clock_rx clock domain
	clock_125_to_clock_tx : process(tx_clock_i)
	begin
		if rising_edge(tx_clock_i) then
			speed_select_tx_ff <= speed_select_i;
			speed_select_tx    <= speed_select_tx_ff;
		end if;
	end process;

	-- Use asynchronous reset, clock_tx is not guaranteed to be running during system initialization
	mii_gmii_tx_fsm_sync : process(reset_i, tx_clock_i)
	begin
		if reset_i = '1' then
			tx_state <= TX_IDLE;
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

	mii_gmii_tx_next_state : process(tx_state, tx_enable_i, speed_select_tx, inter_packet_gap_counter)
	begin
		-- Retain state by default
		tx_next_state <= tx_state;

		case tx_state is
			when TX_IDLE =>
				if tx_enable_i = '1' then
					case speed_select_tx is
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
				elsif speed_select_tx /= SPEED_1000MBPS then
					tx_next_state <= TX_INTER_PACKET_GAP_MII_HI_QUAD;
				end if;
			when TX_INTER_PACKET_GAP_MII_HI_QUAD =>
				tx_next_state <= TX_INTER_PACKET_GAP;
		end case;
	end process;

	mii_gmii_rx_fsm_sync : process(reset_i, rx_clock_i)
	begin
		if reset_i = '1' then
			rx_state <= RX_INIT;
		elsif rising_edge(rx_clock_i) then
			rx_state <= rx_next_state;

			-- Remember last speed to detect speed changes
			last_speed_select_rx <= speed_select_rx;

			-- Capture data
			if rx_state = RX_MII_LO_QUAD then
				rx_data_mii_lo_quad <= mii_rxd_i(3 downto 0);
			end if;

		end if;
	end process;

	mii_gmii_rx_output : process(rx_state, rx_data_mii_lo_quad, mii_rx_dv_i, mii_rx_er_i, mii_rxd_i)
	begin
		rx_frame_o         <= '0';
		rx_data_o          <= (others => '0');
		rx_byte_received_o <= '0';
		rx_error_o         <= '0';

		-- Frame/error control signals
		case rx_state is
			when RX_GMII | RX_MII_LO_QUAD | RX_MII_HI_QUAD =>
				rx_frame_o <= mii_rx_dv_i;
				rx_error_o <= mii_rx_er_i;
			when RX_SPEED_CHANGE =>
				rx_frame_o <= mii_rx_dv_i;
				-- Output an error to throw the frame away at the receiver
				-- if speed transition occurs while receiving
				rx_error_o <= '1';
			when others =>
				null;
		end case;

		-- Data and data valid signals
		case rx_state is
			when RX_GMII =>
				-- Pass data through directly
				rx_data_o          <= mii_rxd_i;
				rx_byte_received_o <= mii_rx_dv_i; -- '1';
			when RX_MII_LO_QUAD =>
				-- Delay needed for behavioral simulation
				-- Pass high 4 bits through directly, use the buffered 4 bits for the other part
				rx_data_o          <= mii_rxd_i(3 downto 0) & rx_data_mii_lo_quad;
				rx_byte_received_o <= '0';
			when RX_MII_HI_QUAD =>
				rx_data_o          <= mii_rxd_i(3 downto 0) & rx_data_mii_lo_quad;
				rx_byte_received_o <= '1';
			when others =>
				null;
		end case;
	end process;

	mii_gmii_rx_fsm : process(last_speed_select_rx, speed_select_rx, rx_state, mii_rx_dv_i)
	begin
		rx_next_state <= rx_state;

		if last_speed_select_rx /= speed_select_rx then
			rx_next_state <= RX_SPEED_CHANGE;
		else
			case rx_state is
				when RX_INIT =>
					-- Wait for a pause in reception
					if mii_rx_dv_i = '0' then
						case speed_select_rx is
							when SPEED_1000MBPS =>
								rx_next_state <= RX_GMII;
							when others =>
								rx_next_state <= RX_MII_LO_QUAD;
						end case;
					end if;
				when RX_GMII =>
					null;
				when RX_MII_LO_QUAD =>
					-- Stay here until start of reception
					if mii_rx_dv_i = '1' then
						rx_next_state <= RX_MII_HI_QUAD;
					end if;
				when RX_MII_HI_QUAD =>
					rx_next_state <= RX_MII_LO_QUAD;
				when RX_SPEED_CHANGE =>
					rx_next_state <= RX_INIT;
			end case;
		end if;
	end process;

end architecture;
