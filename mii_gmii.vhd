library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ethernet_mac;
use ethernet_mac.mii_types.all;
use ethernet_mac.ethernet_types.all;

-- MII Reconciliation Sublayer
entity mii_rs is
	port(
		clock_125_i            : in  std_ulogic;
		clock_125_inv_i        : in  std_ulogic;
		clock_125_unbuffered_i : in  std_ulogic;
		reset_i                : in  std_ulogic;

		-- MII (Media-independent interface)
		mii_tx_clk_i           : in  std_ulogic;
		mii_tx_er_o            : out std_ulogic;
		mii_tx_en_o            : out std_ulogic;
		mii_txd_o              : out std_ulogic_vector(7 downto 0);
		mii_rx_clk_i           : in  std_ulogic;
		mii_rx_er_i            : in  std_ulogic;
		mii_rx_dv_i            : in  std_ulogic;
		mii_rxd_i              : in  std_ulogic_vector(7 downto 0);
		--mii_crs_i          : in  std_ulogic;
		--mii_col_i          : in  std_ulogic;

		-- GMII (Gigabit media-independent interface)
		gmii_gtx_clk_o         : out std_ulogic;

		-- RGMII (Reduced pin count gigabit media-independent interface)
		-- Leave open if RGMII is not used
		rgmii_tx_ctl_o         : out std_ulogic;
		rgmii_rx_ctl_i         : in  std_ulogic;
		-- Other pins:
		-- mii_gtx_clk_o is TXC
		-- mii_txd_o[3:0] is TD[3:0]
		-- mii_rx_clk_i is RXC 
		-- mii_rxd_i[3:0] is RD[3:0]

		-- Interface control signals
		-- Synchronous to clock_125_i
		speed_select_i         : in  ethernet_speed_t;

		-- TX/RX FIFO
		--		tx_fifo_rd_en_o    : out std_ulogic;
		--		tx_fifo_rd_data_i  : in  tx_fifo_data_t;
		--		tx_fifo_empty_i    : in  std_ulogic;
		--		rx_fifo_wr_en_o    : out std_ulogic;
		--		rx_fifo_wr_data_o  : out rx_fifo_data_t;
		--		rx_fifo_full_i     : in  std_ulogic

		-- TX/RX control
		-- TX signals synchronous to tx_clock_o
		tx_clock_o             : out std_ulogic;
		tx_enable_i            : in  std_ulogic;
		tx_data_i              : in  ethernet_data_t;
		tx_byte_sent_o         : out std_ulogic;
		tx_busy_o              : out std_ulogic;

		-- RX signals synchronous to rx_clock_o
		rx_clock_o             : out std_ulogic;
		-- Valid as long as one continuous frame is being received
		rx_frame_o             : out std_ulogic;
		rx_data_o              : out ethernet_data_t;
		rx_byte_received_o     : out std_ulogic;
		rx_error_o             : out std_ulogic
	);
end entity;

architecture rtl of mii_rs is
	type mii_gmii_tx_state_t is (
		TX_IDLE,
		TX_GMII,
		TX_MII_LO_QUAD,
		TX_MII_HI_QUAD,
		TX_INTERFRAME_GAP,
		-- MII needs two clock cycles per interframe gap byte
		TX_INTERFRAME_GAP_MII_HI_QUAD
	);
	signal tx_state      : mii_gmii_tx_state_t := TX_IDLE;
	signal tx_next_state : mii_gmii_tx_state_t := TX_IDLE;

	type mii_gmii_rx_state_t is (
		RX_INIT,
		RX_GMII,
		RX_MII_LO_QUAD,
		RX_MII_HI_QUAD,
		RX_SPEED_CHANGE
	);
	signal rx_state      : mii_gmii_rx_state_t := RX_INIT;
	signal rx_next_state : mii_gmii_rx_state_t := RX_INIT;

	signal inter_packet_gap_counter : integer range 0 to INTER_PACKET_GAP := 0;

	signal mii_tx_er : std_ulogic      := '0';
	signal mii_tx_en : std_ulogic      := '0';
	signal mii_txd   : ethernet_data_t := (others => '0');
	signal mii_rx_er : std_ulogic      := '0';
	signal mii_rx_dv : std_ulogic      := '0';
	signal mii_rxd   : ethernet_data_t := (others => '0');

	signal mii_rx_er_tmp : std_ulogic      := '0';
	signal mii_rx_dv_tmp : std_ulogic      := '0';
	signal mii_rxd_tmp   : ethernet_data_t := (others => '0');

	signal tx_data             : ethernet_data_t := (others => '0');
	-- Only the MII low 4 bits are buffered, the other 4 bits are 
	-- forwarded directly from the input FIFO.
	-- For GMII, the whole byte is just forwarded without buffering.
	signal rx_data_mii_lo_quad : std_ulogic_vector(3 downto 0);

	signal clock_tx : std_ulogic;
	signal clock_rx : std_ulogic;

	signal speed_select_tx_ff : ethernet_speed_t;
	signal speed_select_tx    : ethernet_speed_t;
	--signal last_speed_select_tx : mii_speed_t;

	signal speed_select_rx_ff   : ethernet_speed_t;
	signal speed_select_rx      : ethernet_speed_t;
	signal last_speed_select_rx : ethernet_speed_t;

begin
	mii_rx_er <= transport mii_rx_er_tmp;
	mii_rx_dv <= transport mii_rx_dv_tmp;
	mii_rxd   <= transport mii_rxd_tmp;

	-- Never transmit errors
	mii_tx_er <= '0';

	tx_clock_o <= clock_tx;
	rx_clock_o <= clock_rx;

-- Synchronizer: use no SR extract, ASYNC_REG

	-- Bring signals from clock_125_i to clock_rx clock domain
	clock_125_to_clock_rx : process(clock_rx)
	begin
		if rising_edge(clock_rx) then
			speed_select_rx_ff <= speed_select_i;
			speed_select_rx    <= speed_select_rx_ff;
		end if;
	end process;

	-- Bring signals from clock_125_i to clock_rx clock domain
	clock_125_to_clock_tx : process(clock_tx)
	begin
		if rising_edge(clock_tx) then
			speed_select_tx_ff <= speed_select_i;
			speed_select_tx    <= speed_select_tx_ff;
		end if;
	end process;

	-- Use asynchronous reset, clock_tx is not guaranteed to be running during system initialization
	mii_gmii_tx_fsm_sync : process(reset_i, clock_tx)
	begin
		if reset_i = '1' then
			tx_state <= TX_IDLE;
		elsif rising_edge(clock_tx) then
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
				when TX_INTERFRAME_GAP =>
					inter_packet_gap_counter <= inter_packet_gap_counter + 1;
				when TX_INTERFRAME_GAP_MII_HI_QUAD =>
					-- Retain counter value
					inter_packet_gap_counter <= inter_packet_gap_counter;
				when others =>
					null;
			end case;
		end if;
	end process;

	mii_gmii_tx_output : process(tx_state, tx_next_state, tx_data)
	begin
		mii_tx_en      <= '0';
		mii_txd        <= (others => '0');
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
				mii_tx_en <= '1';
				mii_txd   <= tx_data;
				-- Look ahead again
				if tx_next_state /= TX_GMII then
					tx_byte_sent_o <= '0';
				else
					tx_byte_sent_o <= '1';
				end if;
			when TX_MII_LO_QUAD =>
				mii_tx_en      <= '1';
				mii_txd        <= "0000" & tx_data(3 downto 0);
				-- Set tx_bytesent here already so the sender can output new data when this 
				-- FSM is in TX_MII_HI_QUAD state
				tx_byte_sent_o <= '1';
			when TX_MII_HI_QUAD =>
				mii_tx_en <= '1';
				mii_txd   <= "0000" & tx_data(7 downto 4);
			when TX_INTERFRAME_GAP | TX_INTERFRAME_GAP_MII_HI_QUAD =>
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
					tx_next_state <= TX_INTERFRAME_GAP;
				end if;
			when TX_MII_LO_QUAD =>
				tx_next_state <= TX_MII_HI_QUAD;
			when TX_MII_HI_QUAD =>
				if tx_enable_i = '0' then
					tx_next_state <= TX_INTERFRAME_GAP;
				else
					tx_next_state <= TX_MII_LO_QUAD;
				end if;
			when TX_INTERFRAME_GAP =>
				if inter_packet_gap_counter = INTER_PACKET_GAP - 1 then
					tx_next_state <= TX_IDLE;
				elsif speed_select_tx /= SPEED_1000MBPS then
					tx_next_state <= TX_INTERFRAME_GAP_MII_HI_QUAD;
				end if;
			when TX_INTERFRAME_GAP_MII_HI_QUAD =>
				tx_next_state <= TX_INTERFRAME_GAP;
		end case;
	end process;

	mii_gmii_rx_fsm_sync : process(reset_i, clock_rx)
	begin
		if reset_i = '1' then
			rx_state <= RX_INIT;
		elsif rising_edge(clock_rx) then
			rx_state <= rx_next_state;

			-- Remember last speed to detect speed changes
			last_speed_select_rx <= speed_select_rx;

			-- Capture data
			if rx_state = RX_MII_LO_QUAD then
				rx_data_mii_lo_quad <= mii_rxd(3 downto 0);
			end if;

		end if;
	end process;

	mii_gmii_rx_output : process(rx_state, rx_data_mii_lo_quad, mii_rx_dv, mii_rx_er, mii_rxd)
	begin
		rx_frame_o         <= '0';
		rx_data_o          <= (others => '0');
		rx_byte_received_o <= '0';
		rx_error_o         <= '0';

		-- Frame/error control signals
		case rx_state is
			when RX_GMII | RX_MII_LO_QUAD | RX_MII_HI_QUAD =>
				rx_frame_o <= mii_rx_dv;
				rx_error_o <= mii_rx_er;
			when RX_SPEED_CHANGE =>
				rx_frame_o <= mii_rx_dv;
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
				rx_data_o          <= mii_rxd;
				rx_byte_received_o <= mii_rx_dv; -- '1';
			when RX_MII_LO_QUAD =>
				-- Delay needed for behavioral simulation
				-- Pass high 4 bits through directly, use the buffered 4 bits for the other part
				rx_data_o          <= mii_rxd(3 downto 0) & rx_data_mii_lo_quad;
				rx_byte_received_o <= '0';
			when RX_MII_HI_QUAD =>
				rx_data_o          <= mii_rxd(3 downto 0) & rx_data_mii_lo_quad;
				rx_byte_received_o <= '1';
			when others =>
				null;
		end case;
	end process;

	mii_gmii_rx_fsm : process(last_speed_select_rx, speed_select_rx, rx_state, mii_rx_dv)
	begin
		rx_next_state <= rx_state;

		if last_speed_select_rx /= speed_select_rx then
			rx_next_state <= RX_SPEED_CHANGE;
		else
			case rx_state is
				when RX_INIT =>
					-- Wait for a pause in reception
					if mii_rx_dv = '0' then
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
					if mii_rx_dv = '1' then
						rx_next_state <= RX_MII_HI_QUAD;
					end if;
				when RX_MII_HI_QUAD =>
					rx_next_state <= RX_MII_LO_QUAD;
				when RX_SPEED_CHANGE =>
					rx_next_state <= RX_INIT;
			end case;
		end if;
	end process;

	mii_gmii_io_inst : entity work.mii_gmii_io
		port map(
			clock_125_i            => clock_125_i,
			clock_125_inv_i        => clock_125_inv_i,
			clock_125_unbuffered_i => clock_125_unbuffered_i,
			clock_tx_o             => clock_tx,
			clock_rx_o             => clock_rx,
			speed_select_i         => speed_select_i,
			mii_tx_clk_i           => mii_tx_clk_i,
			mii_tx_er_o            => mii_tx_er_o,
			mii_tx_en_o            => mii_tx_en_o,
			mii_txd_o              => mii_txd_o,
			mii_rx_clk_i           => mii_rx_clk_i,
			mii_rx_er_i            => mii_rx_er_i,
			mii_rx_dv_i            => mii_rx_dv_i,
			mii_rxd_i              => mii_rxd_i,
			gmii_gtx_clk_o         => gmii_gtx_clk_o,
			int_mii_tx_er_i        => mii_tx_er,
			int_mii_tx_en_i        => mii_tx_en,
			int_mii_txd_i          => mii_txd,
			int_mii_rx_er_o        => mii_rx_er_tmp,
			int_mii_rx_dv_o        => mii_rx_dv_tmp,
			int_mii_rxd_o          => mii_rxd_tmp
		);

end architecture rtl;
