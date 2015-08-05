-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

-- Self-checking testbench for the complete ethernet_mac

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.ethernet_types.all;
use work.framing_common.all;
use work.utility.all;
use work.crc32.all;

entity ethernet_mac_tb is
end entity;

architecture behavioral of ethernet_mac_tb is

	-- ethernet_with_fifos signals
	signal clock_125     : std_ulogic                    := '0';
	signal reset         : std_ulogic                    := '1';
	signal mii_tx_clk    : std_ulogic                    := '0';
	signal mii_tx_er     : std_ulogic                    := '0';
	signal mii_tx_en     : std_ulogic                    := '0';
	signal mii_txd       : std_ulogic_vector(7 downto 0) := (others => '0');
	signal mii_rx_clk    : std_ulogic                    := '0';
	signal mii_rx_er     : std_ulogic                    := '0';
	signal mii_rx_dv     : std_ulogic                    := '0';
	signal mii_rxd       : std_ulogic_vector(7 downto 0) := (others => '0');
	signal gmii_gtx_clk  : std_ulogic                    := '0';
	signal user_clock    : std_ulogic                    := '0';
	signal rx_empty      : std_ulogic                    := '0';
	signal rx_rd_en      : std_ulogic                    := '0';
	signal rx_data       : t_ethernet_data               := (others => '0');
	signal tx_data       : t_ethernet_data               := (others => '0');
	signal tx_data_wr_en : std_ulogic                    := '0';
	signal tx_data_full  : std_ulogic                    := '0';
	signal link_up       : std_ulogic                    := '0';
	signal speed         : t_ethernet_speed              := (others => '0');

	-- Test configuration
	constant TEST_THOROUGH : boolean := FALSE;

	-- Testbench signals
	signal run : boolean := TRUE;

	constant MAX_PACKETS_IN_TRANSACTION : integer := 10;

	-- Data array length is a bit on the large side so we can send jumbo frames
	type t_packet_data is array (0 to 10000) of t_ethernet_data;
	--type t_packet_data is array (0 to 1050) of ethernet_data_t;
	--type t_packet_data is array (0 to 60) of ethernet_data_t;
	type t_packet_transaction is record
		valid : boolean;
		data  : t_packet_data;
		size  : integer;
	end record;
	type t_packet_buffer is array (0 to MAX_PACKETS_IN_TRANSACTION - 1) of t_packet_transaction;

	signal speed_override     : t_ethernet_speed := SPEED_1000MBPS;
	signal send_packet_req    : boolean          := FALSE;
	signal send_packet_ack    : boolean          := FALSE;
	signal send_corrupt_data  : boolean          := FALSE;
	signal send_packet_buffer : t_packet_buffer;

	signal receive_packet_req            : boolean := FALSE;
	signal receive_packet_ack            : boolean := FALSE;
	signal receive_packet_buffer         : t_packet_buffer;
	signal receive_packet_count_expected : integer := 0;

	signal mac_mirror_run : boolean := TRUE;

	-- Timing definitions
	constant clock_125_period : time := 8 ns;
	constant clock_25_period  : time := 40 ns;
	constant clock_2_5_period : time := 400 ns;
	constant mii_rx_setup     : time := 2 ns;
	constant mii_rx_hold      : time := 0 ns;

	-- Functions

	impure function mii_rx_clk_period return time is
	begin
		case speed_override is
			when SPEED_10MBPS =>
				return clock_2_5_period;
			when SPEED_100MBPS =>
				return clock_25_period;
			when others =>
				return clock_125_period;
		end case;
	end function;

	-- Compare two packet transactions
	function "="(left, right : in t_packet_transaction) return boolean is
	begin
		if left.valid /= right.valid then
			report "Transaction validity state mismatch" severity note;
			return FALSE;
		elsif left.valid = TRUE then
			-- Both are valid
			-- Check size
			if left.size /= right.size then
				report "Transaction size mismatch" severity note;
				return FALSE;
			end if;
			-- Check data
			for i in 0 to left.size - 1 loop
				if left.data(i) /= right.data(i) then
					report "Transaction data mismatch at index " & integer'image(i) severity note;
					return FALSE;
				end if;
			end loop;
			-- All good
			return TRUE;
		else
			-- Both are invalid, no further check necessary
			-- Data does not matter
			return TRUE;
		end if;
	end function;

	-- Compare two packet transaction buffers
	function "="(left, right : in t_packet_buffer) return boolean is
	begin
		for i in t_packet_buffer'range loop
			-- Stop when both elements are invalid (end reached)
			exit when (not left(i).valid) and (not right(i).valid);
			-- Compare elements
			if not (left(i) = right(i)) then
				report "Mismatch in buffer element " & integer'image(i) severity note;
				return FALSE;
			end if;
		end loop;
		return TRUE;
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

begin
	-- Be aware of simulation mismatch because of delta-delay issues here
	user_clock <= clock_125;

	-- Instantiate component
	ethernet_mac_inst : entity work.ethernet_with_fifos --work.test_wrapper_spartan6 
		port map(
			clock_125_i      => clock_125,
			reset_i          => reset,
			mii_tx_clk_i     => mii_tx_clk,
			mii_tx_er_o      => mii_tx_er,
			mii_tx_en_o      => mii_tx_en,
			mii_txd_o        => mii_txd,
			mii_rx_clk_i     => mii_rx_clk,
			mii_rx_er_i      => mii_rx_er,
			mii_rx_dv_i      => mii_rx_dv,
			mii_rxd_i        => mii_rxd,
			gmii_gtx_clk_o   => gmii_gtx_clk,
			rgmii_tx_ctl_o   => open,
			rgmii_rx_ctl_i   => '0',
			miim_clock_i     => clock_125,
			mdc_o            => open,
			mdio_io          => open,
			rx_clock_i       => user_clock,
			rx_empty_o       => rx_empty,
			rx_rd_en_i       => rx_rd_en,
			rx_data_o        => rx_data,
			tx_clock_i       => user_clock,
			tx_data_i        => tx_data,
			tx_wr_en_i       => tx_data_wr_en,
			tx_full_o        => tx_data_full,
			link_up_o        => link_up,
			speed_o          => speed,
			speed_override_i => speed_override
		);

	-- Generate clocks
	clock_125_process : process
	begin
		if not run then
			wait until run;
		end if;
		clock_125 <= not clock_125;
		wait for clock_125_period / 2;
	end process;

	mii_tx_clk_process : process
	begin
		if not run then
			wait until run;
		end if;
		case speed_override is
			when SPEED_10MBPS =>
				mii_tx_clk <= not mii_tx_clk;
				wait for clock_2_5_period / 2;
			when SPEED_100MBPS =>
				mii_tx_clk <= not mii_tx_clk;
				wait for clock_25_period / 2;
			when others =>
				-- MII TX_CLK is inactive in 1 Gbps mode
				wait until ((speed_override = SPEED_10MBPS) or (speed_override = SPEED_100MBPS));
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
			-- Setup/hold time simulation is only useful in post-synthesis simulation
			--			mii_rx_clk <= '0';
			--			wait for (mii_rx_clk_period / 2) - mii_rx_setup;
			--			mii_rx_dv <= dv;
			--			mii_rx_er <= er;
			--			mii_rxd   <= data;
			--			wait for mii_rx_setup;
			--			mii_rx_clk <= '1';
			--			wait for mii_rx_hold;
			--			mii_rxd   <= (others => 'X');
			--			mii_rx_dv <= 'X';
			--			mii_rx_er <= 'X';
			--			wait for (mii_rx_clk_period / 2) - mii_rx_hold;
			mii_rx_clk <= '0';
			mii_rx_dv  <= dv;
			mii_rx_er  <= er;
			mii_rxd    <= data;
			wait for mii_rx_clk_period / 2;
			mii_rx_clk <= '1';
			wait for mii_rx_clk_period / 2;
		end procedure;

		procedure mii_rx_put(
			data : in std_ulogic_vector(7 downto 0) := "XXXXXXXX";
			dv   : in std_ulogic                    := '1';
			er   : in std_ulogic                    := '0') is
		begin
			if speed_override = SPEED_1000MBPS then
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

		variable fcs : std_ulogic_vector(31 downto 0);
	begin
		while not send_packet_req loop
			mii_rx_toggle;
			if not run then
				wait until run;
			end if;
		end loop;

		for packet_i in send_packet_buffer'range loop
			-- Stop at first invalid packet
			exit when not send_packet_buffer(packet_i).valid;

			-- Preamble
			for i in 0 to 3 loop
				mii_rx_put(PREAMBLE_DATA);
			end loop;
			-- SFD
			mii_rx_put(START_FRAME_DELIMITER_DATA);
			-- Data
			fcs := (others => '1');
			for i in 0 to send_packet_buffer(packet_i).size - 1 loop
				if send_corrupt_data then
					mii_rx_put(send_packet_buffer(packet_i).data(i) and "11011111");
				else
					mii_rx_put(send_packet_buffer(packet_i).data(i));
				end if;
				fcs := NEXTCRC32_D8(send_packet_buffer(packet_i).data(i), fcs);
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
		end loop;

		send_packet_ack <= TRUE;
		--report "Done sending" severity note;
		while send_packet_req loop
			mii_rx_toggle;
		end loop;
		send_packet_ack <= FALSE;
	end process;

	-- Process for mirroring packets from the RX FIFO to the TX FIFO
	-- Asynchronous to avoid complicated buffering on full/empty conditions
	fifo_mirror_process : process(rx_empty, tx_data_full, mac_mirror_run, rx_data)
	begin
		tx_data_wr_en <= '0' after 1 ns;
		rx_rd_en      <= '0' after 1 ns;
		tx_data       <= (others => '0') after 1 ns;
		if mac_mirror_run then
			tx_data       <= rx_data after 1 ns;
			tx_data_wr_en <= not rx_empty and not tx_data_full after 1 ns;
			rx_rd_en      <= not rx_empty and not tx_data_full after 1 ns;
		end if;
	end process;

	-- Process for reading the MII TX interface into a packet buffer
	packet_receive_process : process is
		variable current_byte : integer := 0;
		variable data         : t_ethernet_data;
		variable fcs          : std_ulogic_vector(31 downto 0);

		procedure wait_clk is
		begin
			case speed_override is
				when SPEED_10MBPS | SPEED_100MBPS =>
					wait until rising_edge(mii_tx_clk);
				when others =>
					wait until rising_edge(gmii_gtx_clk);
			end case;
			assert mii_tx_er = '0' report "MII transmission error flag is set" severity failure;
		end procedure;

		procedure read_byte(output_byte : out t_ethernet_data) is
		begin
			case speed_override is
				when SPEED_10MBPS | SPEED_100MBPS =>
					output_byte(3 downto 0) := mii_txd(3 downto 0);
					wait_clk;
					assert mii_tx_en = '1' report "Frame transmission ended between byte boundaries" severity failure;
					output_byte(7 downto 4) := mii_txd(3 downto 0);
					wait_clk;
				when others =>
					output_byte := mii_txd;
					wait_clk;
			end case;
		end procedure;

	begin
		wait until receive_packet_req;

		--report "Start receiver" severity note;

		for i in receive_packet_buffer'range loop
			receive_packet_buffer(i).valid <= FALSE;
		end loop;

		packet_loop : for current_packet_i in 0 to receive_packet_count_expected - 1 loop
			current_byte := 0;
			-- Wait for beginning of frame
			loop
				wait_clk;
				-- Allow receive cancellation
				exit packet_loop when not receive_packet_req;
				exit when mii_tx_en = '1';
			end loop;

			report "Start packet reception" severity note;

			for i in 0 to 6 loop
				read_byte(data);
				assert data = PREAMBLE_DATA and mii_tx_en = '1' report "Packet did not start with correct preamble data" severity failure;
			end loop;

			read_byte(data);
			assert data = START_FRAME_DELIMITER_DATA and mii_tx_en = '1' report "Packet did not start with correct preamble data or start frame delimiter" severity failure;

			fcs := (others => '1');

			loop
				read_byte(data);
				receive_packet_buffer(current_packet_i).data(current_byte) <= data;
				current_byte                                               := current_byte + 1;
				assert current_byte <= t_packet_data'high report "Transmitted packet is too long (size now " & integer'image(current_byte) & ")" severity failure;
				fcs := NEXTCRC32_D8(data, fcs);

				-- Exit after frame end
				exit when mii_tx_en = '0';
			end loop;

			-- Subtract FCS size
			current_byte := current_byte - CRC32_BYTES;
			assert current_byte >= MIN_FRAME_DATA_BYTES report "Transmitted packet is too short" severity failure;
			-- Check FCS
			assert fcs = CRC32_POSTINVERT_MAGIC report "FCS of transmitted packet did not match contents" severity failure;

			receive_packet_buffer(current_packet_i).size  <= current_byte;
			receive_packet_buffer(current_packet_i).valid <= TRUE;
			report "Received packet index " & integer'image(current_packet_i) & " size " & integer'image(current_byte) severity note;
		--assert current_packet_i < receive_packet_buffer'high report "Too many packets were transmitted";
		end loop;

		if receive_packet_req then
			receive_packet_ack <= TRUE;
			wait until not receive_packet_req;
			receive_packet_ack <= FALSE;
		end if;

	--report "Stop receiver" severity note;
	end process;

	-- Main test process
	test_process : process is
		-- Activate the sender and wait for it to complete
		procedure do_send is
		begin
			send_packet_req <= TRUE;
			wait until send_packet_ack;
			send_packet_req <= FALSE;
			wait until not send_packet_ack;
		end procedure;

		-- Activate the sender an receiver and wait for both to complete
		procedure do_send_receive is
		begin
			send_packet_req    <= TRUE;
			receive_packet_req <= TRUE;
			wait until send_packet_ack and receive_packet_ack;
			send_packet_req    <= FALSE;
			receive_packet_req <= FALSE;
			wait until (not send_packet_ack) and (not receive_packet_ack);
		end procedure;

		-- Send a packet, receive the mirror packet and check for equality
		procedure test_one_size(size : in integer) is
		begin
			report "Check single frame loopback size " & integer'image(size) severity note;
			send_packet_buffer(0).size <= size;
			do_send_receive;
			assert receive_packet_buffer = send_packet_buffer report "Packet loopback resulted in different packets" severity failure;
		end procedure;

		-- Send a packet, check that nothing comes back
		procedure test_send_broken is
			variable send_corrupt_data_backup : boolean;
		begin
			-- Enable receiver
			receive_packet_req <= TRUE;

			do_send;
			-- Give it some time to send the packet back if it went through
			wait for mii_rx_clk_period * 3000;
			-- Nothing should have been received
			assert not receive_packet_buffer(0).valid report "Invalid packet was answered by the MAC" severity failure;
			-- Now send an OK packet to check that it hasn't deadlocked somewhere
			send_corrupt_data_backup := send_corrupt_data;
			send_corrupt_data        <= FALSE;
			test_one_size(100);
			send_corrupt_data <= send_corrupt_data_backup;
		end procedure;

		procedure test_broken_size(size : in integer) is
		begin
			report "Check single broken frame is not looped back size " & integer'image(size) severity note;
			send_packet_buffer(0).size <= size;
			test_send_broken;
		end procedure;

		procedure test_one_speed is
		begin
			send_packet_buffer(0).valid   <= TRUE;
			send_packet_buffer(1).valid   <= FALSE;
			receive_packet_count_expected <= 1;

			if TRUE then
				-- Tests for packets that should pass
				if TEST_THOROUGH then
					for size in MIN_FRAME_DATA_BYTES to MAX_FRAME_DATA_BYTES loop
						test_one_size(size);
					end loop;
				else
					-- Test just a few sizes
					test_one_size(MIN_FRAME_DATA_BYTES);
					test_one_size(MIN_FRAME_DATA_BYTES + 1);
					test_one_size(1000);
					test_one_size(MAX_FRAME_DATA_BYTES - 1);
					test_one_size(MAX_FRAME_DATA_BYTES);
				end if;
			end if;

			-- Tests for packets that should get dropped
			if TRUE then
				if TEST_THOROUGH then
					-- Test runt frames
					for size in 1 to MIN_FRAME_DATA_BYTES - 1 loop
						test_broken_size(size);
					end loop;
					-- Test jumbo frames
					for size in MAX_FRAME_DATA_BYTES + 1 to MAX_FRAME_DATA_BYTES + 10 loop
						test_broken_size(size);
					end loop;
				else
					-- Test runt frames
					test_broken_size(1);
					test_broken_size(2);
					test_broken_size(3);
					test_broken_size(MIN_FRAME_DATA_BYTES - 2);
					test_broken_size(MIN_FRAME_DATA_BYTES - 1);
					-- Test jumbo frames
					test_broken_size(MAX_FRAME_DATA_BYTES + 1);
					test_broken_size(MAX_FRAME_DATA_BYTES + 2);
				end if;
				-- Test size that overflows 11 bits of size information in FIFOs
				test_broken_size(2 ** 11 + 1);
				-- Test size that is greater than total RX buffer size
				test_broken_size(9999);
				-- Test wrong FCS
				report "Check single broken frame is not looped back size 100 bad FCS" severity note;
				send_packet_buffer(0).size <= 100;
				send_corrupt_data          <= TRUE;
				test_send_broken;
				send_corrupt_data  <= FALSE;
				-- Disable receiver
				receive_packet_req <= FALSE;
				wait for mii_rx_clk_period * 2;
			end if;

			-- Check RX FIFO overflow
			for i in 0 to 7 loop
				send_packet_buffer(i).valid <= TRUE;
				send_packet_buffer(i).size  <= 1024;
			end loop;
			-- Suspend FIFO reader
			mac_mirror_run <= FALSE;

			report "Check RX FIFO overrun: Fill FIFO" severity note;
			-- Fill FIFO
			do_send;
			-- Enable receiver
			receive_packet_req            <= TRUE;
			-- One more than really expected (3)
			receive_packet_count_expected <= 4;
			-- Resume FIFO reader
			mac_mirror_run                <= TRUE;
			report "Check RX FIFO overrun: Receive mirrored packets" severity note;
			-- Wait for 3rd packet received
			wait until receive_packet_buffer(2).valid;
			-- Give it some more time to potentially receive another packet
			wait for mii_rx_clk_period * 3000;
			assert not receive_packet_ack report "Too many packets were received" severity failure;
			-- Disable receiver
			receive_packet_req <= FALSE;
			wait for mii_rx_clk_period * 2;
			-- Validate packets that went through
			for i in 0 to 2 loop
				assert receive_packet_buffer(i) = send_packet_buffer(i) report "Packet loopback resulted in different packets" severity failure;
			end loop;
			-- Check that normal reception is now working again
			receive_packet_count_expected <= 1;
			send_packet_buffer(1).valid   <= FALSE;
			test_one_size(100);
		-- TODO: Check for correct FIFO function when it is filled up exactly to the last byte
		end procedure;
	begin
		report "Hello" severity note;
		reset          <= '1';
		speed_override <= SPEED_1000MBPS;
		wait for 1000 ns;
		reset <= '0';
		wait for 1000 ns;

		for packet_i in send_packet_buffer'range loop
			for i in t_packet_data'range loop
				send_packet_buffer(packet_i).data(i) <= std_ulogic_vector("1" & to_unsigned((i + 7 + packet_i) mod 128, 7));
			end loop;
		end loop;

		report "Testing speed: 1 Gbps" severity note;
		test_one_speed;

		speed_override <= SPEED_100MBPS;
		wait for 10 us;
		report "Testing speed: 100 Mbps" severity note;
		test_one_speed;

		speed_override <= SPEED_10MBPS;
		wait for 10 us;
		report "Testing speed: 10 Mbps" severity note;
		test_one_speed;

		report "MAC functional check ended OK" severity note;
		-- Stop simulation
		run <= FALSE;
		wait;
	end process;

	-- Detect when the MAC is not answering
	watchdog : process is
	begin
		wait for 1 ms;
		if not run then
			wait until run;
		end if;
		if receive_packet_req and receive_packet_req'last_event > 20 ms then
			report "Expected number of frames could not be received within specified timeframe" severity failure;
		end if;
	end process;

end architecture;
