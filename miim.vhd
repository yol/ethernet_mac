-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

library ieee;
use ieee.std_logic_1164.all;

library ethernet_mac;
use ethernet_mac.miim_types.all;
use ethernet_mac.utility.all;

-- MII Management Interface compliant to IEEE 802.3 clause 22
entity miim is
	generic(
		-- Resulting clock frequency fclock / clock_divider has to be below 2.5 MHz for IEEE conformance
		-- Use only even numbers for 50% duty cycle of MDC
		clock_divider : integer range 8 to 1000 := 50
	);
	port(
		reset_i            : in    std_ulogic;
		clock_i            : in    std_ulogic;

		register_address_i : in    register_address_t;
		phy_address_i      : in    phy_address_t := (others => '0');
		data_read_o        : out   data_t;
		data_write_i       : in    data_t;
		req_i              : in    std_ulogic;
		ack_o              : out   std_ulogic;
		we_i               : in    std_ulogic;

		mdc_o              : out   std_ulogic;
		mdio_io            : inout std_ulogic
	);
end entity;

architecture rtl of miim is
	type miim_txrx_state is (
		IDLE_STATE,
		TX_COMMAND_STATE,
		RX_TURNAROUND_Z_STATE,
		RX_TURNAROUND_Z_READLOW_STATE,
		RX_DATA_STATE,
		TX_TURNAROUND_HIGH_STATE,
		TX_TURNAROUND_LOW_STATE,
		TX_DATA_STATE,
		DONE_STATE
	);

	signal state : miim_txrx_state;

	subtype operation_type_t is std_ulogic_vector(1 downto 0);

	constant PREAMBLE_LENGTH : natural                       := 32;
	-- The frame format as described in IEEE 802.3 clause 22.2.4.5 is LSB first, so the constants appear reversed here
	constant START_OF_FRAME  : std_ulogic_vector(1 downto 0) := "10";
	constant OPERATION_READ  : operation_type_t              := "01";
	constant OPERATION_WRITE : operation_type_t              := "10";
	constant COMMAND_LENGTH  : natural                       := PREAMBLE_LENGTH + START_OF_FRAME'length + operation_type_t'length + phy_address_t'length + register_address_t'length;

	signal operation_code       : operation_type_t;
	signal command              : std_ulogic_vector(COMMAND_LENGTH - 1 downto 0);
	signal command_bit_position : integer range 0 to COMMAND_LENGTH;
	signal data_bit_position    : integer range 0 to data_t'length;

	signal clock_divide_counter : integer range 0 to clock_divider;

-- Bit order:
--  PHYAD/REGAD/DATA: MSB first

begin

	-- Disable clock when idle, apply division otherwise
	mdc_o <= '1' when ((state /= IDLE_STATE) and (state /= DONE_STATE) and clock_divide_counter >= (clock_divider / 2)) else '0';

	with we_i select operation_code <=
		OPERATION_WRITE when '1',
		OPERATION_READ when others;

	-- Build command data array
	command(PREAMBLE_LENGTH - 1 downto 0)           <= (others => '1');
	command(command'high(1) downto PREAMBLE_LENGTH) <= reverse_vector(std_ulogic_vector(register_address_i)) & reverse_vector(std_ulogic_vector(phy_address_i)) & operation_code & START_OF_FRAME;

	output : process(state, command_bit_position, data_bit_position, command, data_write_i) is
	begin
		ack_o   <= '0';
		mdio_io <= 'Z';
		case state is
			when IDLE_STATE =>
				null;
			when TX_COMMAND_STATE =>
				mdio_io <= command(command_bit_position);
			when RX_TURNAROUND_Z_STATE =>
				null;
			when RX_TURNAROUND_Z_READLOW_STATE =>
				null;
			when RX_DATA_STATE =>
				null;
			when TX_TURNAROUND_HIGH_STATE =>
				mdio_io <= '1';
			when TX_TURNAROUND_LOW_STATE =>
				mdio_io <= '0';
			when TX_DATA_STATE =>
				mdio_io <= data_write_i(data_bit_position);
			when DONE_STATE =>
				ack_o <= '1';
		end case;
	end process;

	rx : process(clock_i) is
	begin
		-- Synchronize to rising as in the FSM process
		if rising_edge(clock_i) then
			-- and read just before rising (divided) MDC edge
			-- / 2 - 1
			if state = RX_DATA_STATE and (clock_divide_counter = (clock_divider / 4)) then
				data_read_o(data_bit_position) <= mdio_io;
			end if;
		end if;
	end process;

	fsm : process(clock_i) is
	begin
		if rising_edge(clock_i) then
			if reset_i = '1' then
				state                <= IDLE_STATE;
				clock_divide_counter <= 0;
			else
				if clock_divide_counter = clock_divider - 1 then
					clock_divide_counter <= 0;
				else
					clock_divide_counter <= clock_divide_counter + 1;
				end if;

				-- Run the FSM on the falling divided MDC edge
				if (clock_divide_counter = 0) then
					case state is
						when IDLE_STATE =>
							command_bit_position <= 0;
							-- start at MSB
							data_bit_position    <= data_t'length - 1;

							if req_i = '1' then
								state <= TX_COMMAND_STATE;
							end if;

						when TX_COMMAND_STATE =>
							command_bit_position <= command_bit_position + 1;
							if command_bit_position = COMMAND_LENGTH - 1 then
								if we_i = '0' then
									state <= RX_TURNAROUND_Z_STATE;
								else
									state <= TX_TURNAROUND_HIGH_STATE;
								end if;
							end if;

						when RX_TURNAROUND_Z_STATE =>
							state <= RX_TURNAROUND_Z_READLOW_STATE;
						when RX_TURNAROUND_Z_READLOW_STATE =>
							state <= RX_DATA_STATE;
						when TX_TURNAROUND_HIGH_STATE =>
							state <= TX_TURNAROUND_LOW_STATE;
						when TX_TURNAROUND_LOW_STATE =>
							state <= TX_DATA_STATE;

						when RX_DATA_STATE | TX_DATA_STATE =>
							data_bit_position <= data_bit_position - 1;
							if data_bit_position = 0 then
								state <= DONE_STATE;
							end if;

						when DONE_STATE =>
							if req_i = '0' then
								state <= IDLE_STATE;
							end if;
					end case;
				end if;
			end if;
		end if;
	end process;

end architecture;
