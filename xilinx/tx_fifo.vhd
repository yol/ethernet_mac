library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ethernet_mac;
use ethernet_mac.ethernet_types.all;
use ethernet_mac.fifo_types.all;

entity tx_fifo is
	port(
		clock_i            : in  std_ulogic;
		reset_i            : in  std_ulogic;

		data_i             : in  ethernet_data_t;
		data_wr_en_i       : in  std_ulogic;
		data_full_o        : out std_ulogic;

		--size_i             : in  tx_size_fifo_data_t;
		--size_wr_en_i       : in  std_ulogic;
		--size_full_o        : out std_ulogic;

		-- Interface to framing layer
		mac_tx_clock_i     : in  std_ulogic;
		mac_tx_enable_o    : out std_ulogic;
		mac_tx_data_o      : out ethernet_data_t;
		mac_tx_byte_sent_i : in  std_ulogic;
		mac_tx_busy_i      : in  std_ulogic
	);
end entity tx_fifo;

architecture RTL of tx_fifo is
	signal data_rd_en : std_ulogic := '0';
	signal data_out   : std_logic_vector(ethernet_data_t'range);
	signal data_empty : std_ulogic := '0';
	signal data_read_count : std_logic_vector(11 downto 0);

	--signal size_rd_en : std_ulogic := '0';
	--signal size_out   : std_logic_vector(tx_size_fifo_data_t'range);
	--signal size_empty : std_ulogic := '0';

begin
	tx_data_fifo_inst : entity work.tx_data_fifo
		port map(
			rst    => reset_i,
			wr_clk => clock_i,
			rd_clk => mac_tx_clock_i,
			din    => std_logic_vector(data_i),
			wr_en  => data_wr_en_i,
			rd_en  => data_rd_en,
			dout   => data_out,
			full   => data_full_o,
			empty  => data_empty,
			rd_data_count => data_read_count
			--empty  => open
		);

--	tx_size_fifo_inst : entity work.tx_size_fifo
--		port map(
--			rst    => reset_i,
--			wr_clk => clock_i,
--			rd_clk => mac_tx_clock_i,
--			din    => std_logic_vector(size_i),
--			wr_en  => size_wr_en_i,
--			rd_en  => size_rd_en,
--			dout   => size_out,
--			full   => size_full_o,
--			empty  => size_empty
--		);

	tx_fifo_adapter_inst : entity ethernet_mac.tx_fifo_adapter
		port map(
			reset_i            => reset_i,
			mac_tx_clock_i     => mac_tx_clock_i,
			mac_tx_enable_o    => mac_tx_enable_o,
			mac_tx_data_o      => mac_tx_data_o,
			mac_tx_byte_sent_i => mac_tx_byte_sent_i,
			mac_tx_busy_i      => mac_tx_busy_i,
			data_rd_en_o       => data_rd_en,
			data_i             => std_ulogic_vector(data_out),
			data_empty_i       => data_empty,
			data_read_count_i  => unsigned(data_read_count)
--			size_rd_en_o       => size_rd_en,
--			size_i             => std_ulogic_vector(size_out),
--			size_empty_i       => size_empty
		);

end architecture RTL;
