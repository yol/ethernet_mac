# Set ISE_DIR to ISE installation directory. Example: .../14.7/ISE_DS/ISE

OPTS=--std=93c -g --ieee=synopsys -fexplicit --warn-no-vital-generic
# --no-vital-checks

all:
	ghdl -i --work=ethernet_mac --workdir=ghdl $(OPTS) *.vhd xilinx/*.vhd xilinx/ipcore_dir/ethernet_mac_tx_fifo_xilinx.vhd
	ghdl -m --work=ethernet_mac --workdir=ghdl -Pghdl/unisim -Pghdl/xilinxcorelib $(OPTS) ethernet_mac_tb
	
check: all
	./ethernet_mac_tb --stack-max-size=20M --ieee-asserts=disable
	
prepare:
	mkdir -p ghdl/unisim ghdl/xilinxcorelib #ghdl/simprim
	ghdl -i --work=unisim --workdir=ghdl/unisim $(OPTS) $(ISE_DIR)/vhdl/src/unisims/*.vhd
	ghdl -i --work=unisim --workdir=ghdl/unisim $(OPTS) $(ISE_DIR)/vhdl/src/unisims/primitive/*.vhd
	ghdl -i --work=XilinxCoreLib --workdir=ghdl/xilinxcorelib $(OPTS) $(ISE_DIR)/vhdl/src/XilinxCoreLib/*.vhd
	#ghdl -i --work=simprim --workdir=ghdl/simprim $(OPTS) $(ISE_DIR)/vhdl/src/simprims/simprim_Vcomponents.vhd $(ISE_DIR)/vhdl/src/simprims/simprim_Vpackage.vhd $(ISE_DIR)/vhdl/src/simprims/primitive/other/*.vhd
