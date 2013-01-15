--************************************************************************************************
-- Educational Soft-core AVR-8 Microcontroller for the Basys2 Board
-- Version 0.5 (Version for Xilinx)
--
-- The most recent version can be found at: http://www.github.com/ktemkin/SoftAVR/
--
-- Authors include:
--     -Kyle J. Temkin, Binghamton University, <ktemkin@binghamton.edu>
--     -Jack Gassett, Gadget Factory, http://www.gadgetfactory.net
--     -Ruslan Leptenok, original core deisgner
--
--************************************************************************************************

--
-- Use the standard IEEE logic library, as standardized in the IEEE-1164 standard.
-- (Source loation: <Your Xilinx directory>/ISE_DS/ISE/vhdl/src/ieee/std_logic_1164.vhd)
--
library IEEE;
use IEEE.std_logic_1164.all;

--
-- Use the local packages, which are provided with this source code.
-- 
use WORK.AVRuCPackage.all;
use WORK.AVR_uC_CompPack.all;
use WORK.SynthCtrlPack.all; -- Synthesis control
use WORK.XMemCompPack.all;  -- Xilinx RAM components  
use WORK.MemAccessCtrlPack.all;
use WORK.MemAccessCompPack.all;

--
-- This "entity" defines our microcontroller's interface with the outside world-
-- listing each of the device's inputs and outputs. 
--
-- It works very much like a schematic symbol in the ISE schematic tool.
-- 
entity basys_avr8 is port(

    --Active-low reset signal.
    --nrst   : in    std_logic;					

    --Board clock: this is the clock we recieve from outside of our FPGA.
    --In most cases, it will be a small 50MHz oscillator on the Basys board.
    board_clk    : in    std_logic;

    --AVR General-Purpose Inputs and Ouptuts (GPIO)
    --These allow our AVR microcontroller to interface with the outside world!
    port_a  : inout std_logic_vector(7 downto 0);
    port_b  : inout std_logic_vector(7 downto 0);
    port_c  : inout std_logic_vector(7 downto 0);
    port_d  : inout std_logic_vector(7 downto 0);
    port_e  : inout std_logic_vector(7 downto 0);
    port_f  : inout std_logic_vector(7 downto 0);

    --Universal Asynchronous Receiver/Transmitter (UART)
    --This is our microcontroller's serial port!
    rxd    : in    std_logic;
    txd    : out   std_logic
);
end basys_avr8;

--
-- This "architecture" defines the actual logic /content/ of our microcontroller.
--
-- Notice the "of basys_avr8", which tells us that this device provides the contents
-- of the 'basys_avr8' device. This works a lot like a schematic file in the ISE schematic tool.
--
architecture structure of basys_avr8 is

    --These "constants" allow us to easily configure the way the Xilinx tools
    --interpret our microcontroller code below. 
    constant CImplport_a			            : boolean := TRUE;
    constant CImplport_b			            : boolean := TRUE;
    constant CImplport_c						: boolean := TRUE;
    constant CImplport_d    			        : boolean := TRUE;
    constant CImplport_e      			    : boolean := TRUE;
    constant CImplport_f           			: boolean := TRUE;
    constant CImplUART      			    : boolean := TRUE;	--AVR8 UART peripheral
    constant CImplTmrCnt     				: boolean := TRUE;	--AVR8 Timer


    component XDM4Kx8
    port(
        cp2       : in  std_logic;
        ce      : in  std_logic;
        address   : in  std_logic_vector(CDATAMEMSIZE downto 0); 
        din       : in  std_logic_vector(7 downto 0);		                
        dout      : out std_logic_vector(7 downto 0);
        we        : in  std_logic
    );
    end component;

    component XPM8Kx16 
    port(
        cp2     : in  std_logic;
        ce      : in  std_logic;
        address : in  std_logic_vector(CPROGMEMSIZE downto 0); 
        din     : in  std_logic_vector(15 downto 0);		                
        dout    : out std_logic_vector(15 downto 0);
        we      : in  std_logic
    );
    end component;

    --
    -- Digital Clock Manager, 50MHz to 16MHz
    -- Generates the 16MHz system clock for our AVR.
    --
    component DCM50to16
    port(
        clkin_in : in std_logic;          
        clkfx_out : out std_logic;
        clkin_ibufg_out : out std_logic;
        clk0_out : out std_logic
    );
    end component;

-- ############################## Define Components for User Cores ##################################################


-- ###############################################################################################################

    -- ############################## Signals connected directly to the core ##########################################

    signal core_cpuwait  : std_logic;                    

    -- Program memory
    signal core_pc   : std_logic_vector (15 downto 0); -- PROM address
    signal core_inst : std_logic_vector (15 downto 0); -- PROM data

    -- I/O registers
    signal core_adr  : std_logic_vector (15 downto 0);
    signal core_iore : std_logic;                    
    signal core_iowe : std_logic;

    -- Data memery
    signal core_ramadr : std_logic_vector (15 downto 0);
    signal core_ramre  : std_logic;
    signal core_ramwe  : std_logic;

    signal core_dbusin   : std_logic_vector (7 downto 0);
    signal core_dbusout  : std_logic_vector (7 downto 0);

    -- Interrupts
    signal core_irqlines : std_logic_vector(22 downto 0);
    signal core_irqack   : std_logic;
    signal core_irqackad : std_logic_vector(4 downto 0);

    -- ###############################################################################################################

    -- ############################## Signals connected directly to the SRAM controller ###############################

    signal ram_din       : std_logic_vector(7 downto 0);

    -- ###############################################################################################################

    -- ####################### Signals connected directly to the external multiplexer ################################
    signal   io_port_out     : ext_mux_din_type;
    signal   io_port_out_en  : ext_mux_en_type;
    signal   ind_irq_ack     : std_logic_vector(core_irqlines'range);
    -- ###############################################################################################################

    -- ################################## Reset signals #############################################
    signal core_ireset        : std_logic;
    -- ##############################################################################################

    -- Port signals
    signal port_aReg : std_logic_vector(port_a'range);
    signal DDRAReg  : std_logic_vector(port_a'range);

    signal port_bReg : std_logic_vector(port_b'range);
    signal DDRBReg  : std_logic_vector(port_b'range);

    signal port_cReg : std_logic_vector(port_c'range);
    signal DDRCReg  : std_logic_vector(port_c'range);

    signal port_dReg : std_logic_vector(port_d'range);
    signal DDRDReg  : std_logic_vector(port_d'range);

    signal port_eReg : std_logic_vector(port_e'range);
    signal DDREReg  : std_logic_vector(port_e'range);

    signal port_fReg : std_logic_vector(port_f'range);
    signal DDRFReg  : std_logic_vector(port_f'range);

    -- Added for Synopsys compatibility
    signal gnd   : std_logic;
    signal vcc    : std_logic;

    -- Sleep support
    signal core_cp2  : std_logic; -- Global clock signal after gating(and global primitive)
    signal sleep_en  : std_logic;

    signal sleepi   : std_logic;
    signal irqok    : std_logic;
    signal globint  : std_logic;

    signal nrst_clksw : std_logic; -- Separate reset for clock gating module 

    -- Watchdog related signals
    signal wdtmout 	  : std_logic; -- Watchdog overflow
    signal core_wdri  : std_logic; -- Watchdog clear

    -- **********************  JTAG and memory **********************************************
    -- PM address,data and control
    signal pm_adr         : std_logic_vector(core_pc'range);
    signal pm_h_we        : std_logic;
    signal pm_l_we        : std_logic;
    signal pm_din         : std_logic_vector(core_inst'range);

    signal pm_dout        : std_logic_vector(core_inst'range);

    signal TDO_Out        : std_logic;
    signal TDO_OE         : std_logic;

    signal JTAG_Rst       : std_logic;

    -- **********************  JTAG and memory **********************************************

    signal nrst_cp64m_tmp   : std_logic;

    signal ram_cp2_n        : std_logic;

    signal sleep_mode       : std_logic; 

    -- "EEPROM" related signals
    signal EEPrgSel : std_logic; 
    signal EEAdr    : std_logic_vector(11 downto 0);
    signal EEWrData : std_logic_vector(7 downto 0);
    signal EERdData : std_logic_vector(7 downto 0);
    signal EEWr     : std_logic; 


    -- New
    signal busmin   : MastersOutBus_Type;                            
    signal busmwait : std_logic_vector(CNumOfBusMasters-1 downto 0) := (others => '0'); 

    signal slv_outs : SlavesOutBus_Type;

    signal ram_sel  : std_logic;

    -- UART DMA
    signal udma_mack    : std_logic;


    signal mem_mux_out   : std_logic_vector (7 downto 0);

    -- Place Holder Signals for JTAG instead of connecting them externally
    signal TRSTn         : std_logic;     
    signal TMS           : std_logic;     
    signal TCK           : std_logic;      
    signal TDI           : std_logic;
    signal TDO           : std_logic;

    -- AES

    signal aes_mack         : std_logic;        


    -- Address decoder
    signal stb_IO        : std_logic;   
    signal stb_IOmod     : std_logic_vector (CNumOfSlaves-1 downto 0);

    signal ram_ce      	 : std_logic;

    signal slv_cpuwait   : std_logic;

    -- Memory i/f
    signal mem_ramadr       : std_logic_vector (15 downto 0);  
    signal mem_ram_dbus_in  : std_logic_vector (7 downto 0);
    signal mem_ram_dbus_out : std_logic_vector (7 downto 0);
    signal mem_ramwe        : std_logic;
    signal mem_ramre        : std_logic;

    -- RAM
    signal ram_ramwe         : std_logic;

    -- Clock generation/distribution
    signal clk              : std_logic; 

    -- nrst
    signal nrst             : std_logic;  		--Comment this to connect reset to an external pushbutton.

    -- ############################## Signals connected directly to the I/O registers ################################
    -- port_a
    signal port_a_dbusout : std_logic_vector (7 downto 0);
    signal port_a_out_en  : std_logic;

    -- port_b
    signal port_b_dbusout : std_logic_vector (7 downto 0);
    signal port_b_out_en  : std_logic;

    -- port_c
    signal port_c_dbusout : std_logic_vector (7 downto 0);
    signal port_c_out_en  : std_logic;

    -- port_d
    signal port_d_dbusout : std_logic_vector (7 downto 0);
    signal port_d_out_en  : std_logic;

    -- port_e
    signal port_e_dbusout : std_logic_vector (7 downto 0);
    signal port_e_out_en  : std_logic;

    -- port_f
    signal port_f_dbusout : std_logic_vector (7 downto 0);
    signal port_f_out_en  : std_logic;


    -- Timer/Counter
    signal tc_dbusout    : std_logic_vector (7 downto 0);
    signal tc_out_en     : std_logic;

    -- UART
    signal uart_dbusout  : std_logic_vector (7 downto 0);
    signal uart_out_en   : std_logic;


    -- ###############################################################################################################

    -- ############################## Define Signals for User Cores ##################################################
    -- Example Core - - core9
    --signal core9_input_sig : std_logic_vector(1 downto 0);		--Define a signal for the inputs.
    signal core9_dbusout  : std_logic_vector (7 downto 0);
    signal core9_out_en   : std_logic;

    -- ###############################################################################################################

begin

    -- Added for Synopsys compatibility
    gnd <= '0';
    vcc  <= '1';
    -- Added for Synopsys compatibility	

    nrst <= '1';										--Comment this to connect reset to an external pushbutton.

	Inst_DCM50to16: DCM50to16 PORT MAP(
		CLKIN_IN => board_clk,
		CLKFX_OUT => clk,
		CLKIN_IBUFG_OUT => open,
		CLK0_OUT => open
	);

    core_inst <= pm_dout;





    -- Unused IRQ lines
    core_irqlines(7 downto 4) <= ( others => '0');
    core_irqlines(3 downto 0) <= ( others => '0');
    core_irqlines(13 downto 10) <= ( others => '0');
    core_irqlines(16) <= '0';
    core_irqlines(22 downto 20) <= ( others => '0');
    -- ************************

    -- Unused out_en
    io_port_out_en(10 to 15) <= (others => '0');
    io_port_out(10 to 15) <= (others => (others => '0'));

    AVR_Core_Inst:component AVR_Core port map(
        --Clock and reset
            cp2      => core_cp2,
            cp2en    => vcc,
            ireset   => core_ireset,
        -- JTAG OCD support
            valid_instr => open,
            insert_nop  => gnd,
            block_irq   => gnd,
            change_flow => open,
        -- Program Memory
            pc       => core_pc,
            inst     => core_inst,
        -- I/O control
            adr      => core_adr,
            iore     => core_iore,
            iowe     => core_iowe,
        -- Data memory control
            ramadr   => core_ramadr,
            ramre    => core_ramre,
            ramwe    => core_ramwe,
            cpuwait  => core_cpuwait,
        -- Data paths
            dbusin   => core_dbusin,
            dbusout  => core_dbusout,
        -- Interrupts
            irqlines => core_irqlines, 
            irqack   => core_irqack,
            irqackad => core_irqackad, 
        --Sleep Control
            sleepi   => sleepi,
            irqok	   => irqok,
            globint  => globint,
        --Watchdog
            wdri	   => core_wdri);
                                              

    RAM_Data_Register:component RAMDataReg port map(	                   
                   ireset      => core_ireset,
                   cp2	       => clk, -- clk,
                   cpuwait     => core_cpuwait,
                   RAMDataIn   => core_dbusout,
                   RAMDataOut  => ram_din
                             );



    EXT_MUX:component external_mux port map(
              ramre              => mem_ramre,		   -- ramre output of the arbiter and multiplexor
              dbus_out           => core_dbusin,       -- Data input of the core 
              ram_data_out       => mem_mux_out,       -- Data output of the RAM mux(RAM or memory located I/O)
              io_port_bus        => io_port_out,       -- Data outputs of the I/O
              io_port_en_bus     => io_port_out_en,    -- Out enable outputs of I/O
              irqack             => core_irqack,		  
              irqackad			 => core_irqackad,
              ind_irq_ack		 =>	ind_irq_ack		  -- Individual interrupt acknolege for the peripheral
                                                );


    -- ******************  port_a **************************				
    port_a_COMP:component gpio_port  
        generic map(port_number => 0)
        port map(
           clk	            => clk,
           reset_not        => core_ireset,
           bus_addr         => core_adr,
           bus_in           => core_dbusout,
           bus_out          => port_a_dbusout,
           read_enable      => core_iore,
           write_enable     => core_iowe,
           driving_bus      => port_a_out_en,
           output_port      => port_aReg,
           data_direction   => DDRAReg,
           input_pins       => port_a
        );

    -- port_a connection to the external multiplexer
    io_port_out(0) <= port_a_dbusout;
    io_port_out_en(0) <= port_a_out_en;

    -- Tri-state control for port_a
    port_aZCtrl:for i in port_a'range generate
        port_a(i) <= port_aReg(i) when DDRAReg(i)='1' else 'Z'; 	
    end generate;


    -- ******************  port_b **************************		
    port_b_Impl:if CImplport_b generate
    port_b_COMP:component gpio_port 
        generic map (port_number => 1)
        port map(
           clk	            => clk,
           reset_not        => core_ireset,
           bus_addr         => core_adr,
           bus_in           => core_dbusout,
           bus_out          => port_b_dbusout,
           read_enable      => core_iore,
           write_enable     => core_iowe,
           driving_bus      => port_b_out_en,
           output_port      => port_bReg,
           data_direction   => DDRBReg,
           input_pins       => port_b
       );

    -- port_b connection to the external multiplexer
    io_port_out(1) <= port_b_dbusout;
    io_port_out_en(1) <= port_b_out_en;

    -- Tri-state control for port_b
    port_bZCtrl:for i in port_b'range generate
    port_b(i) <= port_bReg(i) when DDRBReg(i)='1' else 'Z'; 	
    end generate;

    end generate;

    port_b_Not_Impl:if not CImplport_b generate
     port_b <= (others => 'Z');	
    end generate; 
        
    -- ************************************************

    -- ******************  port_c **************************				
    port_c_Impl:if CImplport_c generate
    port_c_COMP:component gpio_port  
        generic map(port_number => 2)
        port map(
           clk	            => clk,
           reset_not        => core_ireset,
           bus_addr         => core_adr,
           bus_in           => core_dbusout,
           bus_out          => port_c_dbusout,
           read_enable      => core_iore,
           write_enable     => core_iowe,
           driving_bus      => port_c_out_en,
           output_port      => port_cReg,
           data_direction   => DDRCReg,
           input_pins       => port_c
       );
    -- port_c connection to the external multiplexer
    io_port_out(5) <= port_c_dbusout;
    io_port_out_en(5) <= port_c_out_en;

    -- Tri-state control for port_c
    port_cZCtrl:for i in port_c'range generate
    port_c(i) <= port_cReg(i) when DDRCReg(i)='1' else 'Z'; 	
    end generate;

    end generate;

    port_c_Not_Impl:if not CImplport_c generate
     port_c <= (others => 'Z');	
    end generate; 

    -- ******************  port_d **************************		
    port_d_Impl:if CImplport_d generate
    port_d_COMP:component gpio_port 
        generic map (port_number => 3)
        port map(
           clk	            => clk,
           reset_not        => core_ireset,
           bus_addr         => core_adr,
           bus_in           => core_dbusout,
           bus_out          => port_d_dbusout,
           read_enable      => core_iore,
           write_enable     => core_iowe,
           driving_bus      => port_d_out_en,
           output_port      => port_dReg,
           data_direction   => DDRDReg,
           input_pins       => port_d
       );

    -- port_d connection to the external multiplexer
    io_port_out(6) <= port_d_dbusout;
    io_port_out_en(6) <= port_d_out_en;

    -- Tri-state control for port_d
    port_dZCtrl:for i in port_d'range generate
    port_d(i) <= port_dReg(i) when DDRDReg(i)='1' else 'Z'; 	
    end generate;

    end generate;

    port_d_Not_Impl:if not CImplport_d generate
     port_d <= (others => 'Z');	
    end generate; 
        
    -- ************************************************

    -- ******************  port_e **************************				
    port_e_Impl:if CImplport_e generate
    port_e_COMP:component gpio_port  
        generic map(port_number => 4)
        port map(
           clk	            => clk,
           reset_not        => core_ireset,
           bus_addr         => core_adr,
           bus_in           => core_dbusout,
           bus_out          => port_e_dbusout,
           read_enable      => core_iore,
           write_enable     => core_iowe,
           driving_bus      => port_e_out_en,
           output_port      => port_eReg,
           data_direction   => DDREReg,
           input_pins       => port_e
       );
    -- port_e connection to the external multiplexer
    io_port_out(7) <= port_e_dbusout;
    io_port_out_en(7) <= port_e_out_en;

    -- Tri-state control for port_e
    port_eZCtrl:for i in port_e'range generate
    port_e(i) <= port_eReg(i) when DDREReg(i)='1' else 'Z'; 	
    end generate;

    end generate;

    port_e_Not_Impl:if not CImplport_e generate
     port_e <= (others => 'Z');	
    end generate; 

    -- ******************  port_f **************************		
    port_f_Impl:if CImplport_f generate
    port_f_COMP:component gpio_port 
        generic map (port_number => 5)
        port map(
           clk	            => clk,
           reset_not        => core_ireset,
           bus_addr         => core_adr,
           bus_in           => core_dbusout,
           bus_out          => port_f_dbusout,
           read_enable      => core_iore,
           write_enable     => core_iowe,
           driving_bus      => port_f_out_en,
           output_port      => port_fReg,
           data_direction   => DDRFReg,
           input_pins       => port_f
       );

    -- port_f connection to the external multiplexer
    io_port_out(8) <= port_f_dbusout;
    io_port_out_en(8) <= port_f_out_en;

    -- Tri-state control for port_f
    port_fZCtrl:for i in port_f'range generate
        port_f(i) <= port_fReg(i) when DDRFReg(i)='1' else 'Z'; 	
    end generate;

    end generate;

    port_f_Not_Impl:if not CImplport_f generate
     port_f <= (others => 'Z');	
    end generate; 
        
    -- ************************************************




    --****************** Timer/Counter **************************
    TmrCnt_Impl:if CImplTmrCnt generate
    TmrCnt_Inst:component Timer_Counter port map(
                   -- AVR Control
                   ireset     => core_ireset,
                   cp2	      => clk, -- clk,
                   cp2en	  => vcc,
                   tmr_cp2en  => vcc,
                   stopped_mode   => gnd,
                   tmr_running    => gnd,
                   adr        => core_adr,
                   dbus_in    => core_dbusout,
                   dbus_out   => tc_dbusout, 
                   iore       => core_iore,
                   iowe       => core_iowe,
                   out_en     => tc_out_en,
                   -- External inputs/outputs
                   EXT1           => gnd,
                   EXT2           => gnd,
                   OC0_PWM0       => open,
                   OC1A_PWM1A     => open,
                   OC1B_PWM1B     => open,
                   OC2_PWM2       => open,
                   -- Interrupt related signals
                   TC0OvfIRQ      => core_irqlines(15),  -- Timer/Counter0 overflow ($0020)
                   TC0OvfIRQ_Ack  => ind_irq_ack(15),
                   TC0CmpIRQ      => core_irqlines(14),  -- Timer/Counter0 Compare Match ($001E)
                   TC0CmpIRQ_Ack  => ind_irq_ack(14),
                   TC2OvfIRQ      => core_irqlines(9),	-- Timer/Counter2 overflow ($0014)
                   TC2OvfIRQ_Ack  => ind_irq_ack(9),
                   TC2CmpIRQ      => core_irqlines(8),	-- Timer/Counter2 Compare Match ($0012)
                   TC2CmpIRQ_Ack  => ind_irq_ack(8),
                   TC1OvfIRQ      => open,
                   TC1OvfIRQ_Ack  => gnd,
                   TC1CmpAIRQ     => open,
                   TC1CmpAIRQ_Ack => gnd,
                   TC1CmpBIRQ     => open,
                   TC1CmpBIRQ_Ack => gnd,
                   TC1ICIRQ       => open,
                   TC1ICIRQ_Ack   => gnd);

    -- Timer/Counter connection to the external multiplexer							  
    io_port_out(4)    <= tc_dbusout;
    io_port_out_en(4) <= tc_out_en;
    end generate;

    -- Watchdog is not implemented
    wdtmout <= '0';


    -- Reset generator						 
    ResetGenerator_Inst:component ResetGenerator port map(
                                    -- Clock inputs
                                    cp2	       => clk, -- clk,
                                    cp64m	   => gnd,
                                    -- Reset inputs
                                    nrst       => nrst,
                                    npwrrst    => vcc,
                                    wdovf      => wdtmout,
                                    jtagrst    => JTAG_Rst,
                                    -- Reset outputs
                                    nrst_cp2   => core_ireset,
                                    nrst_cp64m => nrst_cp64m_tmp,
                                    nrst_clksw => nrst_clksw
                                    );

                               
    ClockGatingDis:if not CImplClockSw generate
     core_cp2 <=  clk;
    end generate;

    -- **********************  JTAG and memory **********************************************

    ram_cp2_n <= not clk;

    ---- Data memory(8-bit)					   
    DRAM_Inst:component XDM4Kx8 
    port map(
                            cp2       => ram_cp2_n,
                            ce        => vcc,
                            address   => mem_ramadr(CDATAMEMSIZE downto 0), 
                            din       => mem_ram_dbus_in, 
                            dout      => mem_ram_dbus_out, 
                            we        => ram_ramwe
                           );

    -- Program memory					   
    PM_Inst:component XPM8Kx16 
    port map(
                          cp2     => ram_cp2_n, 
                          ce      => vcc,
                          address => pm_adr(CPROGMEMSIZE downto 0),
                          din     => pm_din,
                          dout    => pm_dout,
                          we     => pm_l_we
                          );  
                                               
    -- **********************  JTAG and memory **********************************************

    -- Sleep mode is not implemented
    sleep_mode <= '0';


    JTAGOCDPrgTop_Inst:component JTAGOCDPrgTop port map(
                              -- AVR Control
                              ireset       => core_ireset,
                              cp2	       => core_cp2,
                              -- JTAG related inputs/outputs
                              TRSTn        => TRSTn, -- Optional
                              TMS          => TMS,
                              TCK	       => TCK,
                              TDI          => TDI,
                              TDO          => TDO_Out,
                              TDO_OE       => TDO_OE,
                              -- From the core
                              PC           => core_pc,
                              -- To the PM("Flash")
                              pm_adr       => pm_adr,
                              pm_h_we      => pm_h_we,
                              pm_l_we      => pm_l_we,
                              pm_dout      => pm_dout,
                              pm_din       => pm_din,
                              -- To the "EEPROM" 
                              EEPrgSel     => EEPrgSel,
                              EEAdr        => EEAdr,
                              EEWrData     => EEWrData,
                              EERdData     => EERdData,
                              EEWr         => EEWr,
                              -- CPU reset
                              jtag_rst     => JTAG_Rst
                              );

    -- JTAG OCD module connection to the external multiplexer
    io_port_out(3) <= (others => '0');
    io_port_out_en(3) <= gnd;						  
                              
    TDO <= TDO_Out when TDO_OE='1' else 'Z'; 						  

    -- *******************************************************************************************************	
    -- DMA, Memory decoder, ...
    -- *******************************************************************************************************	


    uart_Inst:component uart port map(
                        -- AVR Control
                        ireset     => core_ireset,
                        cp2	       => core_cp2,
                        adr        => core_adr,
                        dbus_in    => core_dbusout,
                        dbus_out   => uart_dbusout,
                        iore       => core_iore,
                        iowe       => core_iowe,
                        out_en     => uart_out_en,
                        -- UART
                        rxd        => rxd,
                        rx_en      => open,
                        txd        => txd,
                        tx_en      => open,
                        -- IRQ
                        txcirq     => core_irqlines(19),
                        txc_irqack => ind_irq_ack(19),
                        udreirq    => core_irqlines(18),
                        rxcirq     => core_irqlines(17)
                        );


    -- UART connection to the external multiplexer							  
    io_port_out(2)    <= uart_dbusout;
    io_port_out_en(2) <= uart_out_en;


    -- Arbiter and mux
    ArbiterAndMux_Inst:component ArbiterAndMux port map(
                            --Clock and reset
                            ireset      => core_ireset,
                            cp2         => core_cp2,
                            -- Bus masters
                            busmin		=> busmin,
                            busmwait	=> busmwait,
                            -- Memory Address,Data and Control
                            ramadr     => mem_ramadr,
                            ramdout    => mem_ram_dbus_in,
                            ramre      => mem_ramre,
                            ramwe      => mem_ramwe,
                            cpuwait    => slv_cpuwait
                            );

    -- cpuwait 
    slv_cpuwait <= '0';
                            
    -- Core connection						
    busmin(0).ramadr <= core_ramadr; 						
    busmin(0).dout   <=	ram_din; -- !!!
    busmin(0).ramre  <=	core_ramre;
    busmin(0).ramwe  <=	core_ramwe;				
    core_cpuwait     <=	busmwait(0);

    -- UART DMA connection						
    busmin(1).ramadr <= (others => '0'); 						
    busmin(1).dout   <=	(others => '0'); -- !!!
    busmin(1).ramre  <=	gnd;
    busmin(1).ramwe  <=	gnd;				
    udma_mack        <=  not busmwait(1);

    -- AES DMA connection
    busmin(2).ramadr <= (others => '0');		
    busmin(2).dout   <=	(others => '0');
    busmin(2).ramre  <=	gnd;
    busmin(2).ramwe  <=	gnd;
    aes_mack         <=  not busmwait(2);

    -- UART DMA slave part
    slv_outs(0).dout    <= (others => '0');
    slv_outs(0).out_en 	<= gnd;	

    -- AES DMA slave part
    slv_outs(1).dout    <= (others => '0');
    slv_outs(1).out_en 	<= gnd;	


    -- Memory read mux
    MemRdMux_inst:component MemRdMux port map(
                            slv_outs  =>  slv_outs,
                            ram_sel   =>  ram_sel,    -- Data RAM selection(optional input)
                            ram_dout  =>  mem_ram_dbus_out,            -- Data memory output (From RAM)
                            dout      =>  mem_mux_out -- Data output (To the core and other bus masters)
                            );



    -- Address decoder
    RAMAdrDcd_Inst:component RAMAdrDcd port map(
                             ramadr    => mem_ramadr, 
                             ramre     => mem_ramre,
                             ramwe     => mem_ramwe,
                             -- Memory mapped I/O i/f
                             stb_IO	   => stb_IO,
                             stb_IOmod => stb_IOmod,
                             -- Data memory i/f
                             ram_we    => ram_ramwe,
                             ram_ce    => ram_ce,
                             ram_sel   => ram_sel
                            );

end architecture;
