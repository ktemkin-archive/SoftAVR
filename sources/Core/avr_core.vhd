--************************************************************************************************
--  AVR CPU Core
--  (ATMega103 compatible.)
--
--  Authors:
--      -- Kyle J. Temkin, Binghamton University, <ktemkin@binghamton.edu>
--      -- Ruslan Leptenok, original core designer
--************************************************************************************************

library IEEE;
use IEEE.std_logic_1164.all;

use Work.AVR_Core_CompPack.all;
use WORK.AVRucPackage.all;

entity AVR_Core is port(
  --Clock and reset
  cp2         : in  std_logic;
  cp2en       : in  std_logic;
  ireset      : in  std_logic;
  -- JTAG OCD support
  valid_instr : out std_logic;
  insert_nop  : in  std_logic; 
  block_irq   : in  std_logic;
  change_flow : out std_logic;
  -- Program Memory
  pc          : out std_logic_vector(15 downto 0);   
  inst        : in  std_logic_vector(15 downto 0);
  -- I/O control
  adr         : buffer std_logic_vector(15 downto 0); 	
  iore        : buffer std_logic;                       
  iowe        : buffer std_logic;						
  -- Data memory control
  ramadr      : out std_logic_vector(15 downto 0);
  ramre       : out std_logic;
  ramwe       : out std_logic;
  cpuwait     : in  std_logic;
  -- Data paths
  dbusin      : in  std_logic_vector(7 downto 0);
  dbusout     : buffer std_logic_vector(7 downto 0);
  -- Interrupt
  irqlines    : in  std_logic_vector(22 downto 0);
  irqack      : out std_logic;
  irqackad    : out std_logic_vector(4 downto 0);
  --Sleep Control
  sleepi	    : out std_logic;
  irqok	    : out std_logic;
  globint	    : out std_logic;
  --Watchdog
  wdri	    : out std_logic
);
end AVR_Core;

architecture Struct of avr_core is

  signal dbusin_int  : std_logic_vector(7 downto 0);

  -- SIGNALS FOR INSTRUCTION AND STATES
  signal active_operation : decoded_operation;
  signal adiw_st 	: std_logic;
  signal sbiw_st 	: std_logic;

  signal alu_data_r_in : std_logic_vector(7 downto 0);
  signal alu_data_out  : std_logic_vector(7 downto 0);

  signal reg_rd_in     : std_logic_vector(7 downto 0);
  signal reg_rd_out    : std_logic_vector(7 downto 0);
  signal reg_rr_out    : std_logic_vector(7 downto 0);

  signal reg_rd_adr    : std_logic_vector(4 downto 0);
  signal reg_rr_adr    : std_logic_vector(4 downto 0);

  signal reg_h_out     : std_logic_vector(15 downto 0);
  signal reg_z_out     : std_logic_vector(15 downto 0);

  signal reg_h_adr     : std_logic_vector(2 downto 0);

  signal reg_rd_wr     : std_logic;
  signal post_inc      : std_logic;
  signal pre_dec       : std_logic;
  signal reg_h_wr      : std_logic;

  signal sreg_fl_in    : std_logic_vector(7 downto 0);
  signal sreg_out      : std_logic_vector(7 downto 0);
  signal sreg_fl_wr_en : std_logic_vector(7 downto 0);
  signal spl_out       : std_logic_vector(7 downto 0);
  signal sph_out       : std_logic_vector(7 downto 0);
  signal rampz_out     : std_logic_vector(7 downto 0);
       
  signal sp_ndown_up   : std_logic;
  signal sp_en         : std_logic;

  signal bit_num_r_io  : std_logic_vector(2 downto 0);
  signal branch        : std_logic_vector(2 downto 0);

  signal bitpr_io_out    : std_logic_vector(7 downto 0);
  signal bit_pr_sreg_out : std_logic_vector(7 downto 0);
  signal sreg_flags      : std_logic_vector(7 downto 0);
  signal bld_op_out      : std_logic_vector(7 downto 0);
  signal reg_file_rd_in  : std_logic_vector(7 downto 0);

  signal bit_test_op_out : std_logic;

  signal alu_flags_out   : flag_set;

  signal sbi_st : std_logic;
  signal cbi_st : std_logic;
 
begin

FETCH_DECODER:
component pm_fetch_dec port map(
  -- Clock and reset
  cp2      => cp2,
  cp2en    => cp2en,
  ireset   => ireset,
  -- JTAG OCD support
  valid_instr => valid_instr,
  insert_nop  => insert_nop,
  block_irq   => block_irq,
  change_flow => change_flow,
  -- Program memory
  pc       => pc,    
  inst     => inst,
  -- I/O control
  adr      => adr,
  iore     => iore,
  iowe     => iowe,
  -- Data memory control
  ramadr   => ramadr,
  ramre    => ramre,
  ramwe    => ramwe,
  cpuwait  => cpuwait,
  -- Data paths
  dbusin   => dbusin_int,
  dbusout  => dbusout,
  -- Interrupt
  irqlines => irqlines,
  irqack   => irqack,
  irqackad => irqackad,
  --Sleep 
  sleepi	 => sleepi,
  irqok	 => irqok,
  --Watchdog
  wdri	 => wdri,
  -- ALU interface(Data inputs)
  alu_data_r_in   => alu_data_r_in,

  -- ALU interface(Instruction inputs)

  adiw_st_out  => adiw_st,
  sbiw_st_out  => sbiw_st,

  -- ALU interface(Data output)
  alu_data_out => alu_data_out,

  -- ALU interface(Flag outputs)
  alu_flags_out => alu_flags_out,

  active_operation => active_operation,

  -- General purpose register file interface
  reg_rd_in   => reg_rd_in,
  reg_rd_out  => reg_rd_out,
  reg_rd_adr  => reg_rd_adr,
  reg_rr_out  => reg_rr_out,
  reg_rr_adr  => reg_rr_adr,
  reg_rd_wr   => reg_rd_wr,

  post_inc    => post_inc,
  pre_dec     => pre_dec,
  reg_h_wr    => reg_h_wr,
  reg_h_out   => reg_h_out,
  reg_h_adr   => reg_h_adr,
  reg_z_out   => reg_z_out,
  -- I/O register file interface
  sreg_fl_in    => sreg_fl_in, --??   
  globint       => sreg_out(7), -- SREG I flag   

  sreg_fl_wr_en => sreg_fl_wr_en,

  spl_out       => spl_out,       
  sph_out       => sph_out,       
  sp_ndown_up   => sp_ndown_up,
  sp_en         => sp_en,

  rampz_out     => rampz_out,
  -- Bit processor interface
  bit_num_r_io    => bit_num_r_io,  
  bitpr_io_out    => bitpr_io_out, 
  branch          => branch, 

  bit_pr_sreg_out => bit_pr_sreg_out,
  bld_op_out      => bld_op_out, 
  bit_test_op_out => bit_test_op_out,

  sbi_st_out   => sbi_st,
  cbi_st_out   => cbi_st
);


REGISTERS:
component reg_file port map (
  --Clock and reset
  cp2         => cp2,
  cp2en       => cp2en,
  ireset      => ireset,

  reg_rd_in   => reg_rd_in,
  reg_rd_out  => reg_rd_out,
  reg_rd_adr  => reg_rd_adr,
  reg_rr_out  => reg_rr_out,
  reg_rr_adr  => reg_rr_adr,
  reg_rd_wr   => reg_rd_wr,

  post_inc    => post_inc,
  pre_dec     => pre_dec,
  reg_h_wr    => reg_h_wr,
  reg_h_out   => reg_h_out,
  reg_h_adr   => reg_h_adr,
  reg_z_out   => reg_z_out
);


BIT_ALU:
component bit_processor port map(

  operation => active_operation,

  --Clock and reset
  cp2         => cp2,
  cp2en    => cp2en,
  ireset      => ireset, 

  bit_num_r_io  => bit_num_r_io,  
  dbusin        => dbusin_int,   
  bitpr_io_out  => bitpr_io_out,   

  sreg_out      => sreg_out,   
  branch   => branch,  

  bit_pr_sreg_out => bit_pr_sreg_out,

  bld_op_out      => bld_op_out,
  reg_rd_out      => reg_rd_out,

  bit_test_op_out => bit_test_op_out,

  -- Instructions and states
  sbi_st   => sbi_st,       
  cbi_st   => cbi_st
);                      


io_dec_Inst:
component io_adr_dec port map (
  adr          => adr,
  iore         => iore,
  dbusin_int   => dbusin_int,			-- LOCAL DATA BUS OUTPUT
  dbusin_ext   => dbusin,               -- EXTERNAL DATA BUS INPUT

  spl_out      => spl_out,
  sph_out      => sph_out,
  sreg_out     => sreg_out,
  rampz_out    => rampz_out
);

IORegs_Inst: 
component io_reg_file port map(

  --Clock and reset
  cp2        => cp2,
  cp2en    => cp2en,
  ireset     => ireset,     

  adr        => adr,       
  iowe       => iowe,
  dbusout    => dbusout,     

  sreg_fl_in => sreg_fl_in,
  sreg_out   => sreg_out,

  sreg_fl_wr_en => sreg_fl_wr_en,

  spl_out    => spl_out,    
  sph_out    => sph_out,    
  sp_ndown_up => sp_ndown_up, 
  sp_en      => sp_en,   

  rampz_out  => rampz_out
);


ALU:
component alu_avr port map(

  operation => active_operation,
  
  -- Data inputs
  alu_data_r_in => alu_data_r_in,
  alu_data_d_in => reg_rd_out,

  alu_c_flag_in => sreg_out(0),
  alu_z_flag_in => sreg_out(1),

  adiw_st  => adiw_st,     
  sbiw_st  => sbiw_st,     

  -- Data outputs
  alu_data_out => alu_data_out,  

  -- Flag outputs
  flags_out => alu_flags_out
);

-- Sleep support
globint	<= sreg_out(7); -- I flag

end Struct;
