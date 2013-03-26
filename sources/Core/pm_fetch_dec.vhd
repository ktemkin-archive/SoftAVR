--************************************************************************************************
--  PM_FETCH_DEC(internal module) for AVR core
--	Version 2.6! (Special version for the JTAG OCD)
--  Designed by Ruslan Lepetenok 14.11.2001
--  Modified 31.05.06
--  Modification:
--  Registered ramre/ramwe outputs
--  cpu_busy logic modified(affects RCALL/ICALL/CALL instruction interract with interrupt)
--  SLEEP and CLRWDT instructions support was added
--  V-flag bug fixed (AND/ANDI/OR/ORI/EOR)
--  V-flag bug fixed (ADIW/SBIW)
--  Unused outputs(sreg_bit_num[2..0],active_operation.is_sbi_out,active_operation.is_cbi_out,active_operation.is_bld_out) were removed.
--  Output alu_data_d_in[7..0] was removed.
--  Gloabal clock enable(cp2en) was added  
--  cpu_busy(push/pop) + irq bug was fixed 14.07.05
--  BRXX+IRQ interaction was modified -> cpu_busy
--  LDS/STS now requires only two cycles for execution (13.01.06 -> last modificatioon)
--************************************************************************************************

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;

use work.AVRuCPackage.all;

entity pm_fetch_dec is port(
  -- Clock and reset
  cp2              : in  std_logic;
  cp2en            : in  std_logic; 
  ireset           : in  std_logic;

  -- JTAG OCD support
  valid_instr      : out  std_logic;
  insert_nop       : in   std_logic; 
  block_irq        : in   std_logic;
  change_flow      : out  std_logic;

  -- Program memory
  pc               : out program_address;
  inst             : in  std_logic_vector (15 downto 0);

  -- I/O control
  adr              : out std_logic_vector (15 downto 0); 	
  iore             : out std_logic;                       
  iowe             : out std_logic;						

  -- Data memory control
  ramadr           : out std_logic_vector (15 downto 0);
  ramre            : out std_logic;
  ramwe            : out std_logic;
  cpuwait          : in  std_logic;

  -- Data paths
  dbusin           : in  std_logic_vector (7 downto 0);
  dbusout          : out std_logic_vector (7 downto 0);

  -- Interrupt
  irqlines         : in  std_logic_vector (22 downto 0);
  irqack           : out std_logic;
  irqackad         : out std_logic_vector(4 downto 0);

  --Sleep 
  sleepi	         : out std_logic;
  irqok	           : out std_logic;

  --Watchdog
  wdri	           : out std_logic;

  -- ALU interface(Data inputs)
  alu_data_r_in    : out std_logic_vector(7 downto 0);

  -- ALU interface(Instruction inputs)
  active_operation : buffer decoded_operation;

  adiw_st_out      : out std_logic;
  sbiw_st_out      : out std_logic;

  -- ALU interface(Data output)
  alu_data_out    : in std_logic_vector(7 downto 0);

  -- ALU interface(Flag outputs)
  alu_flags_out   : in flag_set;

  -- General purpose register file interface
  reg_rd_in       : out std_logic_vector  (7 downto 0);
  reg_rd_out      : in  std_logic_vector  (7 downto 0);
  reg_rd_adr      : out std_logic_vector  (4 downto 0);
  reg_rr_out      : in  std_logic_vector  (7 downto 0);
  reg_rr_adr      : out std_logic_vector  (4 downto 0);
  reg_rd_wr       : out std_logic;

  post_inc        : out std_logic;                       -- POST INCREMENT FOR LD/ST INSTRUCTIONS
  pre_dec         : out std_logic;                        -- PRE DECREMENT FOR LD/ST INSTRUCTIONS
  reg_h_wr        : out std_logic;
  reg_h_out       : in  std_logic_vector (15 downto 0);
  reg_h_adr       : out std_logic_vector (2 downto 0);    -- x,y,z
  reg_z_out       : in  std_logic_vector (15 downto 0);  -- OUTPUT OF R31:R30 FOR LPM/ELPM/IJMP INSTRUCTIONS

  -- I/O register file interface
  sreg_fl_in      : out std_logic_vector(7 downto 0); 
  globint         : in  std_logic; -- SREG I flag

  sreg_fl_wr_en   : out std_logic_vector(7 downto 0);   --FLAGS WRITE ENABLE SIGNALS       

  spl_out         : in  std_logic_vector(7 downto 0);         
  sph_out         : in  std_logic_vector(7 downto 0);         
  sp_ndown_up     : out std_logic; -- DIRECTION OF CHANGING OF STACK POINTER SPH:SPL 0->UP(+) 1->DOWN(-)
  sp_en           : out std_logic; -- WRITE ENABLE(COUNT ENABLE) FOR SPH AND SPL REGISTERS

  rampz_out       : in  std_logic_vector(7 downto 0);

  -- Bit processor interface
  bit_num_r_io    : out std_logic_vector (2 downto 0); -- BIT NUMBER FOR CBI/SBI/BLD/BST/SBRS/SBRC/SBIC/SBIS INSTRUCTIONS
  bitpr_io_out    : in  std_logic_vector(7 downto 0);  -- SBI/CBI OUT        
  branch          : out std_logic_vector (2 downto 0); -- NUMBER (0..7) OF BRANCH CONDITION FOR BRBS/BRBC INSTRUCTION
  bit_pr_sreg_out : in  std_logic_vector(7 downto 0);  -- BCLR/BSET/BST(T-FLAG ONLY)             
  bld_op_out      : in  std_logic_vector(7 downto 0);  -- BLD OUT (T FLAG)
  bit_test_op_out : in  std_logic;                     -- OUTPUT OF SBIC/SBIS/SBRS/SBRC

  sbi_st_out      : out std_logic;
  cbi_st_out      : out std_logic);

end pm_fetch_dec;

architecture RTL of pm_fetch_dec is

-- COPIES OF OUTPUTS
signal ramadr_reg_in  : std_logic_vector(15 downto 0); -- INPUT OF THE ADDRESS REGISTER
signal ramadr_reg_en  : std_logic;                     -- ADRESS REGISTER CLOCK ENABLE SIGNAL

signal irqack_int     : std_logic;
signal irqackad_int   : std_logic_vector(irqackad'range);

-- ####################################################
-- INTERNAL SIGNALS
-- ####################################################

-- NEW SIGNALS
signal   two_word_inst       : std_logic;                    -- CALL/JMP/STS/LDS INSTRUCTION INDICATOR

signal   ram_adr_int         : std_logic_vector (15 downto 0);
constant const_ram_to_reg    : std_logic_vector := "00000000000";  -- LD/LDS/LDD/ST/STS/STD ADDRESSING GENERAL PURPOSE REGISTER (R0-R31) 0x00..0x19
constant const_ram_to_io_a   : std_logic_vector := "00000000001";  -- LD/LDS/LDD/ST/STS/STD ADDRESSING GENERAL I/O PORT 0x20 0x3F 
constant const_ram_to_io_b   : std_logic_vector := "00000000010";  -- LD/LDS/LDD/ST/STS/STD ADDRESSING GENERAL I/O PORT 0x20 0x3F 
constant const_ram_to_io_c   : std_logic_vector := "0001";  -- LD/LDS/LDD/ST/STS/STD ADDRESSING GENERAL I/O PORT 0x1000 0x1FFF 
constant const_ram_to_io_d   : std_logic_vector := "00100000000";  -- LD/LDS/LDD/ST/STS/STD ADDRESSING GENERAL I/O PORT 0x1000 0x3FFF 

-- LD/LDD/ST/STD SIGNALS
signal adiw_sbiw_encoder_out : std_logic_vector (4 downto 0);
signal adiw_sbiw_encoder_mux_out : std_logic_vector (4 downto 0);


-- PROGRAM COUNTER SIGNALS
signal program_counter_tmp      : std_logic_vector (15 downto 0); -- TO STORE PC DURING LPM/ELPM INSTRUCTIONS
signal program_counter          : std_logic_vector (15 downto 0);
signal program_counter_in       : std_logic_vector (15 downto 0);
signal program_counter_high_fr  : std_logic_vector (7 downto 0); -- TO STORE PC FOR CALL,IRQ,RCALL,ICALL

signal pc_low       : std_logic_vector (7 downto 0);
signal pc_high      : std_logic_vector (7 downto 0);


signal pc_low_en       : std_logic;
signal pc_high_en      : std_logic;

signal offset_brbx     : std_logic_vector (15 downto 0);    -- OFFSET FOR BRCS/BRCC   INSTRUCTION  !!CHECKED
signal offset_rxx      : std_logic_vector (15 downto 0);    -- OFFSET FOR RJMP/RCALL  INSTRUCTION  !!CHECKED

signal pa15_pm         : std_logic; -- ADDRESS LINE 15 FOR LPM/ELPM INSTRUCTIONS ('0' FOR LPM,RAMPZ(0) FOR ELPM) 

signal alu_reg_wr      : std_logic; -- ALU INSTRUCTIONS PRODUCING WRITE TO THE GENERAL PURPOSE REGISTER FILE	

-- DATA MEMORY,GENERAL PURPOSE REGISTERS AND I/O REGISTERS LOGIC

--! IMPORTANT NOTICE : OPERATIONS WHICH USE STACK POINTER (SPH:SPL) CAN NOT ACCCSESS GENERAL
-- PURPOSE REGISTER FILE AND INPUT/OUTPUT REGISTER FILE !
-- THESE OPERATIONS ARE : RCALL/ICALL/CALL/RET/RETI/PUSH/POP INSTRUCTIONS  AND INTERRUPT 

signal reg_file_adr_space  : std_logic; -- ACCSESS TO THE REGISTER FILE
signal io_file_adr_space   : std_logic; -- ACCSESS TO THE I/O FILE

-- STATE MACHINES SIGNALS
signal irq_start      : std_logic;

signal nirq_st0       : std_logic;
signal irq_st1        : std_logic;
signal irq_st2        : std_logic;
signal irq_st3        : std_logic;

signal ncall_st0      : std_logic;
signal call_st1       : std_logic;
signal call_st2       : std_logic;
signal call_st3       : std_logic;

signal nrcall_st0     : std_logic;
signal rcall_st1      : std_logic;
signal rcall_st2      : std_logic;

signal nicall_st0     : std_logic;
signal icall_st1      : std_logic;
signal icall_st2      : std_logic;

signal njmp_st0       : std_logic;
signal jmp_st1        : std_logic;
signal jmp_st2        : std_logic;

signal ijmp_st        : std_logic;

signal rjmp_st        : std_logic;

signal nret_st0       : std_logic;
signal ret_st1        : std_logic;
signal ret_st2        : std_logic;
signal ret_st3        : std_logic;

signal nreti_st0      : std_logic;
signal reti_st1       : std_logic;
signal reti_st2       : std_logic;
signal reti_st3       : std_logic;

signal brxx_st        : std_logic;  -- BRANCHES

signal adiw_st        : std_logic;
signal sbiw_st        : std_logic;

signal nskip_inst_st0 : std_logic;
signal skip_inst_st1  : std_logic;
signal skip_inst_st2  : std_logic;  -- ALL SKIP INSTRUCTIONS SBRS/SBRC/SBIS/SBIC/CPSE 

signal skip_inst_start  : std_logic;

signal nlpm_st0       : std_logic;
signal lpm_st1        : std_logic;
signal lpm_st2        : std_logic;

signal nelpm_st0      : std_logic;
signal elpm_st1       : std_logic;
signal elpm_st2       : std_logic;

--signal nsts_st0       : std_logic;
--signal sts_st1        : std_logic;
--signal sts_st2        : std_logic;

signal sts_st         : std_logic;

--signal nlds_st0       : std_logic;
--signal lds_st1        : std_logic;
--signal lds_st2        : std_logic;

signal lds_st           : std_logic;

signal st_st          : std_logic;
signal ld_st          : std_logic;

signal sbi_st         : std_logic;
signal cbi_st         : std_logic;

signal push_st        : std_logic;
signal pop_st	      : std_logic;

-- INTERNAL STATE MACHINES
signal nop_insert_st  : std_logic;
signal cpu_busy       : std_logic;

-- INTERNAL COPIES OF OUTPUTS
signal pc_int              : std_logic_vector (15 downto 0);
signal adr_int             : std_logic_vector (15 downto 0);
signal iore_int 		   : std_logic;
signal iowe_int            : std_logic;
signal ramadr_int          : std_logic_vector (15 downto 0);
signal ramre_int           : std_logic;
signal ramwe_int           : std_logic;
signal dbusout_int         : std_logic_vector (7 downto 0);

-- COMMAND REGISTER
signal instruction_reg      : std_logic_vector (15 downto 0); -- OUTPUT OF THE INSTRUCTION REGISTER
signal instruction_code_reg : std_logic_vector (15 downto 0); -- OUTPUT OF THE INSTRUCTION REGISTER WITH NOP INSERTION
signal instruction_reg_ena  : std_logic;                               -- CLOCK ENABLE


-- IRQ INTERNAL LOGIC
signal irq_int              : std_logic;
signal irq_vector_adr       : std_logic_vector(15 downto 0);

-- INTERRUPT RELATING REGISTERS
signal pc_for_interrupt : std_logic_vector(15 downto 0); 

-- DATA EXTRACTOR SIGNALS
signal dex_dat8_immed  : std_logic_vector (7 downto 0);  -- IMMEDIATE CONSTANT (DATA) -> ANDI,ORI,SUBI,SBCI,CPI,LDI
signal dex_dat6_immed  : std_logic_vector (5 downto 0);  -- IMMEDIATE CONSTANT (DATA) -> ADIW,SBIW
signal dex_adr12mem_s  : std_logic_vector (11 downto 0); -- RELATIVE ADDRESS (SIGNED) -> RCALL,RJMP
signal dex_adr6port    : std_logic_vector (5 downto 0);  -- I/O PORT ADDRESS -> IN,OUT
signal dex_adr5port    : std_logic_vector (4 downto 0);  -- I/O PORT ADDRESS -> CBI,SBI,SBIC,SBIS
signal dex_adr_disp    : std_logic_vector (5 downto 0);  -- DISPLACEMENT FO ADDDRESS -> STD,LDD
signal dex_condition   : std_logic_vector (2 downto 0);  -- CONDITION -> BRBC,BRBS
signal dex_bitnum_sreg : std_logic_vector (2 downto 0);  -- NUMBER OF BIT IN SREG -> BCLR,BSET
signal dex_adrreg_r    : std_logic_vector (4 downto 0);  -- SOURCE REGISTER ADDRESS -> .......
signal dex_adrreg_d    : std_logic_vector (4 downto 0);  -- DESTINATION REGISTER ADDRESS -> ......
signal dex_bitop_bitnum : std_logic_vector(2 downto 0);  -- NUMBER OF BIT FOR BIT ORIENTEDE OPERATION -> BST/BLD+SBI/CBI+SBIC/SBIS+SBRC/SBRS !! CHECKED
signal dex_brxx_offset : std_logic_vector (6 downto 0);  -- RELATIVE ADDRESS (SIGNED) -> BRBC,BRBS !! CHECKED
signal dex_adiw_sbiw_reg_adr  : std_logic_vector (1 downto 0);  -- ADDRESS OF THE LOW REGISTER FOR ADIW/SBIW INSTRUCTIONS

signal dex_adrreg_d_latched : std_logic_vector (4 downto 0);   --  STORE ADDRESS OF DESTINATION REGISTER FOR LDS/STS/POP INSTRUCTIONS
signal gp_reg_tmp           : std_logic_vector (7 downto 0);   --  STORE DATA FROM THE REGISTERS FOR STS,ST INSTRUCTIONS
signal cbi_sbi_io_adr_tmp   : std_logic_vector (4 downto 0);   --  STORE ADDRESS OF I/O PORT FOR CBI/SBI INSTRUCTION
signal cbi_sbi_bit_num_tmp  : std_logic_vector (2 downto 0);   --  STORE ADDRESS OF I/O PORT FOR CBI/SBI INSTRUCTION

-- INSTRUCTIONS DECODER SIGNALS

--signal active_operation : decoded_operation;

-- ADDITIONAL SIGNALS
signal is_post_increment_operation   :  std_logic; -- POST INCREMENT FLAG FOR LD,ST INSTRUCTIONS
signal is_pre_decrement_operation    :  std_logic; -- PRE DECREMENT  FLAG FOR LD,ST INSTRUCTIONS

-- ##################################################

-- SREG FLAGS WRITE ENABLE SIGNALS

alias sreg_c_wr_en  : std_logic is sreg_fl_wr_en(0);
alias sreg_z_wr_en  : std_logic is sreg_fl_wr_en(1);
alias sreg_n_wr_en  : std_logic is sreg_fl_wr_en(2);
alias sreg_v_wr_en  : std_logic is sreg_fl_wr_en(3);
alias sreg_s_wr_en  : std_logic is sreg_fl_wr_en(4);
alias sreg_h_wr_en  : std_logic is sreg_fl_wr_en(5);
alias sreg_t_wr_en  : std_logic is sreg_fl_wr_en(6);
alias sreg_i_wr_en  : std_logic is sreg_fl_wr_en(7);

signal sreg_bop_wr_en : std_logic_vector (7 downto 0);                

signal sreg_adr_eq  : std_logic;
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

begin


-- INSTRUCTION FETCH
instruction_reg_ena <= '1'; -- FOR TEST

instruction_fetch:process(cp2,ireset)
begin
if ireset='0' then                              -- RESET
instruction_reg <= (others => '0');
elsif (cp2='1' and cp2'event) then            -- CLOCK
 if (cp2en='1') then 							  -- Clock enable
  if instruction_reg_ena='1' then               
   instruction_reg <= inst;
  end if;
 end if; 
end if;
end process;

-- TWO WORDS INSTRUCTION DETECTOR (CONNECTED DIRECTLY TO THE INSTRUCTION REGISTER)
two_word_inst <= '1' when 
((instruction_reg(15 downto 9)&instruction_reg(3 downto 1)="1001010111") or    -- CALL
 (instruction_reg(15 downto 9)&instruction_reg(3 downto 1)="1001010110")) or   -- JMP
 (instruction_reg(15 downto 9)&instruction_reg(3 downto 0) = "10010000000") or -- LDS
 (instruction_reg(15 downto 9)&instruction_reg(3 downto 0) = "10010010000")    -- STS
   else '0';  -- TO DETECT CALL/JMP/LDS/STS INSTRUCTIONS FOR SBRS/SBRC/SBIS/SBIC/CPSE

	
	
-- DATA EXTRACTOR (CONNECTED DIRECTLY TO THE INSTRUCTION REGISTER)
dex_dat8_immed <= instruction_reg(11 downto 8) & instruction_reg(3 downto 0);
dex_dat6_immed <= instruction_reg(7 downto 6) & instruction_reg(3 downto 0);
dex_adr12mem_s <= instruction_reg(11 downto 0); 
dex_adr6port <= instruction_reg(10 downto 9) & instruction_reg(3 downto 0);    
dex_adr5port <= instruction_reg(7 downto 3);  
dex_adr_disp <= instruction_reg(13) & instruction_reg(11 downto 10) & instruction_reg(2 downto 0);      
dex_condition <= instruction_reg(2 downto 0);   
dex_bitop_bitnum <= instruction_reg(2 downto 0);      -- NUMBER(POSITION) OF TESTING BIT IN SBRC/SBRS/SBIC/SBIS INSTRUCTION
dex_bitnum_sreg <= instruction_reg(6 downto 4);    
dex_adrreg_r  <=  instruction_reg(9) & instruction_reg(3 downto 0);
dex_adrreg_d  <= instruction_reg(8 downto 4);     
dex_brxx_offset <= instruction_reg(9 downto 3);       -- OFFSET FOR BRBC/BRBS     
dex_adiw_sbiw_reg_adr <= instruction_reg(5 downto 4); -- ADDRESS OF THE LOW REGISTER FOR ADIW/SBIW INSTRUCTIONS
--dex_adrindreg <= instruction_reg(3 downto 2);     

-- LATCH Rd ADDDRESS FOR LDS/STS/POP INSTRUCTIONS
latcht_rd_adr:process(cp2,ireset)
begin
if ireset ='0' then
dex_adrreg_d_latched <= (others => '0');
elsif (cp2='1' and cp2'event) then
 if (cp2en='1') then 							  -- Clock enable
  if ((active_operation.is_ld_x or active_operation.is_ld_y or active_operation.is_ldd_y or active_operation.is_ld_z or active_operation.is_ldd_z) or active_operation.is_sts or 
	  (active_operation.is_st_x  or active_operation.is_st_y or active_operation.is_std_y or active_operation.is_st_z or active_operation.is_std_z)or active_operation.is_lds or 
	   active_operation.is_pop)='1' then 
   dex_adrreg_d_latched <= dex_adrreg_d;
  end if;
 end if;
end if;
end process;
-- +++++++++++++++++++++++++++++++++++++++++++++++++


-- R24:R25/R26:R27/R28:R29/R30:R31 ADIW/SBIW  ADDRESS CONTROL LOGIC
adiw_sbiw_encoder_out <= "11"&dex_adiw_sbiw_reg_adr&'0';

adiw_sbiw_high_reg_adr:process(cp2,ireset)
begin
if ireset ='0' then
adiw_sbiw_encoder_mux_out <= (others=>'0'); 
elsif(cp2='1' and cp2'event) then
 if (cp2en='1') then 							  -- Clock enable
  adiw_sbiw_encoder_mux_out <= adiw_sbiw_encoder_out +1;
 end if;
end if;
end process;
	
-- ##########################

-- NOP INSERTION

--instruction_code_reg <= instruction_reg when nop_insert_st='0' else (others => '0');
instruction_code_reg <= (others => '0') when (nop_insert_st='1') else -- NOP
                        instruction_reg;												-- Instruction 

	
nop_insert_st <= adiw_st or sbiw_st or cbi_st or sbi_st or rjmp_st or ijmp_st or pop_st or push_st or
              brxx_st or ld_st or st_st or ncall_st0 or nirq_st0 or nret_st0 or nreti_st0 or nlpm_st0 or njmp_st0 or
              nrcall_st0 or nicall_st0 or sts_st or lds_st or nskip_inst_st0;

			  
-- INSTRUCTION DECODER (CONNECTED AFTER NOP INSERTION LOGIC)

active_operation.is_adc  <= '1' when instruction_code_reg(15 downto 10) = "000111" else '0'; -- 000111XXXXXXXXXX
active_operation.is_add  <= '1' when instruction_code_reg(15 downto 10) = "000011" else '0'; -- 000011XXXXXXXXXX
active_operation.is_adiw <= '1' when instruction_code_reg(15 downto 8) = "10010110" else '0'; -- 10010110XXXXXXXX
active_operation.is_and  <= '1' when instruction_code_reg(15 downto 10) = "001000" else '0'; -- 001000XXXXXXXXXX
active_operation.is_andi <= '1' when instruction_code_reg(15 downto 12) = "0111" else '0'; -- 0111XXXXXXXXXXXX
active_operation.is_asr  <= '1' when instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0) = "10010100101" else '0'; -- 1001010XXXXX0101
active_operation.is_bclr <= '1' when instruction_code_reg(15 downto 7)&instruction_code_reg(3 downto 0) = "1001010011000" else '0'; -- 100101001XXX1000
active_operation.is_bld  <= '1' when instruction_code_reg(15 downto 9)&instruction_code_reg(3) = "11111000" else '0'; -- 1111100XXXXX0XXX
active_operation.is_brbc <= '1' when instruction_code_reg(15 downto 10) = "111101" else '0'; -- 111101XXXXXXXXXX
active_operation.is_brbs <= '1' when instruction_code_reg(15 downto 10) = "111100" else '0'; -- 111100XXXXXXXXXX
active_operation.is_bset <= '1' when instruction_code_reg(15 downto 7)&instruction_code_reg(3 downto 0) = "1001010001000" else '0'; -- 100101000XXX1000
active_operation.is_bst  <= '1' when instruction_code_reg(15 downto 9) = "1111101" else '0'; -- 1111101XXXXXXXXX
active_operation.is_call <= '1' when instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 1) = "1001010111" else '0'; -- 1001010XXXXX111X
active_operation.is_cbi  <= '1' when instruction_code_reg(15 downto 8) = "10011000" else '0'; -- 10011000XXXXXXXX
active_operation.is_com  <= '1' when instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0) = "10010100000" else '0'; -- 1001010XXXXX0000
active_operation.is_cp   <= '1' when instruction_code_reg(15 downto 10) = "000101" else '0'; -- 000101XXXXXXXXXX
active_operation.is_cpc  <= '1' when instruction_code_reg(15 downto 10) = "000001" else '0'; -- 000001XXXXXXXXXX
active_operation.is_cpi  <= '1' when instruction_code_reg(15 downto 12) = "0011" else '0'; -- 0011XXXXXXXXXXXX
active_operation.is_cpse <= '1' when instruction_code_reg(15 downto 10) = "000100" else '0'; -- 000100XXXXXXXXXX
active_operation.is_dec  <= '1' when instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0) = "10010101010" else '0'; -- 1001010XXXXX1010
active_operation.is_elpm <= '1' when instruction_code_reg = "1001010111011000" else '0'; -- 1001010111011000
active_operation.is_eor  <= '1' when instruction_code_reg(15 downto 10) = "001001" else '0'; -- 001001XXXXXXXXXX
active_operation.is_icall<= '1' when instruction_code_reg(15 downto 8)&instruction_code_reg(3 downto 0) = "100101011001" else '0'; -- 10010101XXXX1001
active_operation.is_ijmp <= '1' when instruction_code_reg(15 downto 8)&instruction_code_reg(3 downto 0) = "100101001001" else '0'; -- 10010100XXXX1001
active_operation.is_in   <= '1' when instruction_code_reg(15 downto 11) = "10110" else '0'; -- 10110XXXXXXXXXXX
active_operation.is_inc  <= '1' when instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0) = "10010100011" else '0'; -- 1001010XXXXX0011
active_operation.is_jmp  <= '1' when instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 1) = "1001010110" else '0'; -- 1001010XXXXX110X


-- LD,LDD 
active_operation.is_ld_x <= '1' when instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0) = "10010001100" or 
                     instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0) = "10010001101"	or
					 instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0) = "10010001110" else '0';
		
active_operation.is_ld_y <= '1' when (instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0)="10010001001" or 
					  instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0)="10010001010") else '0'; 

active_operation.is_ldd_y<= '1' when instruction_code_reg(15 downto 14)&instruction_code_reg(12)&instruction_code_reg(9)&instruction_code_reg(3) = "10001" else '0'; -- 10X0XX0XXXXX1XXX    

active_operation.is_ld_z <= '1' when (instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0)="10010000001" or 
					  instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0)="10010000010") else '0'; 

active_operation.is_ldd_z<= '1' when instruction_code_reg(15 downto 14)&instruction_code_reg(12)&instruction_code_reg(9)&instruction_code_reg(3) = "10000" else '0'; -- 10X0XX0XXXXX0XXX       
-- ######


active_operation.is_ldi <= '1' when instruction_code_reg(15 downto 12) = "1110" else '0'; -- 1110XXXXXXXXXXXX

active_operation.is_lds <= '1' when instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0) = "10010000000" else '0'; -- 1001000XXXXX0000

active_operation.is_lpm <= '1' when instruction_code_reg = "1001010111001000" else '0'; -- 1001010111001000

active_operation.is_lsr <= '1' when instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0) = "10010100110" else '0'; -- 1001010XXXXX0110

active_operation.is_mov <= '1' when instruction_code_reg(15 downto 10) = "001011" else '0'; -- 001011XXXXXXXXXX

active_operation.is_mul <= '1' when instruction_code_reg(15 downto 10) = "100111" else '0'; -- 100111XXXXXXXXXX

active_operation.is_neg <= '1' when instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0) = "10010100001" else '0'; -- 1001010XXXXX0001

active_operation.is_nop <= '1' when instruction_code_reg = "0000000000000000" else '0'; -- 0000000000000000

active_operation.is_or  <= '1' when instruction_code_reg(15 downto 10) = "001010" else '0'; -- 001010XXXXXXXXXX

active_operation.is_ori <= '1' when instruction_code_reg(15 downto 12) = "0110" else '0'; -- 0110XXXXXXXXXXXX 

active_operation.is_out <= '1' when instruction_code_reg(15 downto 11) = "10111" else '0'; -- 10111XXXXXXXXXXX

active_operation.is_pop <= '1' when instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0) = "10010001111" else '0'; -- 1001000XXXXX1111

active_operation.is_push<= '1' when instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0) = "10010011111" else '0'; -- 1001001XXXXX1111

active_operation.is_rcall<= '1' when instruction_code_reg(15 downto 12) = "1101" else '0'; -- 1101XXXXXXXXXXXX

active_operation.is_ret  <= '1' when instruction_code_reg(15 downto 7)&instruction_code_reg(4 downto 0) = "10010101001000" else '0'; -- 100101010XX01000

active_operation.is_reti <= '1' when instruction_code_reg(15 downto 7)&instruction_code_reg(4 downto 0) = "10010101011000" else '0'; -- 100101010XX11000

active_operation.is_rjmp <= '1' when instruction_code_reg(15 downto 12) = "1100" else '0'; -- 1100XXXXXXXXXXXX

active_operation.is_ror  <= '1' when instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0) = "10010100111" else '0'; -- 1001010XXXXX0111

active_operation.is_sbc  <= '1' when instruction_code_reg(15 downto 10) = "000010" else '0'; -- 000010XXXXXXXXXX

active_operation.is_sbci <= '1' when instruction_code_reg(15 downto 12) = "0100" else '0'; -- 0100XXXXXXXXXXXX

active_operation.is_sbi  <= '1' when instruction_code_reg(15 downto 8) = "10011010" else '0'; -- 10011010XXXXXXXX

active_operation.is_sbic <= '1' when instruction_code_reg(15 downto 8) = "10011001" else '0'; -- 10011001XXXXXXXX

active_operation.is_sbis <= '1' when instruction_code_reg(15 downto 8) = "10011011" else '0'; -- 10011011XXXXXXXX

active_operation.is_sbiw <= '1' when instruction_code_reg(15 downto 8) = "10010111" else '0'; -- 10010111XXXXXXXX

active_operation.is_sbrc <= '1' when instruction_code_reg(15 downto 9) = "1111110" else '0'; -- 1111110XXXXXXXXX

active_operation.is_sbrs <= '1' when instruction_code_reg(15 downto 9) = "1111111" else '0'; -- 1111111XXXXXXXXX

active_operation.is_sleep<= '1' when instruction_code_reg(15 downto 5)&instruction_code_reg(3 downto 0) = "100101011001000" else '0'; -- 10010101100X1000


-- ST,STD
active_operation.is_st_x <= '1' when instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0) = "10010011100" or 
                     instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0) = "10010011101" or 
                     instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0) = "10010011110" else '0';
	
active_operation.is_st_y <= '1' when (instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0)="10010011001" or 
					  instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0)="10010011010") else '0'; 

active_operation.is_std_y<= '1' when instruction_code_reg(15 downto 14)&instruction_code_reg(12)&instruction_code_reg(9)&instruction_code_reg(3) = "10011" else '0'; -- 10X0XX1XXXXX1XXX    

active_operation.is_st_z <= '1' when (instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0)="10010010001" or 
					  instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0)="10010010010") else '0'; 

active_operation.is_std_z<= '1' when instruction_code_reg(15 downto 14)&instruction_code_reg(12)&instruction_code_reg(9)&instruction_code_reg(3) = "10010" else '0'; -- 10X0XX1XXXXX0XXX 
-- ######

active_operation.is_sts  <= '1' when instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0) = "10010010000" else '0'; -- 1001001XXXXX0000

active_operation.is_sub  <= '1' when instruction_code_reg(15 downto 10) = "000110" else '0'; -- 000110XXXXXXXXXX

active_operation.is_subi <= '1' when instruction_code_reg(15 downto 12) = "0101" else '0'; -- 0101XXXXXXXXXXXX

active_operation.is_swap <= '1' when instruction_code_reg(15 downto 9)&instruction_code_reg(3 downto 0) = "10010100010" else '0'; -- 1001010XXXXX0010

active_operation.is_wdr  <= '1' when instruction_code_reg(15 downto 5)&instruction_code_reg(3 downto 0) = "100101011011000" else '0'; -- 10010101101X1000

-- ADDITIONAL SIGNALS
is_post_increment_operation <= '1' when (instruction_code_reg(1 downto 0) = "01" and 
 (active_operation.is_st_x or active_operation.is_st_y or active_operation.is_st_z or active_operation.is_ld_x or active_operation.is_ld_y or active_operation.is_ld_z)='1') else '0';  -- POST INCREMENT FOR LD/ST INSTRUCTIONS

is_pre_decrement_operation <= '1' when (instruction_code_reg(1 downto 0)	= "10" and
 (active_operation.is_st_x or active_operation.is_st_y or active_operation.is_st_z or active_operation.is_ld_x or active_operation.is_ld_y or active_operation.is_ld_z)='1') else '0';  -- PRE DECREMENT FOR LD/ST INSTRUCTIONS 
				
	
-- ##########################################################################################################

-- WRITE ENABLE SIGNALS FOR ramadr_reg
ramadr_reg_en <= active_operation.is_ld_x or active_operation.is_ld_y or active_operation.is_ldd_y or active_operation.is_ld_z or active_operation.is_ldd_z or active_operation.is_lds or    -- LD/LDD/LDS(two cycle execution) 
                 active_operation.is_st_x or active_operation.is_st_y or active_operation.is_std_y or active_operation.is_st_z or active_operation.is_std_z or active_operation.is_sts or    -- ST/STS/STS(two cycle execution)
				 active_operation.is_push or active_operation.is_pop or
				 active_operation.is_rcall or (rcall_st1 and not cpuwait) or active_operation.is_icall or (icall_st1 and not cpuwait) or -- RCALL/ICALL
				 call_st1 or  (call_st2 and not cpuwait) or irq_st1 or (irq_st2 and not cpuwait) or      -- CALL/IRQ
				 active_operation.is_ret or (ret_st1 and not cpuwait ) or active_operation.is_reti or (reti_st1 and not cpuwait);		 -- RET/RETI  -- ??


-- RAMADR MUX
ramadr_reg_in <= sph_out&spl_out when 
  (active_operation.is_rcall or (rcall_st1 and not cpuwait)or active_operation.is_icall or (icall_st1 and not cpuwait)or  -- RCALL/ICALL
   call_st1  or (call_st2 and not cpuwait) or irq_st1   or (irq_st2 and not cpuwait)  or  -- CALL/IRQ
   active_operation.is_push )='1' else 	                                                                  -- PUSH
   (sph_out&spl_out)+1 when (active_operation.is_ret or (ret_st1 and not cpuwait)  or active_operation.is_reti  or (reti_st1 and not cpuwait) or active_operation.is_pop)='1' else  -- RET/RETI/POP
   inst when (active_operation.is_lds or active_operation.is_sts) ='1' else     -- LDS/STS (two cycle execution)	
   reg_h_out when (active_operation.is_ld_x or active_operation.is_ld_y or active_operation.is_ld_z or active_operation.is_st_x or active_operation.is_st_y or active_operation.is_st_z)='1' else  -- LD/ST	  
   (reg_h_out + ("000000000"&dex_adr_disp));                                                       -- LDD/STD  
	  
								
-- ADDRESS REGISTER								
ramadr_reg:process(cp2,ireset)
begin
if ireset='0' then 
ramadr_int <= (others => '0');
elsif(cp2='1' and cp2'event) then
 if (cp2en='1') then 							  -- Clock enable
  if (ramadr_reg_en='1') then                            
   ramadr_int <= ramadr_reg_in;
  end if;
 end if;
end if;
end process;

ramadr <= ramadr_int;

-- GENERAL PURPOSE REGISTERS ADDRESSING FLAG FOR ST/STD/STS INSTRUCTIONS
gp_reg_adr:process(cp2,ireset)
begin
if ireset='0' then 
reg_file_adr_space <='0';
elsif(cp2='1' and cp2'event) then
 if (cp2en='1') then 							  -- Clock enable
  if (ramadr_reg_en='1') then                            
   if (ramadr_reg_in(15 downto 5)=const_ram_to_reg) then 
    reg_file_adr_space <= '1';                             -- ADRESS RANGE 0x0000-0x001F -> REGISTERS (R0-R31)
   else 
    reg_file_adr_space <= '0';
   end if;
  end if;
 end if;
end if;
end process;

-- I/O REGISTERS ADDRESSING FLAG FOR ST/STD/STS INSTRUCTIONS
io_reg_adr:process(cp2,ireset)
begin
if ireset='0' then io_file_adr_space<='0';
elsif(cp2='1' and cp2'event) then
 if (cp2en='1') then 							  -- Clock enable
  if (ramadr_reg_en='1') then                           
   if (ramadr_reg_in(15 downto 5)=const_ram_to_io_a or ramadr_reg_in(15 downto 5)=const_ram_to_io_b or ramadr_reg_in(15 downto 12)=const_ram_to_io_c) then 
    io_file_adr_space <= '1';                             -- ADRESS RANGE 0x0020-0x005F -> I/O PORTS (0x00-0x3F) and ADRESS RANGE 0x1000-0x1FFF -> I/O PORTS (0x0FE0-0x1FDF) User Ports
   else 
    io_file_adr_space <= '0';
   end if;
  end if;
 end if;
end if;
end process;



-- ##########################################################################################################


-- REGRE/REGWE LOGIC (5 BIT ADDSRESS BUS (INTERNAL ONLY) 32 LOCATIONS (R0-R31))

-- WRITE ENABLE FOR Rd REGISTERS 
alu_reg_wr <= active_operation.is_adc or active_operation.is_add or active_operation.is_adiw or adiw_st or active_operation.is_sub or active_operation.is_subi or active_operation.is_sbc or active_operation.is_sbci or
              active_operation.is_sbiw or  sbiw_st or active_operation.is_and or active_operation.is_andi or active_operation.is_or or active_operation.is_ori or active_operation.is_eor or active_operation.is_com or
   			  active_operation.is_neg or active_operation.is_inc or active_operation.is_dec or active_operation.is_lsr or active_operation.is_ror or active_operation.is_asr or active_operation.is_swap;
			  

reg_rd_wr <= active_operation.is_in or alu_reg_wr or active_operation.is_bld or             -- ALU INSTRUCTIONS + IN/BLD INSRTRUCTION                
 (pop_st or ld_st or lds_st)or			                    -- POP/LD/LDD/LDS INSTRUCTIONS
 ((st_st or sts_st) and reg_file_adr_space)or              -- ST/STD/STS INSTRUCTION 	      
  lpm_st2 or active_operation.is_ldi or active_operation.is_mov;                            -- LPM/LDI/MOV INSTRUCTION
 
  
  reg_rd_adr <= '1'&dex_adrreg_d(3 downto 0) when (active_operation.is_subi or active_operation.is_sbci or active_operation.is_andi or active_operation.is_ori or active_operation.is_cpi or active_operation.is_ldi)='1' else
			   "00000" when lpm_st2='1' else 
               adiw_sbiw_encoder_out     when (active_operation.is_adiw or active_operation.is_sbiw)='1' else
               adiw_sbiw_encoder_mux_out when (adiw_st or sbiw_st)='1' else
			   dex_adrreg_d_latched      when (((st_st or sts_st) and not reg_file_adr_space) or ld_st or lds_st or pop_st)='1' else
               ramadr_int(4 downto 0)    when ((st_st or sts_st) and reg_file_adr_space)='1'else --!!??
			   dex_adrreg_d;

reg_rr_adr <= ramadr_int(4 downto 0) when ((ld_st or lds_st) and reg_file_adr_space)='1'else --!!??
	          dex_adrreg_d_latched   when ((st_st or sts_st) and reg_file_adr_space)='1'else --!!??
	          dex_adrreg_r;		   
  
-- MULTIPLEXER FOR REGISTER FILE Rd INPUT
reg_rd_in <= dbusin when (active_operation.is_in or ((lds_st or ld_st)and not reg_file_adr_space) or pop_st)='1' else -- FROM INPUT DATA BUS
			 reg_rr_out when ((lds_st or ld_st)  and reg_file_adr_space)='1' else
             gp_reg_tmp when ((st_st or sts_st)  and reg_file_adr_space)='1' else -- ST/STD/STS &  ADDRESS FROM 0 TO 31 (REGISTER FILE)
			 bld_op_out when (active_operation.is_bld='1')else                                     -- FROM BIT PROCESSOR BLD COMMAND
             reg_rr_out when (active_operation.is_mov='1')else                                     -- FOR MOV INSTRUCTION 
			 instruction_reg(15 downto 8) when (lpm_st2='1' and reg_z_out(0)='1') else -- LPM/ELPM
			 instruction_reg(7 downto 0) when  (lpm_st2='1' and reg_z_out(0)='0') else -- LPM/ELPM
             dex_dat8_immed when active_operation.is_ldi='1' else
			 alu_data_out;                                               -- FROM ALU DATA OUT

-- IORE/IOWE LOGIC (6 BIT ADDRESS adr[5..0] FOR I/O PORTS(64 LOCATIONS))
iore_int <= active_operation.is_in or active_operation.is_sbi or active_operation.is_cbi or active_operation.is_sbic or active_operation.is_sbis or ((ld_st or lds_st) and io_file_adr_space);   -- IN/SBI/CBI 
iowe_int <= '1' when ((active_operation.is_out or sbi_st or cbi_st) or 
                     ((st_st or sts_st) and io_file_adr_space))='1' else '0'; -- OUT/SBI/CBI + !! ST/STS/STD


-- adr[5..0] BUS MULTIPLEXER
adr_int <= "0000000000" & dex_adr6port when (active_operation.is_in or active_operation.is_out) = '1' else                          -- IN/OUT INSTRUCTIONS  
           "0000000000" &'0'&dex_adr5port when (active_operation.is_cbi or active_operation.is_sbi or active_operation.is_sbic or active_operation.is_sbis) ='1'    else  -- CBI/SBI (READ PHASE) + SBIS/SBIC
		       "0000000000" &'0'&cbi_sbi_io_adr_tmp when (cbi_st or sbi_st)='1' else	-- CBI/SBI (WRITE PHASE)
		        ramadr_int-x"20"; --(6)&ramadr_int(4 downto 0);                                                   -- LD/LDS/LDD/ST/STS/STD

-- ramre LOGIC (16 BIT ADDRESS ramadr[15..0] FOR DATA RAM (64*1024-64-32 LOCATIONS))
--ramre_int <= not(reg_file_adr_space or io_file_adr_space) and 
--            (ld_st or lds_st2 or pop_st or                    -- LD/LDD/LDS/POP/
--             ret_st1 or ret_st2 or reti_st1 or reti_st2);     -- RET/RETI

DataMemoryRead:process(cp2,ireset)
begin
if ireset='0' then -- Reset
 ramre_int <= '0';
elsif (cp2='1' and cp2'event) then -- Clock
 if (cp2en='1') then 							  -- Clock enable	
  case ramre_int is
   when '0' =>	
    if(ramadr_reg_in(15 downto 5)/=const_ram_to_io_a and 
	   ramadr_reg_in(15 downto 5)/=const_ram_to_io_b and     
	   ramadr_reg_in(15 downto 12)/=const_ram_to_io_c and     
       ramadr_reg_in(15 downto 5)/=const_ram_to_reg  and  
      (active_operation.is_ld_x or active_operation.is_ld_y or active_operation.is_ldd_y or active_operation.is_ld_z or active_operation.is_ldd_z or  -- LD/LDD instruction	
	   active_operation.is_lds or                                           -- LDS instruction(two cycle execution)
	   active_operation.is_pop or                                                     -- POP instruction
       active_operation.is_ret or 	                                                -- RET instruction 
	   active_operation.is_reti)='1') 												    -- RETI instruction 
	   then ramre_int <='1';
    end if;
   when '1' =>	
    if ((ld_st or lds_st or pop_st or ret_st2 or reti_st2)and not cpuwait)='1' then 
     ramre_int <='0';
    end if;
   when others  =>	null;
  end case;
 end if;  
end if;
end process;			 
			 
-- ramwe LOGIC (16 BIT ADDRESS ramadr[15..0] FOR DATA RAM (64*1024-64-32 LOCATIONS))
--ramwe_int <= not(reg_file_adr_space or io_file_adr_space) and 
--            (st_st or sts_st2 or push_st or rcall_st1 or rcall_st2 or -- ST/STD/STS/PUSH/RCALL
--			                                icall_st1 or icall_st2 or -- ICALL
--			                                call_st2 or call_st3 or   -- CALL
--											irq_st2 or irq_st3);      -- INTERRUPT

DataMemoryWrite:process(cp2,ireset)
begin
if ireset='0' then -- Reset
 ramwe_int <= '0';
elsif (cp2='1' and cp2'event) then -- Clock
 if (cp2en='1') then 							  -- Clock enable
  case ramwe_int is
   when '0' =>	
    if(ramadr_reg_in(15 downto 5)/=const_ram_to_io_a and 
	   ramadr_reg_in(15 downto 5)/=const_ram_to_io_b and     
	   ramadr_reg_in(15 downto 12)/=const_ram_to_io_c and     
       ramadr_reg_in(15 downto 5)/=const_ram_to_reg  and  
      (active_operation.is_st_x or active_operation.is_st_y or active_operation.is_std_y or active_operation.is_st_z or active_operation.is_std_z or  -- ST/STD instruction	
	   active_operation.is_sts or                                           -- STS instruction (two cycle execution)	
	   active_operation.is_push or                                                    -- PUSH instruction
	   active_operation.is_rcall or													  -- RCALL instruction
	   active_operation.is_icall or													  -- ICALL instruction
	   call_st1 or                                                    -- CALL instruction
	   irq_st1)='1')                                                  -- Interrupt  
	  then ramwe_int <='1';
    end if;
   when '1' =>	
    if ((st_st or sts_st or push_st or rcall_st2 or 
	     icall_st2 or call_st3 or irq_st3)and not cpuwait)='1' then ramwe_int <='0';
    end if;
   when others  =>	null;
  end case;
end if;
end if;
end process;

-- DBUSOUT MULTIPLEXER
dbusout_mux_logic: for i in dbusout_int'range generate
dbusout_int(i)<= (reg_rd_out(i) and (active_operation.is_push or active_operation.is_sts or
                 (active_operation.is_st_x or active_operation.is_st_y or active_operation.is_std_y or active_operation.is_st_z or active_operation.is_std_z)))or      -- PUSH/ST/STD/STS INSTRUCTIONS
				 (gp_reg_tmp(i) and (st_st or sts_st))or                            -- NEW
				 (bitpr_io_out(i) and (cbi_st or sbi_st))or                          -- CBI/SBI  INSTRUCTIONS
                 (program_counter(i)         and (active_operation.is_rcall or active_operation.is_icall or call_st1))or                        -- LOW  PART OF PC
                 (program_counter_high_fr(i) and (rcall_st1 or icall_st1 or call_st2))or                        -- HIGH PART OF PC
                 (pc_for_interrupt(i) and irq_st1) or
				 (pc_for_interrupt(i+8) and irq_st2) or
				 (reg_rd_out(i) and  active_operation.is_out); -- OUT
end generate;


-- ALU CONNECTION

-- ALU Rr INPUT MUX
alu_data_r_in <= dex_dat8_immed       when (active_operation.is_subi or active_operation.is_sbci or active_operation.is_andi or active_operation.is_ori or active_operation.is_cpi)='1' else
                 "00"&dex_dat6_immed  when (active_operation.is_adiw or active_operation.is_sbiw) ='1' else
                 "00000000"           when (adiw_st or sbiw_st) ='1' else
                 reg_rr_out;


-- gp_reg_tmp STORES TEMPREOARY THE VALUE OF SOURCE REGISTER DURING ST/STD/STS INSTRUCTION
gp_registers_trig:process(cp2,ireset)
begin
if (ireset='0') then
gp_reg_tmp <= (others=>'0');
elsif (cp2='1' and cp2'event) then
 if (cp2en='1') then 							  -- Clock enable
  -- if ((active_operation.is_st_x or active_operation.is_st_y or active_operation.is_std_y or active_operation.is_st_z or active_operation.is_std_z) or sts_st1)='1' then  -- CLOCK ENABLE
  if ((active_operation.is_st_x or active_operation.is_st_y or active_operation.is_std_y or active_operation.is_st_z or active_operation.is_std_z) or active_operation.is_sts)='1' then  -- CLOCK ENABLE
     gp_reg_tmp <= reg_rd_out;
  end if;
 end if;
end if;
end process;

-- **********************************************************************************************************

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- +++++++++++++++++++++++++++++++++++++++ PROGRAM COUNTER ++++++++++++++++++++++++++++++++++++++++++++++++++
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

program_counter_high_store:process(cp2,ireset)
begin
if ireset='0' then                         -- RESET
program_counter_high_fr <=(others => '0');
elsif (cp2='1' and cp2'event) then       -- CLOCK
 if (cp2en='1') then 							  -- Clock enable
  if (active_operation.is_rcall or active_operation.is_icall or call_st1 or irq_st1) ='1' then   
   program_counter_high_fr <= program_counter(15 downto 8);       -- STORE HIGH BYTE OF THE PROGRAMM COUNTER FOR RCALL/ICALL/CALL INSTRUCTIONS AND INTERRUPTS   
  end if;
 end if;
end if;
end process;


program_counter_for_lpm_elpm:process(cp2,ireset)
begin
if ireset='0' then                         -- RESET
program_counter_tmp<=(others => '0');
elsif (cp2='1' and cp2'event) then       -- CLOCK
 if (cp2en='1') then 							  -- Clock enable
  if (active_operation.is_lpm or active_operation.is_elpm) ='1' then       
   program_counter_tmp <= program_counter;               
  end if;
 end if; 
end if;
end process;

pa15_pm <= rampz_out(0) and active_operation.is_elpm; -- '0' WHEN LPM INSTRUCTIONS  RAMPZ(0) WHEN ELPM INSTRUCTION

-- OFFSET FOR BRBC/BRBS INSTRUCTIONS +63/-64
offset_brbx <= "0000000000"&dex_brxx_offset(5 downto 0) when (dex_brxx_offset(6)='0') else -- +
               "1111111111"&dex_brxx_offset(5 downto 0);                                   -- - 

-- OFFSET FOR RJMP/RCALL INSTRUCTIONS +2047/-2048
offset_rxx <= "00000" & dex_adr12mem_s(10 downto 0) when (dex_adr12mem_s(11) ='0' ) else       -- +
              "11111" & dex_adr12mem_s(10 downto 0);                                          -- -

program_counter <= pc_high & pc_low;

program_counter_in <= 
  program_counter + offset_brbx when ((active_operation.is_brbc or active_operation.is_brbs) and  bit_test_op_out) ='1'else  -- BRBC/BRBS                  
  program_counter + offset_rxx  when (active_operation.is_rjmp or active_operation.is_rcall) = '1' else     -- RJMP/RCALL
  reg_z_out when (active_operation.is_ijmp or active_operation.is_icall)='1'else                        -- IJMP/ICALL
  pa15_pm&reg_z_out(15 downto 1) when (active_operation.is_lpm or active_operation.is_elpm) ='1'else    -- LPM/ELPM
  instruction_reg  when (jmp_st1 or call_st1)='1'else                    -- JMP/CALL
  "0000000000" & irqackad_int & '0' when irq_st1 ='1' else                 -- INTERRUPT      
  dbusin & "00000000"  when (ret_st1 or reti_st1)='1' else                 -- RET/RETI -> PC HIGH BYTE                  
  "00000000" & dbusin  when (ret_st2 or reti_st2)='1' else                 -- RET/RETI -> PC LOW BYTE                       
  program_counter_tmp when (lpm_st1)='1'                                 -- AFTER LPM/ELPM INSTRUCTION   
  else program_counter+1;      -- THE MOST USUAL CASE

            

pc_low_en  <= not (active_operation.is_ld_x or active_operation.is_ld_y or active_operation.is_ld_z or active_operation.is_ldd_y or active_operation.is_ldd_z or
	               active_operation.is_st_x or active_operation.is_st_y or active_operation.is_st_z or active_operation.is_std_y or active_operation.is_std_z or
				   ((sts_st or lds_st) and cpuwait) or 
				   active_operation.is_adiw or active_operation.is_sbiw or
				   active_operation.is_push or active_operation.is_pop or
				   active_operation.is_cbi or active_operation.is_sbi or
				   rcall_st1 or icall_st1 or call_st2 or irq_st2 or cpuwait or
				   ret_st1 or reti_st1); 


pc_high_en <= not (active_operation.is_ld_x or active_operation.is_ld_y or active_operation.is_ld_z or active_operation.is_ldd_y or active_operation.is_ldd_z or
	               active_operation.is_st_x or active_operation.is_st_y or active_operation.is_st_z or active_operation.is_std_y or active_operation.is_std_z or
				   ((sts_st or lds_st) and cpuwait) or 
				   active_operation.is_adiw or active_operation.is_sbiw or
				   active_operation.is_push or active_operation.is_pop or
				   active_operation.is_cbi or active_operation.is_sbi or
				   rcall_st1 or icall_st1 or call_st2 or irq_st2 or cpuwait or
				   ret_st2 or reti_st2);
				   
program_counter_low:process(cp2,ireset)
begin
if ireset='0' then                              -- RESET
pc_low<=(others => '0');
elsif (cp2='1' and cp2'event) then              -- CLOCK
 if (cp2en='1') then 							-- Clock enable
  if pc_low_en ='1' then                          
   pc_low <= program_counter_in(7 downto 0);
  end if;
 end if;
end if;
end process;

program_counter_high:
process(cp2,ireset)
begin
if ireset='0' then                               -- RESET
pc_high<=(others => '0');
elsif (cp2='1' and cp2'event) then               -- CLOCK
 if (cp2en='1') then 							 -- Clock enable
  if pc_high_en ='1' then                          
   pc_high <= program_counter_in(15 downto 8);
  end if;
 end if;
end if;
end process;

pc <= program_counter;														   


program_counter_for_interrupt:process(cp2,ireset)
begin
if ireset='0' then                                 -- RESET
pc_for_interrupt <=(others => '0');
elsif (cp2='1' and cp2'event) then               -- CLOCK
 if (cp2en='1') then 							 -- Clock enable
  if irq_start ='1' then                           
   pc_for_interrupt <= program_counter;
  end if;
 end if;
end if;
end process;

-- END OF PROGRAM COUNTER 

-- STATE MACHINES

skip_inst_start <= ((active_operation.is_sbrc or active_operation.is_sbrs or active_operation.is_sbic or active_operation.is_sbis) and bit_test_op_out)or
                   (active_operation.is_cpse and alu_flags_out.z);

skip_instruction_sm:process(cp2,ireset)
begin
if ireset='0' then                       -- RESET
nskip_inst_st0 <= '0';
skip_inst_st1  <= '0';
skip_inst_st2  <= '0';
elsif (cp2='1' and cp2'event) then       -- CLOCK
 if (cp2en='1') then 				     -- Clock enable
  nskip_inst_st0 <= (not nskip_inst_st0 and skip_inst_start) or 
                    (nskip_inst_st0 and not((skip_inst_st1 and not two_word_inst) or skip_inst_st2));
  skip_inst_st1  <= (not skip_inst_st1 and not nskip_inst_st0 and skip_inst_start);
  skip_inst_st2  <=  not skip_inst_st2 and skip_inst_st1 and two_word_inst;
 end if;
end if;
end process;



alu_state_machines:process(cp2,ireset)
begin
if ireset='0' then                       -- RESET
adiw_st <= '0';
sbiw_st <= '0';
elsif (cp2='1' and cp2'event) then       -- CLOCK
 if (cp2en='1') then 					 -- Clock enable
  adiw_st <= not adiw_st and active_operation.is_adiw;
  sbiw_st <= not sbiw_st and active_operation.is_sbiw;
 end if;
end if;
end process;


lpm_state_machine:process(cp2,ireset)
begin
if ireset='0' then                       -- RESET
nlpm_st0 <= '0';
lpm_st1 <= '0';
lpm_st2 <= '0';
elsif (cp2='1' and cp2'event) then       -- CLOCK
 if (cp2en='1') then 							  -- Clock enable
  nlpm_st0 <= (not nlpm_st0 and (active_operation.is_lpm or active_operation.is_elpm)) or (nlpm_st0 and not lpm_st2);
  lpm_st1  <= (not lpm_st1 and not nlpm_st0 and (active_operation.is_lpm or active_operation.is_elpm)); -- ?? 
  lpm_st2  <=  not lpm_st2 and lpm_st1;
 end if;
end if;
end process;


lds_state_machine:process(cp2,ireset)
begin
if ireset='0' then                       -- RESET
 lds_st <= '0';
elsif (cp2='1' and cp2'event) then       -- CLOCK
 if (cp2en='1') then 							  -- Clock enable	
  lds_st  <= (not lds_st and active_operation.is_lds) or (lds_st and cpuwait);
 end if;
end if;
end process;


sts_state_machine:process(cp2,ireset)
begin
if ireset='0' then                       -- RESET
 sts_st <= '0';
elsif (cp2='1' and cp2'event) then       -- CLOCK
 if (cp2en='1') then 							  -- Clock enable
  sts_st  <= (not sts_st and active_operation.is_sts) or (sts_st and cpuwait);
 end if;
end if;
end process;

jmp_state_machine:process(cp2,ireset)
begin
if ireset='0' then                       -- RESET
njmp_st0 <= '0';
jmp_st1 <= '0';
jmp_st2 <= '0';
elsif (cp2='1' and cp2'event) then       -- CLOCK
 if (cp2en='1') then 							  -- Clock enable
  njmp_st0 <= (not njmp_st0 and active_operation.is_jmp) or (njmp_st0 and not jmp_st2);
  jmp_st1  <= not jmp_st1 and not njmp_st0 and active_operation.is_jmp; -- ?? 
  jmp_st2  <= not jmp_st2 and jmp_st1;
 end if;
end if;
end process;

rcall_state_machine:process(cp2,ireset)
begin
if ireset='0' then                       -- RESET
nrcall_st0 <= '0';
rcall_st1 <= '0';
rcall_st2 <= '0';
elsif (cp2='1' and cp2'event) then       -- CLOCK
 if (cp2en='1') then 							  -- Clock enable	
  nrcall_st0 <= (not nrcall_st0 and active_operation.is_rcall) or (nrcall_st0 and not (rcall_st2 and not cpuwait));
  rcall_st1  <= (not rcall_st1 and not nrcall_st0 and active_operation.is_rcall) or (rcall_st1 and cpuwait);
  rcall_st2  <= (not rcall_st2 and rcall_st1 and not cpuwait) or (rcall_st2 and cpuwait);
 end if;
end if;
end process;

icall_state_machine:process(cp2,ireset)
begin
if ireset='0' then                       -- RESET
nicall_st0 <= '0';
icall_st1 <= '0';
icall_st2 <= '0';
elsif (cp2='1' and cp2'event) then       -- CLOCK
 if (cp2en='1') then 							  -- Clock enable	
  nicall_st0 <= (not nicall_st0 and active_operation.is_icall) or (nicall_st0 and not (icall_st2 and not cpuwait));
  icall_st1  <= (not icall_st1 and not nicall_st0 and active_operation.is_icall) or (icall_st1 and cpuwait);
  icall_st2  <= (not icall_st2 and icall_st1 and not cpuwait) or (icall_st2 and cpuwait);
 end if;
end if;
end process;

call_state_machine:process(cp2,ireset)
begin
if ireset='0' then                       -- RESET
ncall_st0 <= '0';
call_st1 <= '0';
call_st2 <= '0';
call_st3  <= '0';
elsif (cp2='1' and cp2'event) then       -- CLOCK
 if (cp2en='1') then 							  -- Clock enable
  ncall_st0 <= (not ncall_st0 and active_operation.is_call) or (ncall_st0 and not( call_st3 and not cpuwait));
  call_st1  <= not call_st1 and not ncall_st0 and active_operation.is_call;
  call_st2  <= (not call_st2 and call_st1) or (call_st2 and cpuwait);
  call_st3  <= (not call_st3 and call_st2 and not cpuwait) or (call_st3 and cpuwait);
 end if;
end if;
end process;

ret_state_machine:process(cp2,ireset)
begin
if ireset='0' then                       -- RESET
nret_st0 <= '0';
ret_st1 <= '0';
ret_st2 <= '0';
ret_st3  <= '0';
elsif (cp2='1' and cp2'event) then       -- CLOCK
 if (cp2en='1') then 							  -- Clock enable
  nret_st0 <= (not nret_st0 and active_operation.is_ret) or (nret_st0 and not ret_st3);
  ret_st1  <= (not ret_st1 and not nret_st0 and active_operation.is_ret) or (ret_st1 and cpuwait);
  ret_st2  <= (not ret_st2 and ret_st1 and not cpuwait) or (ret_st2 and cpuwait) ;
  ret_st3  <= not ret_st3 and ret_st2 and not cpuwait; 
 end if;
end if;
end process;

reti_state_machine:process(cp2,ireset)
begin
if ireset='0' then                       -- RESET
nreti_st0 <= '0';
reti_st1 <= '0';
reti_st2 <= '0';
reti_st3  <= '0';
elsif (cp2='1' and cp2'event) then       -- CLOCK
 if (cp2en='1') then 							  -- Clock enable
  nreti_st0 <= (not nreti_st0 and active_operation.is_reti) or (nreti_st0 and not reti_st3);
  reti_st1  <= (not reti_st1 and not nreti_st0 and active_operation.is_reti) or (reti_st1 and cpuwait);
  reti_st2  <= (not reti_st2 and reti_st1 and not cpuwait) or (reti_st2 and cpuwait) ;
  reti_st3  <= not reti_st3 and reti_st2 and not cpuwait; 
 end if;
end if;
end process;


-- INTERRUPT LOGIC AND STATE MACHINE 

irq_int <= '0' when	irqlines="00000000000000000000000" else '1';
 
irq_vector_adr(15 downto 6)<=(others => '0');
irq_vector_adr(0) <= '0';
-- PRIORITY ENCODER
irq_vector_adr(5 downto 1) <= "00001" when irqlines(0)='1'  else -- 0x0002
                              "00010" when irqlines(1)='1'  else -- 0x0004  
                              "00011" when irqlines(2)='1'  else -- 0x0006  
                              "00100" when irqlines(3)='1'  else -- 0x0008  
                              "00101" when irqlines(4)='1'  else -- 0x000A  
                              "00110" when irqlines(5)='1'  else -- 0x000C  
                              "00111" when irqlines(6)='1'  else -- 0x000E  
                              "01000" when irqlines(7)='1'  else -- 0x0010  
                              "01001" when irqlines(8)='1'  else -- 0x0012  
                              "01010" when irqlines(9)='1'  else -- 0x0014
                              "01011" when irqlines(10)='1' else -- 0x0016
                              "01100" when irqlines(11)='1' else -- 0x0018
                              "01101" when irqlines(12)='1' else -- 0x001A
                              "01110" when irqlines(13)='1' else -- 0x001C
                              "01111" when irqlines(14)='1' else -- 0x001E
                              "10000" when irqlines(15)='1' else -- 0x0020
                              "10001" when irqlines(16)='1' else -- 0x0022
                              "10010" when irqlines(17)='1' else -- 0x0024
                              "10011" when irqlines(18)='1' else -- 0x0026
                              "10100" when irqlines(19)='1' else -- 0x0028
                              "10101" when irqlines(20)='1' else -- 0x002A
                              "10110" when irqlines(21)='1' else -- 0x002C
                              "10111" when irqlines(22)='1' else -- 0x002E  								  
							  "00000";	  

-- MULTI CYCLE INSTRUCTION FLAG FOR IRQ
cpu_busy <= active_operation.is_adiw or active_operation.is_sbiw or active_operation.is_cbi or active_operation.is_sbi or
            active_operation.is_rjmp or active_operation.is_ijmp or
			active_operation.is_jmp or jmp_st1 or
--			active_operation.is_brbs or active_operation.is_brbc or -- Old variant
            ((active_operation.is_brbc or active_operation.is_brbs) and  bit_test_op_out) or
			active_operation.is_lpm or lpm_st1 or
			skip_inst_start or (skip_inst_st1 and two_word_inst) or
			active_operation.is_ld_x or active_operation.is_ld_y or active_operation.is_ldd_y or active_operation.is_ld_z or active_operation.is_ldd_z or (ld_st and cpuwait) or
			active_operation.is_st_x or active_operation.is_st_y or active_operation.is_std_y or active_operation.is_st_z or active_operation.is_std_z or (st_st and cpuwait) or
			active_operation.is_lds or (lds_st and cpuwait) or 
			active_operation.is_sts or (sts_st and cpuwait) or
			active_operation.is_rcall or rcall_st1 or (rcall_st2 and cpuwait) or           -- RCALL
			active_operation.is_icall or icall_st1 or (icall_st2 and cpuwait) or		   -- ICALL
			active_operation.is_call or call_st1 or call_st2 or (call_st3 and cpuwait) or  -- CALL
			active_operation.is_push or (push_st and cpuwait) or                           -- PUSH (added 14.07.05)
			active_operation.is_pop or (pop_st and cpuwait) or                             -- POP  (added 14.07.05)
		    (active_operation.is_bclr and sreg_bop_wr_en(7)) or                 -- ??? CLI
		    (iowe_int and sreg_adr_eq and not dbusout_int(7))or -- ??? Writing '0' to I flag (OUT/STD/ST/STD)
			nirq_st0 or
--			active_operation.is_ret  or nret_st0 or                             -- Old variant 
			active_operation.is_ret or ret_st1 or ret_st2 or
--			active_operation.is_reti or nreti_st0;                              -- At least one instruction must be executed after RETI and before the new interrupt.
			active_operation.is_reti or reti_st1 or reti_st2;
			
sreg_adr_eq <= '1' when adr_int = SREG_Address else '0';			

--irq_start <= irq_int and not cpu_busy and globint;
irq_start <= irq_int and not cpu_busy and globint;

irq_state_machine:process(cp2,ireset)
begin
if ireset='0' then                       -- RESET
nirq_st0 <= '0';
irq_st1 <= '0';
irq_st2 <= '0';
irq_st3 <= '0';
elsif (cp2='1' and cp2'event) then       -- CLOCK
 if (cp2en='1') then 							  -- Clock enable	
  nirq_st0 <= (not nirq_st0 and irq_start) or (nirq_st0 and not (irq_st3 and not cpuwait));
  irq_st1  <= (not irq_st1 and not nirq_st0 and irq_start);
  irq_st2  <= (not irq_st2 and irq_st1) or (irq_st2 and cpuwait);
  irq_st3  <= (not irq_st3 and irq_st2 and not cpuwait) or (irq_st3 and cpuwait);
 end if;
end if;
end process;

irqack_reg:process(cp2,ireset)
begin
if ireset='0' then                       -- RESET
irqack_int<='0';
elsif (cp2='1' and cp2'event) then       -- CLOCK
 if (cp2en='1') then 							  -- Clock enable	
  irqack_int<= not irqack_int and irq_start;
 end if;
end if;
end process;
irqack <= irqack_int;

irqackad_reg:process(cp2,ireset)
begin
if ireset='0' then                                -- RESET
irqackad_int<=(others=>'0');
elsif (cp2='1' and cp2'event) then              -- CLOCK
 if (cp2en='1') then 							  -- Clock enable
  irqackad_int<=irq_vector_adr(5 downto 1);
 end if;
end if;
end process;
irqackad <= irqackad_int;

-- *******************************************************************************************

rjmp_push_pop_ijmp_state_brxx_machine:process(cp2,ireset)
begin
if ireset='0' then                       -- RESET
rjmp_st <= '0';
ijmp_st <= '0';
push_st <= '0';
pop_st <= '0';
brxx_st <= '0';
elsif (cp2='1' and cp2'event) then       -- CLOCK
 if (cp2en='1') then 							  -- Clock enable
  rjmp_st <= active_operation.is_rjmp;    -- ??
  ijmp_st <= active_operation.is_ijmp;
  push_st <= (not push_st and active_operation.is_push) or (push_st and cpuwait);
  pop_st  <= (not pop_st  and active_operation.is_pop) or (pop_st and cpuwait);
  brxx_st <= not brxx_st and (active_operation.is_brbc or active_operation.is_brbs) and bit_test_op_out;
 end if;
end if;
end process;

-- LD/LDD/ST/STD
ld_st_state_machine:process(cp2,ireset)
begin
if ireset='0' then                       -- RESET
ld_st <= '0';
st_st <= '0';
elsif (cp2='1' and cp2'event) then       -- CLOCK
 if (cp2en='1') then 							  -- Clock enable	
  ld_st <= (not ld_st and (active_operation.is_ld_x or active_operation.is_ld_y or active_operation.is_ldd_y or active_operation.is_ld_z or active_operation.is_ldd_z)) or (ld_st and cpuwait);
  st_st <= (not st_st and (active_operation.is_st_x or active_operation.is_st_y or active_operation.is_std_y or active_operation.is_st_z or active_operation.is_std_z)) or (st_st and cpuwait);
 end if;
end if;
end process;

-- SBI/CBI
sbi_cbi_machine:process(cp2,ireset)
begin
if ireset='0' then                       -- RESET
sbi_st <= '0';
cbi_st <= '0';
cbi_sbi_io_adr_tmp  <= (others => '0');
cbi_sbi_bit_num_tmp	<= (others => '0');
elsif (cp2='1' and cp2'event) then       -- CLOCK
 if (cp2en='1') then 							  -- Clock enable
  sbi_st <= not sbi_st and active_operation.is_sbi;
  cbi_st <= not cbi_st and active_operation.is_cbi;
  cbi_sbi_io_adr_tmp <= dex_adr5port;
  cbi_sbi_bit_num_tmp <= dex_bitop_bitnum;
 end if;
end if;
end process;

-- ########################################################################################

-- SREG FLAGS WRITE ENABLE LOGIC

bclr_bset_op_en_logic:for i in sreg_bop_wr_en'range generate
sreg_bop_wr_en(i) <= '1' when (dex_bitnum_sreg=i and (active_operation.is_bclr or active_operation.is_bset)='1') else '0';
end generate;

sreg_c_wr_en <= active_operation.is_add or active_operation.is_adc or (active_operation.is_adiw or adiw_st) or active_operation.is_sub  or active_operation.is_subi or 
                active_operation.is_sbc or active_operation.is_sbci or (active_operation.is_sbiw or sbiw_st) or active_operation.is_com or active_operation.is_neg or
				active_operation.is_cp or active_operation.is_cpc or active_operation.is_cpi or
                active_operation.is_lsr or active_operation.is_ror or active_operation.is_asr or sreg_bop_wr_en(0);

sreg_z_wr_en <= active_operation.is_add or active_operation.is_adc or (active_operation.is_adiw or adiw_st) or active_operation.is_sub  or active_operation.is_subi or 
                active_operation.is_sbc or active_operation.is_sbci or (active_operation.is_sbiw or sbiw_st) or
				active_operation.is_cp or active_operation.is_cpc or active_operation.is_cpi or
                active_operation.is_and or active_operation.is_andi or active_operation.is_or or active_operation.is_ori or active_operation.is_eor or active_operation.is_com or active_operation.is_neg or
                active_operation.is_inc or active_operation.is_dec or active_operation.is_lsr or active_operation.is_ror or active_operation.is_asr or sreg_bop_wr_en(1);
                

sreg_n_wr_en <= active_operation.is_add or active_operation.is_adc or adiw_st or active_operation.is_sub  or active_operation.is_subi or 
                active_operation.is_sbc or active_operation.is_sbci or sbiw_st or
				active_operation.is_cp or active_operation.is_cpc or active_operation.is_cpi or
                active_operation.is_and or active_operation.is_andi or active_operation.is_or or active_operation.is_ori or active_operation.is_eor or active_operation.is_com or active_operation.is_neg or
                active_operation.is_inc or active_operation.is_dec or active_operation.is_lsr or active_operation.is_ror or active_operation.is_asr or sreg_bop_wr_en(2);

sreg_v_wr_en <= active_operation.is_add or active_operation.is_adc or adiw_st or active_operation.is_sub  or active_operation.is_subi or -- active_operation.is_adiw
                active_operation.is_sbc or active_operation.is_sbci or sbiw_st or active_operation.is_neg or active_operation.is_com or  -- active_operation.is_sbiw
                active_operation.is_inc or active_operation.is_dec or
				active_operation.is_cp or active_operation.is_cpc or active_operation.is_cpi or
                active_operation.is_lsr or active_operation.is_ror or active_operation.is_asr or sreg_bop_wr_en(3) or
				active_operation.is_and or active_operation.is_andi or active_operation.is_or or active_operation.is_ori or active_operation.is_eor; -- V-flag bug fixing

sreg_s_wr_en <= active_operation.is_add or active_operation.is_adc or adiw_st or active_operation.is_sub or active_operation.is_subi or 
                active_operation.is_sbc or active_operation.is_sbci or sbiw_st or 
				active_operation.is_cp or active_operation.is_cpc or active_operation.is_cpi or				
				active_operation.is_and or active_operation.is_andi or active_operation.is_or or active_operation.is_ori or active_operation.is_eor or active_operation.is_com or active_operation.is_neg or
				active_operation.is_inc or active_operation.is_dec or active_operation.is_lsr or active_operation.is_ror or active_operation.is_asr or sreg_bop_wr_en(4);

sreg_h_wr_en <= active_operation.is_add or active_operation.is_adc or active_operation.is_sub  or active_operation.is_subi or
				active_operation.is_cp or active_operation.is_cpc or active_operation.is_cpi or
                active_operation.is_sbc or active_operation.is_sbci or active_operation.is_neg or sreg_bop_wr_en(5);

sreg_t_wr_en <=  active_operation.is_bst or sreg_bop_wr_en(6);

sreg_i_wr_en <= irq_st1 or reti_st3 or sreg_bop_wr_en(7); -- WAS "irq_start"

sreg_fl_in <=  bit_pr_sreg_out when (active_operation.is_bst or active_operation.is_bclr or active_operation.is_bset)='1' else		           -- TO THE SREG
reti_st3&'0'&alu_flags_out.h&alu_flags_out.s&alu_flags_out.v&alu_flags_out.n&alu_flags_out.z&alu_flags_out.c;      
               
-- #################################################################################################################

-- *********************************************************************************************
-- ************** INSTRUCTION DECODER OUTPUTS FOR THE OTHER BLOCKS  ****************************
-- *********************************************************************************************

-- FOR ALU

adiw_st_out   <= adiw_st;
sbiw_st_out   <= sbiw_st;

-- FOR THE BIT PROCESSOR
sbi_st_out   <= sbi_st;
cbi_st_out   <= cbi_st;

-- POST INCREMENT/PRE DECREMENT FOR THE X,Y,Z REGISTERS
post_inc <= is_post_increment_operation;
pre_dec  <= is_pre_decrement_operation;
reg_h_wr <= (active_operation.is_st_x or active_operation.is_st_y or active_operation.is_st_z or active_operation.is_ld_x or active_operation.is_ld_y or active_operation.is_ld_z) and (is_post_increment_operation or is_pre_decrement_operation);

reg_h_adr(0)<= active_operation.is_st_x or active_operation.is_ld_x;
reg_h_adr(1)<= active_operation.is_st_y or active_operation.is_std_y or active_operation.is_ld_y or active_operation.is_ldd_y;
reg_h_adr(2)<= active_operation.is_st_z or active_operation.is_std_z or active_operation.is_ld_z or active_operation.is_ldd_z;

-- STACK POINTER CONTROL
sp_ndown_up <= active_operation.is_pop or active_operation.is_ret or (ret_st1 and not cpuwait) or active_operation.is_reti or (reti_st1 and not cpuwait); -- ?????????
sp_en <= active_operation.is_push or active_operation.is_pop or active_operation.is_rcall or (rcall_st1 and not cpuwait) or active_operation.is_icall or (icall_st1 and not cpuwait) or 
active_operation.is_ret or (ret_st1 and not cpuwait) or active_operation.is_reti or (reti_st1 and not cpuwait) or
call_st1 or (call_st2 and not cpuwait) or irq_st1 or (irq_st2 and not cpuwait); --????????


branch  <= dex_condition;
bit_num_r_io <= cbi_sbi_bit_num_tmp when (cbi_st or sbi_st)='1' else dex_bitop_bitnum;

adr <= adr_int;

ramre <= ramre_int;
ramwe <= ramwe_int;

iore <= iore_int;
iowe <= iowe_int;

dbusout <= dbusout_int;

-- Sleep Control
sleepi <= active_operation.is_sleep; 	 
irqok  <= irq_int;	 

-- Watchdog
wdri <= active_operation.is_wdr; 	 

-- ************************** JTAG OCD support ************************************

-- Change of flow	
change_flow <= '0';
valid_instr <= '0';

			   
end RTL;
