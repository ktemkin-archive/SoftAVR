--************************************************************************************************
--  ALU(internal module) for AVR core
--	Version 1.2
--  Designed by Ruslan Lepetenok 
--	Modified 02.08.2003 
-- (CPC/SBC/SBCI Z-flag bug found)
--  H-flag with NEG instruction found
--************************************************************************************************

library IEEE;
use IEEE.std_logic_1164.all;

use WORK.AVRucPackage.all;


entity alu_avr is port(

              --"One-hot" record which specifies the current operation.
              --Contains a range of "is_<instruction>" signals, which indicate the type of 
              --the current instruction.
              operation       : decoded_operation;

              alu_data_r_in   : in std_logic_vector(7 downto 0);
              alu_data_d_in   : in std_logic_vector(7 downto 0);
              
              alu_c_flag_in   : in std_logic;
              alu_z_flag_in   : in std_logic;


-- OPERATION SIGNALS INPUTS

              adiw_st         : in std_logic;
              sbiw_st         : in std_logic;

-- DATA OUTPUT
              alu_data_out    : out std_logic_vector(7 downto 0);

-- FLAGS OUTPUT
              flags_out   : out flag_set
);

end alu_avr;

architecture rtl of alu_avr is

-- ####################################################
-- INTERNAL SIGNALS
-- ####################################################

signal alu_data_out_int		    : std_logic_vector (7 downto 0);	

-- ALU FLAGS (INTERNAL)
signal alu_z_flag_out_int       : std_logic;
signal alu_c_flag_in_int        : std_logic;            -- INTERNAL CARRY FLAG

signal alu_n_flag_out_int       : std_logic;
signal alu_v_flag_out_int       : std_logic;
signal alu_c_flag_out_int       : std_logic;

-- ADDER SIGNALS --
signal adder_nadd_sub : std_logic;        -- 0 -> ADD ,1 -> SUB
signal adder_v_flag_out	: std_logic;

signal adder_carry : std_logic_vector(8 downto 0);
signal adder_d_in  : std_logic_vector(8 downto 0);
signal adder_r_in  : std_logic_vector(8 downto 0);
signal adder_out   : std_logic_vector(8 downto 0);

-- NEG OPERATOR SIGNALS 
signal neg_op_in    : std_logic_vector(7 downto 0);	
signal neg_op_carry : std_logic_vector(8 downto 0);
signal neg_op_out   : std_logic_vector(8 downto 0);

-- INC, DEC OPERATOR SIGNALS 
signal incdec_op_in    : std_logic_vector (7 downto 0);	
signal incdec_op_carry : std_logic_vector(7 downto 0);
signal incdec_op_out   : std_logic_vector(7 downto 0);


signal com_op_out : std_logic_vector(7 downto 0);
signal and_op_out : std_logic_vector(7 downto 0);
signal or_op_out : std_logic_vector(7 downto 0);
signal eor_op_out : std_logic_vector(7 downto 0);

-- SHIFT SIGNALS
signal right_shift_out : std_logic_vector(7 downto 0);

-- SWAP SIGNALS
signal swap_out : std_logic_vector(7 downto 0);

begin

	
-- ########################################################################
-- ###############              ALU
-- ########################################################################

adder_nadd_sub <=(operation.is_sub or operation.is_subi or operation.is_sbc or operation.is_sbci or operation.is_sbiw or sbiw_st or
                  operation.is_cp or operation.is_cpc or operation.is_cpi or operation.is_cpse ); -- '0' -> '+'; '1' -> '-' 

-- SREG C FLAG (ALU INPUT)
alu_c_flag_in_int <= alu_c_flag_in and 
(operation.is_adc or adiw_st or operation.is_sbc or operation.is_sbci or sbiw_st or 
operation.is_cpc or
operation.is_ror);
                                          
-- SREG Z FLAG ()
-- flags_out.z<= (alu_z_flag_out_int and not(adiw_st or sbiw_st)) or 
--                   ((alu_z_flag_in and alu_z_flag_out_int) and (adiw_st or sbiw_st));
flags_out.z<= (alu_z_flag_out_int and not(adiw_st or sbiw_st or operation.is_cpc or operation.is_sbc or operation.is_sbci)) or 
                  ((alu_z_flag_in and alu_z_flag_out_int) and (adiw_st or sbiw_st))or
				  (alu_z_flag_in and alu_z_flag_out_int and(operation.is_cpc or operation.is_sbc or operation.is_sbci));   -- Previous value (for CPC/SBC/SBCI instructions)

-- SREG N FLAG
flags_out.n<= alu_n_flag_out_int;				  
				  
-- SREG V FLAG
flags_out.v<= alu_v_flag_out_int;				  


flags_out.c<= alu_c_flag_out_int;				  

alu_data_out <= alu_data_out_int;

-- #########################################################################################

adder_d_in <= '0'&alu_data_d_in;
adder_r_in <= '0'&alu_data_r_in;  

--########################## ADDEER ###################################

adder_out(0) <= adder_d_in(0) xor adder_r_in(0) xor alu_c_flag_in_int;
adder_carry(0) <= ((adder_d_in(0) xor adder_nadd_sub) and adder_r_in(0)) or
                (((adder_d_in(0) xor adder_nadd_sub) or adder_r_in(0)) and alu_c_flag_in_int);

summator:for i in 1 to 8 generate
adder_out(i) <= adder_d_in(i) xor adder_r_in(i) xor adder_carry(i-1);
adder_carry(i) <= ((adder_d_in(i) xor adder_nadd_sub) and adder_r_in(i)) or 
                (((adder_d_in(i) xor adder_nadd_sub) or adder_r_in(i)) and adder_carry(i-1));
end generate;

-- FLAGS  FOR ADDER INSTRUCTIONS: 
-- CARRY FLAG (C) -> adder_out(8)
-- HALF CARRY FLAG (H) -> adder_carry(3)
-- TOW'S COMPLEMENT OVERFLOW  (V) -> 

adder_v_flag_out <= (((adder_d_in(7) and adder_r_in(7) and not adder_out(7)) or 
                    (not adder_d_in(7) and not adder_r_in(7) and adder_out(7))) and not adder_nadd_sub) or -- ADD
                    (((adder_d_in(7) and not adder_r_in(7) and not adder_out(7)) or
					(not adder_d_in(7) and adder_r_in(7) and adder_out(7))) and adder_nadd_sub);
																										   -- SUB
--#####################################################################


-- LOGICAL OPERATIONS FOR ONE OPERAND

--########################## NEG OPERATION ####################

neg_op_out(0)   <= not alu_data_d_in(0) xor '1';
neg_op_carry(0) <= not alu_data_d_in(0) and '1';

neg_op:for i in 1 to 7 generate
neg_op_out(i)   <= not alu_data_d_in(i) xor neg_op_carry(i-1);
neg_op_carry(i) <= not alu_data_d_in(i) and neg_op_carry(i-1);
end generate;

neg_op_out(8) <= neg_op_carry(7) xor '1';
neg_op_carry(8) <= neg_op_carry(7);                            -- ??!!

-- CARRY FLAGS  FOR NEG INSTRUCTION: 
-- CARRY FLAG -> neg_op_out(8)
-- HALF CARRY FLAG -> neg_op_carry(3)
-- TOW's COMPLEMENT OVERFLOW FLAG -> alu_data_d_in(7) and neg_op_carry(6) 
--############################################################################	


--########################## INC, DEC OPERATIONS ####################

incdec_op_out(0)      <=  alu_data_d_in(0) xor '1';
incdec_op_carry(0)    <=  alu_data_d_in(0) xor operation.is_dec;

inc_dec:for i in 1 to 7 generate
incdec_op_out(i)   <= alu_data_d_in(i) xor incdec_op_carry(i-1);
incdec_op_carry(i) <= (alu_data_d_in(i) xor operation.is_dec) and incdec_op_carry(i-1);
end generate;

-- TOW's COMPLEMENT OVERFLOW FLAG -> (alu_data_d_in(7) xor operation.is_dec) and incdec_op_carry(6) 
--####################################################################


--########################## COM OPERATION ###################################
com_op_out <= not alu_data_d_in;
-- FLAGS 
-- TOW's COMPLEMENT OVERFLOW FLAG (V)  -> '0'
-- CARRY FLAG (C) -> '1' 
--############################################################################

-- LOGICAL OPERATIONS FOR TWO OPERANDS	

--########################## AND OPERATION ###################################
and_op_out <= alu_data_d_in and alu_data_r_in;
-- FLAGS 
-- TOW's COMPLEMENT OVERFLOW FLAG (V)  -> '0'
--############################################################################

--########################## OR OPERATION ###################################
or_op_out <= alu_data_d_in or alu_data_r_in;
-- FLAGS 
-- TOW's COMPLEMENT OVERFLOW FLAG (V)  -> '0'
--############################################################################

--########################## EOR OPERATION ###################################
eor_op_out <= alu_data_d_in xor alu_data_r_in;
-- FLAGS 
-- TOW's COMPLEMENT OVERFLOW FLAG (V)  -> '0'
--############################################################################

-- SHIFT OPERATIONS 

-- ########################## RIGHT(LSR, ROR, ASR) #######################

right_shift_out(7) <= (operation.is_ror and alu_c_flag_in_int) or (operation.is_asr and alu_data_d_in(7)); -- right_shift_out(7)
shift_right:for i in 6 downto 0 generate
right_shift_out(i) <= alu_data_d_in(i+1);
end generate;	

-- FLAGS 
-- CARRY FLAG (C)                      -> alu_data_d_in(0) 
-- NEGATIVE FLAG (N)                   -> right_shift_out(7)
-- TOW's COMPLEMENT OVERFLOW FLAG (V)  -> N xor C  (left_shift_out(7) xor alu_data_d_in(0))

-- #######################################################################


-- ################################## SWAP ###############################

swap_h:for i in 7 downto 4 generate
swap_out(i) <= alu_data_d_in(i-4);
end generate;
swap_l:for i in 3 downto 0 generate
swap_out(i) <= alu_data_d_in(i+4);
end generate;
-- #######################################################################

-- ALU OUTPUT MUX

alu_data_out_mux:for i in alu_data_out_int'range generate
alu_data_out_int(i) <= (adder_out(i) and (operation.is_add or operation.is_adc or (operation.is_adiw or adiw_st) or    -- !!!!!
                                     operation.is_sub or operation.is_subi or operation.is_sbc or operation.is_sbci or
                                     (operation.is_sbiw or sbiw_st) or    -- !!!!!
                                     operation.is_cpse or operation.is_cp or operation.is_cpc or operation.is_cpi)) or 
                                     (neg_op_out(i) and operation.is_neg) or                               -- NEG
                                     (incdec_op_out(i) and (operation.is_inc or operation.is_dec)) or               -- INC/DEC
                                     (com_op_out(i) and operation.is_com) or                               -- COM
                                     (and_op_out(i) and (operation.is_and or operation.is_andi)) or                 -- AND/ANDI                                   
                                     (or_op_out(i)  and (operation.is_or or operation.is_ori)) or                   -- OR/ORI                                   
                                     (eor_op_out(i) and operation.is_eor) or                               -- EOR
                                     (right_shift_out(i) and (operation.is_lsr or operation.is_ror or operation.is_asr)) or  -- LSR/ROR/ASR
                                     (swap_out(i) and operation.is_swap);                                  -- SWAP

                                     
end generate;

--@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ALU FLAGS OUTPUTS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

flags_out.h<= (adder_carry(3) and                                                      -- ADDER INSTRUCTIONS
             (operation.is_add or operation.is_adc or operation.is_sub or operation.is_subi or operation.is_sbc or operation.is_sbci or operation.is_cp or operation.is_cpc or operation.is_cpi)) or   
             (not neg_op_carry(3) and operation.is_neg); -- H-flag problem with NEG instruction fixing                                         -- NEG
             
             
flags_out.s<= alu_n_flag_out_int xor alu_v_flag_out_int;

alu_v_flag_out_int <= (adder_v_flag_out and	               
             (operation.is_add or operation.is_adc or operation.is_sub or operation.is_subi or operation.is_sbc or operation.is_sbci or adiw_st or sbiw_st or operation.is_cp or operation.is_cpi or operation.is_cpc)) or
             ((alu_data_d_in(7) and neg_op_carry(6)) and operation.is_neg) or                                       -- NEG
		     (not alu_data_d_in(7) and incdec_op_carry(6) and operation.is_inc) or -- INC
		     (alu_data_d_in(7) and incdec_op_carry(6) and operation.is_dec) or	  -- DEC
			 ((alu_n_flag_out_int xor alu_c_flag_out_int) and (operation.is_lsr or operation.is_ror or operation.is_asr));            -- LSR,ROR,ASR


alu_n_flag_out_int <= alu_data_out_int(7);

alu_z_flag_out_int <= '1' when alu_data_out_int="00000000" else '0';

alu_c_flag_out_int <= (adder_out(8) and 
                       (operation.is_add or operation.is_adc or (operation.is_adiw or adiw_st) or operation.is_sub or operation.is_subi or operation.is_sbc or operation.is_sbci or (operation.is_sbiw or sbiw_st) or operation.is_cp or operation.is_cpc or operation.is_cpi)) or -- ADDER
					   (not alu_z_flag_out_int and operation.is_neg) or    -- NEG
					   (alu_data_d_in(0) and (operation.is_lsr or operation.is_ror or operation.is_asr)) or operation.is_com;

-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


end rtl;
