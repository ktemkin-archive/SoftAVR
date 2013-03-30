--************************************************************************************************
--  AVR ALU
--  (ATMega103 compatible.)
--
--  Authors:
--      -- Kyle J. Temkin, Binghamton University, <ktemkin@binghamton.edu>
--      -- Ruslan Lepetenok, original core designer
--************************************************************************************************

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.AVRucPackage.all;

library avr;
use avr.instruction.all;

entity instruction_decoder is port(

  -- The machine code for the current instruction, as the
  -- the 16-bits pointed to by the program counter.
  instruction : in encoded_instruction;

  -- A "decoded" one-hot version of the current instruction. 
  -- Contains a range of "is_<instruction>" signals, which indicate the type of 
  -- the current instruction.
  operation   : out decoded_operation

);
end instruction_decoder;

architecture Behavioral of instruction_decoder is
begin

  --
  -- Core instruction decoder.
  --
  -- As the AVR family has a complex instruction form, we can't easily sepearate the opcode
  -- from its operands. Instead, we use this decoder to determine if a given instruction matches
  -- the "form" for each instruction. See the AVR Instruction Set datasheet for the machine code
  -- meaning for each of the instruction forms below.
  --

  operation.is_adc   <= matches_form(instruction, OPCODE_ADC);
  operation.is_add   <= matches_form(instruction, OPCODE_ADD);
  operation.is_adiw  <= matches_form(instruction, OPCODE_ADIW);
  operation.is_and   <= matches_form(instruction, OPCODE_AND);
  operation.is_andi  <= matches_form(instruction, OPCODE_ANDI);
  operation.is_asr   <= matches_form(instruction, OPCODE_ASR);
  operation.is_bclr  <= matches_form(instruction, OPCODE_BCLR);
  operation.is_bld   <= matches_form(instruction, OPCODE_BLD);
  operation.is_brbc  <= matches_form(instruction, OPCODE_BRBC);
  operation.is_brbs  <= matches_form(instruction, OPCODE_BRBS);
  operation.is_bset  <= matches_form(instruction, OPCODE_BSET);
  operation.is_bst   <= matches_form(instruction, OPCODE_BST);
  operation.is_call  <= matches_form(instruction, OPCODE_CALL);
  operation.is_cbi   <= matches_form(instruction, OPCODE_CBI);
  operation.is_com   <= matches_form(instruction, OPCODE_COM);
  operation.is_cp    <= matches_form(instruction, OPCODE_CP);
  operation.is_cpc   <= matches_form(instruction, OPCODE_CPC);
  operation.is_cpi   <= matches_form(instruction, OPCODE_CPI);
  operation.is_cpse  <= matches_form(instruction, OPCODE_CPSE);
  operation.is_dec   <= matches_form(instruction, OPCODE_DEC);
  operation.is_elpm  <= matches_form(instruction, OPCODE_ELPM);
  operation.is_eor   <= matches_form(instruction, OPCODE_EOR);
  operation.is_icall <= matches_form(instruction, OPCODE_ICALL);
  operation.is_ijmp  <= matches_form(instruction, OPCODE_IJMP);
  operation.is_in    <= matches_form(instruction, OPCODE_IN);
  operation.is_inc   <= matches_form(instruction, OPCODE_INC);
  operation.is_jmp   <= matches_form(instruction, OPCODE_JMP);
  --TODO: Potentially seperate the indexed and non-indexed form of these instructions?
  operation.is_ld_x  <= matches_form(instruction, OPCODE_LD_X_POST_INCREMENT) or matches_form(instruction, OPCODE_LD_X_PRE_DECREMENT);
  operation.is_ld_y  <= matches_form(instruction, OPCODE_LD_Y_POST_INCREMENT) or matches_form(instruction, OPCODE_LD_Y_PRE_DECREMENT);
  operation.is_ldd_y <= matches_form(instruction, OPCODE_LDD_y);
  operation.is_ld_z  <= matches_form(instruction, OPCODE_LD_Z_POST_INCREMENT) or matches_form(instruction, OPCODE_LD_Z_PRE_DECREMENT);
  operation.is_ldd_z <= matches_form(instruction, OPCODE_LDD_Z);
  operation.is_ldi   <= matches_form(instruction, OPCODE_LDI);
  operation.is_lds   <= matches_form(instruction, OPCODE_LDS);
  operation.is_lpm   <= matches_form(instruction, OPCODE_LPM);
  operation.is_lsr   <= matches_form(instruction, OPCODE_LSR);
  operation.is_mov   <= matches_form(instruction, OPCODE_MOV);
  operation.is_mul   <= matches_form(instruction, OPCODE_MUL);
  operation.is_neg   <= matches_form(instruction, OPCODE_NEG);
  operation.is_nop   <= matches_form(instruction, OPCODE_NOP);
  operation.is_or    <= matches_form(instruction, OPCODE_OR);
  operation.is_ori   <= matches_form(instruction, OPCODE_ORI);
  operation.is_out   <= matches_form(instruction, OPCODE_OUT);
  operation.is_pop   <= matches_form(instruction, OPCODE_POP);
  operation.is_push  <= matches_form(instruction, OPCODE_PUSH);
  operation.is_rcall <= matches_form(instruction, OPCODE_RCALL);
  operation.is_ret   <= matches_form(instruction, OPCODE_RET);
  operation.is_reti  <= matches_form(instruction, OPCODE_RETI);
  operation.is_rjmp  <= matches_form(instruction, OPCODE_RJMP);
  operation.is_ror   <= matches_form(instruction, OPCODE_ROR);
  operation.is_sbc   <= matches_form(instruction, OPCODE_SBC);
  operation.is_sbci  <= matches_form(instruction, OPCODE_SBCI);
  operation.is_sbi   <= matches_form(instruction, OPCODE_SBI);
  operation.is_sbic  <= matches_form(instruction, OPCODE_SBIC);
  operation.is_sbis  <= matches_form(instruction, OPCODE_SBIS);
  operation.is_sbiw  <= matches_form(instruction, OPCODE_SBIW);
  operation.is_sbrc  <= matches_form(instruction, OPCODE_SBRC);
  operation.is_sbrs  <= matches_form(instruction, OPCODE_SBRS);
  operation.is_sleep <= matches_form(instruction, OPCODE_SLEEP);
  operation.is_st_x  <= matches_form(instruction, OPCODE_ST_X_POST_INCREMENT) or matches_form(instruction, OPCODE_ST_X_PRE_DECREMENT);
  operation.is_st_y  <= matches_form(instruction, OPCODE_ST_Y_POST_INCREMENT) or matches_form(instruction, OPCODE_ST_Y_PRE_DECREMENT);
  operation.is_std_y <= matches_form(instruction, OPCODE_STD_Y);
  operation.is_st_z  <= matches_form(instruction, OPCODE_ST_Z_POST_INCREMENT) or matches_form(instruction, OPCODE_ST_Z_PRE_DECREMENT);
  operation.is_std_z <= matches_form(instruction, OPCODE_STD_Z);
  operation.is_sts   <= matches_form(instruction, OPCODE_STS);
  operation.is_sub   <= matches_form(instruction, OPCODE_SUB);
  operation.is_subi  <= matches_form(instruction, OPCODE_SUBI);
  operation.is_swap  <= matches_form(instruction, OPCODE_SWAP);
  operation.is_wdr   <= matches_form(instruction, OPCODE_WDR);

end Behavioral;

