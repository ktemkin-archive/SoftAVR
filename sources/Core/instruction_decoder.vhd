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

  --
  -- Function that determines whether a given encoded instruction matches an instruction specification.
  -- The instruction specification should include Don't Cares for all bits that vary inside of an instruction.
  --
  function matches_form(instruction : encoded_instruction; general_form : encoded_instruction) return std_ulogic is
  begin
   
    --If the instruction matches the general form, then return one; otherwise, return '0'.
    --These values are different than their Booelan counterparts, so we can't return std_match's 
    --result directly.
    if std_match(instruction, general_form) then
      return '1';
    else
      return '0';
    end if;

  end function;

begin

  --
  -- Core instruction decoder.
  --
  -- As the AVR family has a complex instruction form, we can't easily sepearate the opcode
  -- from its operands. Instead, we use this decoder to determine if a given instruction matches
  -- the "form" for each instruction. See the AVR Instruction Set datasheet for the machine code
  -- meaning for each of the instruction forms below.
  --

  operation.is_adc   <= matches_form(instruction, "000111----------");
  operation.is_add   <= matches_form(instruction, "000011----------");
  operation.is_adiw  <= matches_form(instruction, "10010110--------");
  operation.is_and   <= matches_form(instruction, "001000----------");
  operation.is_andi  <= matches_form(instruction, "0111------------");
  operation.is_asr   <= matches_form(instruction, "1001010-----0101");
  operation.is_bclr  <= matches_form(instruction, "100101001---1000");
  operation.is_bld   <= matches_form(instruction, "1111100-----0---");
  operation.is_brbc  <= matches_form(instruction, "111101----------");
  operation.is_brbs  <= matches_form(instruction, "111100----------");
  operation.is_bset  <= matches_form(instruction, "100101000---1000");
  operation.is_bst   <= matches_form(instruction, "1111101---------");
  operation.is_call  <= matches_form(instruction, "1001010-----111-");
  operation.is_cbi   <= matches_form(instruction, "10011000--------");
  operation.is_com   <= matches_form(instruction, "1001010-----0000");
  operation.is_cp    <= matches_form(instruction, "000101----------");
  operation.is_cpc   <= matches_form(instruction, "000001----------");
  operation.is_cpi   <= matches_form(instruction, "0011------------");
  operation.is_cpse  <= matches_form(instruction, "000100----------");
  operation.is_dec   <= matches_form(instruction, "1001010-----1010");
  operation.is_elpm  <= matches_form(instruction, "1001010111011000");
  operation.is_eor   <= matches_form(instruction, "001001----------");
  operation.is_icall <= matches_form(instruction, "10010101----1001");
  operation.is_ijmp  <= matches_form(instruction, "10010100----1001");
  operation.is_in    <= matches_form(instruction, "10110-----------");
  operation.is_inc   <= matches_form(instruction, "1001010-----0011");
  operation.is_jmp   <= matches_form(instruction, "1001010-----110-");
  --TODO: potentially seperate the indexed and non-indexed form of these instructions.
  operation.is_ld_x  <= matches_form(instruction, "1001000-----110-") or matches_form(instruction, "1001000-----1110");
  operation.is_ld_y  <= matches_form(instruction, "1001000-----1001") or matches_form(instruction, "1001000-----1010");
  operation.is_ldd_y <= matches_form(instruction, "10-0--0-----1---");
  operation.is_ld_z  <= matches_form(instruction, "1001000-----0001") or matches_form(instruction, "1001000-----0010");
  operation.is_ldd_z <= matches_form(instruction, "10-0--0-----0---");
  operation.is_ldi   <= matches_form(instruction, "1110------------");
  operation.is_lds   <= matches_form(instruction, "1001000-----0000");
  operation.is_lpm   <= matches_form(instruction, "1001010111001000");
  operation.is_lsr   <= matches_form(instruction, "1001010-----0110");
  operation.is_mov   <= matches_form(instruction, "001011----------");
  operation.is_mul   <= matches_form(instruction, "100111----------");
  operation.is_neg   <= matches_form(instruction, "1001010-----0001");
  operation.is_nop   <= matches_form(instruction, "0000000000000000");
  operation.is_or    <= matches_form(instruction, "001010----------");
  operation.is_ori   <= matches_form(instruction, "0110------------");
  operation.is_out   <= matches_form(instruction, "10111-----------");
  operation.is_pop   <= matches_form(instruction, "1001000-----1111");
  operation.is_push  <= matches_form(instruction, "1001001-----1111");
  operation.is_rcall <= matches_form(instruction, "1101------------");
  operation.is_ret   <= matches_form(instruction, "100101010--01000");
  operation.is_reti  <= matches_form(instruction, "100101010--11000");
  operation.is_rjmp  <= matches_form(instruction, "1100------------");
  operation.is_ror   <= matches_form(instruction, "1001010-----0111");
  operation.is_sbc   <= matches_form(instruction, "000010----------");
  operation.is_sbci  <= matches_form(instruction, "0100------------");
  operation.is_sbi   <= matches_form(instruction, "10011010--------");
  operation.is_sbic  <= matches_form(instruction, "10011001--------");
  operation.is_sbis  <= matches_form(instruction, "10011011--------");
  operation.is_sbiw  <= matches_form(instruction, "10010111--------");
  operation.is_sbrc  <= matches_form(instruction, "1111110---------");
  operation.is_sbrs  <= matches_form(instruction, "1111111---------");
  operation.is_sleep <= matches_form(instruction, "10010101100-1000");
  operation.is_st_x  <= matches_form(instruction, "1001001-----110-") or matches_form(instruction, "1001001-----1110");
  operation.is_st_y  <= matches_form(instruction, "1001001-----1001") or matches_form(instruction, "1001001-----1010");
  operation.is_std_y <= matches_form(instruction, "10-0--1-----1---");
  operation.is_st_z  <= matches_form(instruction, "1001001-----0001") or matches_form(instruction, "1001001-----0010");
  operation.is_std_z <= matches_form(instruction, "10-0--1-----0---");
  operation.is_sts   <= matches_form(instruction, "1001001-----0000");
  operation.is_sub   <= matches_form(instruction, "000110----------");
  operation.is_subi  <= matches_form(instruction, "0101------------");
  operation.is_swap  <= matches_form(instruction, "1001010-----0010");
  operation.is_wdr   <= matches_form(instruction, "10010101101-1000");

end Behavioral;

