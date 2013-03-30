--************************************************************************************************
--  Generic AVR instruction set definitions.
--
--  Authors:
--      -- Kyle J. Temkin, Binghamton University, <ktemkin@binghamton.edu>
--************************************************************************************************

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package instruction is


  subtype encoded_instruction is std_logic_vector(15 downto 0);

  --
  -- Function that determines whether a given encoded instruction matches an instruction specification.
  -- The instruction specification should include Don't Cares for all bits that vary inside of an instruction.
  --
  function matches_form(instruction : encoded_instruction; general_form : encoded_instruction) return std_ulogic;

  --
  -- Opcode "forms" for each of the AVR operations.
  --
  -- As the AVR family has a complex instruction form, we can't easily sepearate the opcode
  -- from its operands. Instead, the patterns below define the "form" for each instruction, where
  -- each "-" indicates a bit used to store an operand. See the AVR Instruction Set datasheet 
  -- for the machine code meaning for each of the instruction forms below.
  --
  constant OPCODE_ADC                 : encoded_instruction := "000111----------";
  constant OPCODE_ADD                 : encoded_instruction := "000011----------";
  constant OPCODE_ADIW                : encoded_instruction := "10010110--------";
  constant OPCODE_AND                 : encoded_instruction := "001000----------";
  constant OPCODE_ANDI                : encoded_instruction := "0111------------";
  constant OPCODE_ASR                 : encoded_instruction := "1001010-----0101";
  constant OPCODE_BCLR                : encoded_instruction := "100101001---1000";
  constant OPCODE_BLD                 : encoded_instruction := "1111100-----0---";
  constant OPCODE_BRBC                : encoded_instruction := "111101----------";
  constant OPCODE_BRBS                : encoded_instruction := "111100----------";
  constant OPCODE_BSET                : encoded_instruction := "100101000---1000";
  constant OPCODE_BST                 : encoded_instruction := "1111101---------";
  constant OPCODE_CALL                : encoded_instruction := "1001010-----111-";
  constant OPCODE_CBI                 : encoded_instruction := "10011000--------";
  constant OPCODE_COM                 : encoded_instruction := "1001010-----0000";
  constant OPCODE_CP                  : encoded_instruction := "000101----------";
  constant OPCODE_CPC                 : encoded_instruction := "000001----------";
  constant OPCODE_CPI                 : encoded_instruction := "0011------------";
  constant OPCODE_CPSE                : encoded_instruction := "000100----------";
  constant OPCODE_DEC                 : encoded_instruction := "1001010-----1010";
  constant OPCODE_ELPM                : encoded_instruction := "1001010111011000";
  constant OPCODE_EOR                 : encoded_instruction := "001001----------";
  constant OPCODE_ICALL               : encoded_instruction := "10010101----1001";
  constant OPCODE_IJMP                : encoded_instruction := "10010100----1001";
  constant OPCODE_IN                  : encoded_instruction := "10110-----------";
  constant OPCODE_INC                 : encoded_instruction := "1001010-----0011";
  constant OPCODE_JMP                 : encoded_instruction := "1001010-----110-";
  constant OPCODE_LD_X_POST_INCREMENT : encoded_instruction := "1001000-----110-";
  constant OPCODE_LD_X_PRE_DECREMENT  : encoded_instruction := "1001000-----1110";
  constant OPCODE_LD_Y_POST_INCREMENT : encoded_instruction := "1001000-----1001";
  constant OPCODE_LD_Y_PRE_DECREMENT  : encoded_instruction := "1001000-----1010";
  constant OPCODE_LDD_Y               : encoded_instruction := "10-0--0-----1---";
  constant OPCODE_LD_Z_POST_INCREMENT : encoded_instruction := "1001000-----0001";
  constant OPCODE_LD_Z_PRE_DECREMENT  : encoded_instruction := "1001000-----0010";
  constant OPCODE_LDD_Z               : encoded_instruction := "10-0--0-----0---";
  constant OPCODE_LDI                 : encoded_instruction := "1110------------";
  constant OPCODE_LDS                 : encoded_instruction := "1001000-----0000";
  constant OPCODE_LPM                 : encoded_instruction := "1001010111001000";
  constant OPCODE_LSR                 : encoded_instruction := "1001010-----0110";
  constant OPCODE_MOV                 : encoded_instruction := "001011----------";
  constant OPCODE_MUL                 : encoded_instruction := "100111----------";
  constant OPCODE_NEG                 : encoded_instruction := "1001010-----0001";
  constant OPCODE_NOP                 : encoded_instruction := "0000000000000000";
  constant OPCODE_OR                  : encoded_instruction := "001010----------";
  constant OPCODE_ORI                 : encoded_instruction := "0110------------";
  constant OPCODE_OUT                 : encoded_instruction := "10111-----------";
  constant OPCODE_POP                 : encoded_instruction := "1001000-----1111";
  constant OPCODE_PUSH                : encoded_instruction := "1001001-----1111";
  constant OPCODE_RCALL               : encoded_instruction := "1101------------";
  constant OPCODE_RET                 : encoded_instruction := "100101010--01000";
  constant OPCODE_RETI                : encoded_instruction := "100101010--11000";
  constant OPCODE_RJMP                : encoded_instruction := "1100------------";
  constant OPCODE_ROR                 : encoded_instruction := "1001010-----0111";
  constant OPCODE_SBC                 : encoded_instruction := "000010----------";
  constant OPCODE_SBCI                : encoded_instruction := "0100------------";
  constant OPCODE_SBI                 : encoded_instruction := "10011010--------";
  constant OPCODE_SBIC                : encoded_instruction := "10011001--------";
  constant OPCODE_SBIS                : encoded_instruction := "10011011--------";
  constant OPCODE_SBIW                : encoded_instruction := "10010111--------";
  constant OPCODE_SBRC                : encoded_instruction := "1111110---------";
  constant OPCODE_SBRS                : encoded_instruction := "1111111---------";
  constant OPCODE_SLEEP               : encoded_instruction := "10010101100-1000";
  constant OPCODE_ST_X_POST_INCREMENT : encoded_instruction := "1001001-----110-";
  constant OPCODE_ST_X_PRE_DECREMENT  : encoded_instruction := "1001001-----1110";
  constant OPCODE_ST_Y_POST_INCREMENT : encoded_instruction := "1001001-----1001";
  constant OPCODE_ST_Y_PRE_DECREMENT  : encoded_instruction := "1001001-----1010";
  constant OPCODE_STD_Y               : encoded_instruction := "10-0--1-----1---";
  constant OPCODE_ST_Z_POST_INCREMENT : encoded_instruction := "1001001-----0001";
  constant OPCODE_ST_Z_PRE_DECREMENT  : encoded_instruction := "1001001-----0010";
  constant OPCODE_STD_Z               : encoded_instruction := "10-0--1-----0---";
  constant OPCODE_STS                 : encoded_instruction := "1001001-----0000";
  constant OPCODE_SUB                 : encoded_instruction := "000110----------";
  constant OPCODE_SUBI                : encoded_instruction := "0101------------";
  constant OPCODE_SWAP                : encoded_instruction := "1001010-----0010";
  constant OPCODE_WDR                 : encoded_instruction := "10010101101-1000";

  -- Decoded operation, which contains a gamut of "boolean" signals indicating
  -- the operation type decoded from an instruction.
  type decoded_operation is record
     is_adc     : std_ulogic; -- INSTRUCTION ADC
     is_add     : std_ulogic; -- INSTRUCTION ADD
     is_adiw    : std_ulogic; -- INSTRUCTION ADIW
     is_and     : std_ulogic; -- INSTRUCTION AND
     is_andi    : std_ulogic; -- INSTRUCTION ANDI
     is_asr     : std_ulogic; -- INSTRUCTION ASR

     is_bclr    : std_ulogic; -- INSTRUCTION BCLR
     is_bld     : std_ulogic; -- INSTRUCTION BLD
     is_brbc    : std_ulogic; -- INSTRUCTION BRBC
     is_brbs    : std_ulogic; -- INSTRUCTION BRBS
     is_bset    : std_ulogic; -- INSTRUCTION BSET
     is_bst     : std_ulogic; -- INSTRUCTION BST

     is_call    : std_ulogic; -- INSTRUCTION CALL
     is_cbi     : std_ulogic; -- INSTRUCTION CBI
     is_com     : std_ulogic; -- INSTRUCTION COM
     is_cp      : std_ulogic; -- INSTRUCTION CP
     is_cpc     : std_ulogic; -- INSTRUCTION CPC
     is_cpi     : std_ulogic; -- INSTRUCTION CPI
     is_cpse    : std_ulogic; -- INSTRUCTION CPSE

     is_dec     : std_ulogic; -- INSTRUCTION DEC

     is_elpm    : std_ulogic; -- INSTRUCTION ELPM
     is_eor     : std_ulogic; -- INSTRUCTION EOR

     is_icall   : std_ulogic; -- INSTRUCTION ICALL
     is_ijmp    : std_ulogic; -- INSTRUCTION IJMP

     is_in      : std_ulogic; -- INSTRUCTION IN
     is_inc     : std_ulogic; -- INSTRUCTION INC

     is_jmp     : std_ulogic; -- INSTRUCTION JMP

     is_ld_x    : std_ulogic; -- INSTRUCTION LD Rx,X ; LD Rx,X+ ;LD Rx,-X
     is_ld_y    : std_ulogic; -- INSTRUCTION LD Rx,Y ; LD Rx,Y+ ;LD Rx,-Y
     is_ldd_y   : std_ulogic; -- INSTRUCTION LDD Rx,Y+q
     is_ld_z    : std_ulogic; -- INSTRUCTION LD Rx,Z ; LD Rx,Z+ ;LD Rx,-Z
     is_ldd_z   : std_ulogic; -- INSTRUCTION LDD Rx,Z+q

     is_ldi     : std_ulogic; -- INSTRUCTION LDI
     is_lds     : std_ulogic; -- INSTRUCTION LDS
     is_lpm     : std_ulogic; -- INSTRUCTION LPM
     is_lsr     : std_ulogic; -- INSTRUCTION LSR

     is_mov     : std_ulogic; -- INSTRUCTION MOV
     is_mul     : std_ulogic; -- INSTRUCTION MUL

     is_neg     : std_ulogic; -- INSTRUCTION NEG
     is_nop     : std_ulogic; -- INSTRUCTION NOP

     is_or      : std_ulogic; -- INSTRUCTION OR
     is_ori     : std_ulogic; -- INSTRUCTION ORI
     is_out     : std_ulogic; -- INSTRUCTION OUT

     is_pop     : std_ulogic; -- INSTRUCTION POP
     is_push    : std_ulogic; -- INSTRUCTION PUSH

     is_rcall   : std_ulogic; -- INSTRUCTION RCALL
     is_ret     : std_ulogic; -- INSTRUCTION RET
     is_reti    : std_ulogic; -- INSTRUCTION RETI
     is_rjmp    : std_ulogic; -- INSTRUCTION RJMP
     is_ror     : std_ulogic; -- INSTRUCTION ROR

     is_sbc     : std_ulogic; -- INSTRUCTION SBC
     is_sbci    : std_ulogic; -- INSTRUCTION SBCI
     is_sbi     : std_ulogic; -- INSTRUCTION SBI
     is_sbic    : std_ulogic; -- INSTRUCTION SBIC
     is_sbis    : std_ulogic; -- INSTRUCTION SBIS
     is_sbiw    : std_ulogic; -- INSTRUCTION SBIW
     is_sbrc    : std_ulogic; -- INSTRUCTION SBRC
     is_sbrs    : std_ulogic; -- INSTRUCTION SBRS
     is_sleep   : std_ulogic; -- INSTRUCTION SLEEP

     is_st_x    : std_ulogic; -- INSTRUCTION LD X,Rx ; LD X+,Rx ;LD -X,Rx
     is_st_y    : std_ulogic; -- INSTRUCTION LD Y,Rx ; LD Y+,Rx ;LD -Y,Rx
     is_std_y   : std_ulogic; -- INSTRUCTION LDD Y+q,Rx
     is_st_z    : std_ulogic; -- INSTRUCTION LD Z,Rx ; LD Z+,Rx ;LD -Z,Rx
     is_std_z   : std_ulogic; -- INSTRUCTION LDD Z+q,Rx

     is_sts     : std_ulogic; -- INSTRUCTION STS
     is_sub     : std_ulogic; -- INSTRUCTION SUB
     is_subi    : std_ulogic; -- INSTRUCTION SUBI
     is_swap    : std_ulogic; -- INSTRUCTION SWAP

     is_wdr     : std_ulogic; -- INSTRUCTION WDR
  end record;


end;

package body instruction is

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

end;
