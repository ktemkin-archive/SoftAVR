--************************************************************************************************
--  Flag decoder-- determines the write enables for each of the flags.
--  (ATMega103 compatible.)
--
--  Authors:
--      -- Kyle J. Temkin, Binghamton University, <ktemkin@binghamton.edu>
--      -- Ruslan Leptenok, original core designer
--************************************************************************************************

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library avr;
use avr.instruction.all;
use avr.flags.all;

entity flag_decoder is port(

  --Specifies which operation is currently being performed.
  operation : in decoded_operation;
  second_cycle_of_adiw : in std_ulogic;
  second_cycle_of_sbiw : in std_ulogic;
  first_cycle_of_irq   : in std_ulogic;
  third_cycle_of_reti  : in std_ulogic;
  
  --The target bit number for bset/bclr functions. Has no meaning
  --when the active operation is not bset/bclr.
  bit_operation_bit_number : in std_logic_vector(2 downto 0); --replace me with bit_number

  -- A record indicating which flags are affected by the active operation.
  flags_affected : out extended_flag_set
);
end entity;

architecture functional of flag_decoder is

  --Convenience signals which determine whether the operation belongs
  --to one of several standard "classes" of operation, which all affect 
  --the same flags.
  signal is_arithmetic_operation : std_ulogic;
  signal is_logical_operation    : std_ulogic;
  signal is_bit_operation        : std_ulogic;

  -- Determines if a given flag is being addressed directly by bset/bclr.
  signal flag_set_directly       : serialized_flag_set;

begin


  --
  -- Determine if the given operation is designed to adjust the flag's value directly.
  -- Only the bset/bclr instructions directly affect the flags. Note that SEC/etc pseudo-ops
  -- are just aliases for bset or bclr.
  --
  
  is_bit_operation <= operation.is_bclr or operation.is_bset;

  DETERMINE_DIRECT_BIT_NUMBER:
  for i in flag_set_directly'range generate
    flag_set_directly(i) <= '1' when (to_integer(unsigned(bit_operation_bit_number)) = i) and (is_bit_operation = '1') else '0';
  end generate;

  --Convenience signal which indicates whether the given operation is an arithmetic operation.
  --Arithmetic operations set the S, V, N, Z, and C flag.
  is_arithmetic_operation <=
    operation.is_add     or
    operation.is_adc     or
    second_cycle_of_adiw or
    operation.is_sub     or
    operation.is_subi    or
    operation.is_sbc     or
    operation.is_sbci    or
    second_cycle_of_sbiw or
    operation.is_com     or
    operation.is_neg     or
    operation.is_cp      or
    operation.is_cpc     or
    operation.is_cpi     or
    operation.is_lsr     or
    operation.is_ror     or
    operation.is_asr; 

  -- Convenience signal which indicates whether the given operation is an logical
  -- operation. Logical operations set the S, V, N, and Z flags.
  is_logical_operation <=
    operation.is_and   or
    operation.is_andi  or
    operation.is_or    or
    operation.is_ori   or
    operation.is_eor; 

  --
  -- Determines if the interrupt "pseudo-flag" is affected by the given operation.
  -- 
  flags_affected.i <= 
    first_cycle_of_irq   or
    third_cycle_of_reti  or
    flag_set_directly(flag_position_i); 

  --
  -- Determines if the bit transfer "pseudo-flag" is affected by the given operation.
  --
  flags_affected.t <=  
    operation.is_bst   or
    flag_set_directly(flag_position_t);

  --
  -- Determines if the given operation affects the half carry flag.
  -- 
  flags_affected.h <= 
    operation.is_add         or
    operation.is_adc         or
    operation.is_sub         or
    operation.is_subi        or
    operation.is_sbc         or
    operation.is_sbci        or
    operation.is_neg         or
    flag_set_directly(flag_position_h);

  --
  -- Determines if the given operation affects the signed test flag.
  -- 
  flags_affected.s <= 
    is_arithmetic_operation  or
    is_logical_operation     or
    operation.is_inc         or
    operation.is_dec         or
    flag_set_directly(flag_position_s);

  --
  -- Determines if the given operation affects the signed overflow flag.
  -- 
  flags_affected.v <= 
    is_arithmetic_operation  or
    is_logical_operation     or
    operation.is_inc         or
    operation.is_dec         or
    flag_set_directly(flag_position_v);

  --
  -- Determines if the given operation affects the negative flag.
  --
  flags_affected.n <= 
    is_arithmetic_operation  or
    is_logical_operation     or
    operation.is_inc         or
    operation.is_dec         or
    flag_set_directly(flag_position_n);

  --
  -- Determines if the given operation affects the zero flag.
  --
  flags_affected.z <= 
    is_arithmetic_operation  or
    is_logical_operation     or
    operation.is_inc         or
    operation.is_dec         or
    flag_set_directly(flag_position_z);

  --
  -- Determines if the given operation affects the carry flag.
  -- 
  flags_affected.c <= 
    is_arithmetic_operation  or
    operation.is_adiw        or
    operation.is_sbiw        or
    flag_set_directly(flag_position_c);

end architecture;
