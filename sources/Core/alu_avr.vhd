 --************************************************************************************************
--  AVR ALU
--  (ATMega103 compatible.)
--
--  Authors:
--      -- Kyle J. Temkin, Binghamton University, <ktemkin@binghamton.edu>
--      -- Ruslan Lepetenok, original core designer
--************************************************************************************************

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_misc.all;
use IEEE.numeric_std.all;

use work.AVRucPackage.all;

-- 
-- Generic interface for an AVR ALU.
--
entity alu_avr is port(

  --"One-hot" record which specifies the current operation.
  --Contains a range of "is_<instruction>" signals, which indicate the type of 
  --the current instruction.
  operation             : in decoded_operation;

  --
  -- Additional signals which indicate whether we're in the second cycle of
  -- our two-cycle arithmetic instructions. While these are asserted, each
  -- entry in the decoded operation should be zero.
  -- 
  second_cycle_of_adiw  : in std_logic;
  second_cycle_of_sbiw  : in std_logic;

  --
  -- Primary alu operands. 
  -- Rd is used in every ALU operation;
  -- Rr is used in all binary operations.
  -- 
  rd_value              : in byte;
  rr_value              : in byte;

  --
  -- The current value of the system's flags.
  -- These are used for operations like ADIW.
  --
  flags_in              : in flag_set := (others => '0');

  --
  -- The result of the most recent ALU operation.
  --
  result                : buffer byte;

  -- ALU "flags".
  --
  -- These values determine the _input_ to each of the flag registers,
  -- and the next state of a given flag _if_ a relevant instruction is
  -- being performed. When an unrelated operation is being executed,
  -- these values are meaningless.
  flags_out       : buffer flag_set
);

end alu_avr;

architecture RTL of alu_avr is

  -- Operation category signals.
  -- These convenience signals are true iff the given operation
  -- belongs to a category of operations with similar properties.
  signal is_subtraction_operation                     : std_ulogic;   
  signal is_right_shift_operation                     : std_ulogic;
  signal is_carry_operation                           : std_logic;

  -- Operation flag properties.
  -- These convenience signals make an "observation" about how the
  -- given operation affects the flags.
  signal only_clears_z_flag                           : std_logic;
  signal operation_uses_carry                         : std_ulogic;
  signal operation_does_not_set_carry                 : std_ulogic;
  signal adder_sets_extended_flags                    : std_ulogic;
  
  -- Internal adder signals.
  signal operand_left                                 : byte;
  signal operand_right                                : byte;
  signal operand_left_for_carry                       : byte;
  signal adder_carry_in                               : std_logic_vector(8 downto 0);
  signal adder_out                                    : std_logic_vector(8 downto 0);
  signal v_flag_adder	                                : std_ulogic;

  -- Outputs of the individual functional units.
  signal com_operation, and_operation, or_operation   : byte; 
  signal eor_operation, swap_operation                : byte; 
  signal lsr_operation, asr_operation, ror_operation  : byte;

  -- Bit-flag indicating that the ALU result is zero.
  signal result_is_zero                               : std_ulogic;

  -- Flag outputs for individual functional units.
  signal v_flag_shift_right                           : std_ulogic;
  signal v_flag_add_operation, v_flag_sub_operation   : std_ulogic;
  
  -- Signal which determines when the internal adder signal
  -- should drive the output.
  signal use_adder_output                             : std_ulogic;


begin
    
  --Determine if the given operation is a subtraction operation,
  --which requires a slightly different adder setup.
  is_subtraction_operation <= 
    operation.is_sub   or
    operation.is_subi  or
    operation.is_sbc   or
    operation.is_sbci  or
    operation.is_cp    or
    operation.is_cpc   or
    operation.is_cpi   or
    operation.is_cpse  or
    operation.is_sbiw  or 
    operation.is_dec   or 
    operation.is_neg   or
    second_cycle_of_sbiw;

  --Convenience signal which determines when the adder output should be used.
  use_adder_output <= 
    is_subtraction_operation or
    operation.is_add         or 
    operation.is_adc         or 
    operation.is_adiw        or 
    operation.is_inc         or
    second_cycle_of_sbiw                  or
    second_cycle_of_adiw;

  --Convenience signal which determines when the adder's output should be used to
  --set the half-carry and V flags.
  adder_sets_extended_flags <= 
    operation.is_add  or 
    operation.is_adc  or 
    operation.is_sub  or 
    operation.is_subi or 
    operation.is_sbc  or 
    operation.is_sbci or 
    operation.is_cp   or 
    operation.is_cpc  or 
    operation.is_cpi  or
    operation.is_neg;

  --Convenience signal which (non-exhaustively) indicates 
  --operations that do not set the carry.
  operation_does_not_set_carry <= 
    operation.is_cpse or
    operation.is_dec  or
    operation.is_inc;

  -- Convenience signal which determines whether the given operation is a right-shift operation.
  is_right_shift_operation <= operation.is_lsr or operation.is_ror or operation.is_asr;

  --Convenience signals which determine whether the operation is a carry operation, and whether it uses the
  --current value of the carry flag.
  is_carry_operation <= operation.is_adc or operation.is_sbc or operation.is_sbci or operation.is_cpc;
  operation_uses_carry <= is_carry_operation or second_cycle_of_adiw or second_cycle_of_sbiw or operation.is_ror; 

  --Convenience signal which indicates whether the current operation is only capable of clearing the Z flag.
  --This is used to allow multi-byte "chaining" operations to leave the Z flag untounced on zero.
  --Note that ADC is not included, for what seem to be historical reasons.
  only_clears_z_flag <= operation.is_sbc or operation.is_sbci or operation.is_cpc;

  --
  -- Main ALU Adder
  -- Note: switching to a more optimized adder might increase throughput.
  --
                              
  --Set the carry in to the LSB of our carry adder to the carry in,
  --or zero if the operation does not use the carry.
  adder_carry_in(0) <= flags_in.c and operation_uses_carry;

  -- Left hand operand. When subtracting, this will be the minuend.
  operand_left <=
    --The NEG operation subtracts Rd from zero.
    x"00" when operation.is_neg = '1' else 
    --All other operations use Rd as the LH operand.
    rd_value;

  -- Right hand operation. When subtracting, this will be the subtrahend.
  operand_right <= 
    --The NEG operation subtracts Rd from zero...
    rd_value when operation.is_neg = '1' else
    -- The increment and decrement operations add/subtract one respectively.
    x"01" when (operation.is_inc or operation.is_dec) = '1' else
    --All other operations use Rr as the RH operand.
    rr_value; 
                                            
  -- If we have a subtraction operation, use the inverted value of Rd
  -- to compute the carry out. This is a (slightly optimized) way to
  -- invert and add one-- thus computing the two's compliment of addition.
  operand_left_for_carry <= not operand_left when is_subtraction_operation = '1' else operand_left;  

  --
  -- Main adder for the AVR's alu.
  -- Performs all addition and subtraction operations for the AVR.
  --
  CARRY_RIPPLE_ADDER:
  for i in operand_left'range generate

    --Compute the given bit of the provided sum/difference.
    --This uses the classic addition algorithm.
    adder_out(i) <= operand_left(i) xor operand_right(i) xor adder_carry_in(i);

    -- Compute the carry in to the _next_ bit of the provided sum.
    adder_carry_in(i + 1) <= 
      (operand_left_for_carry(i) and operand_right(i)) or
      (operand_left_for_carry(i) and adder_carry_in(i)) or
      (operand_right(i) and adder_carry_in(i));

  end generate;

  -- Determine if signed overflow has occurred. This is functionally
  -- equivalent to using the carry out and the carry out minus one, but
  -- doesn't add to the critical path the way using the carry out does.
  v_flag_add_operation <= 
     --Overflow has occurred if we add a negative to a negative
     --and get a positive, or...
     (operand_left(7) and operand_right(7) and not adder_out(7)) or 
     --... if we add a positive to a positive and get a negative.
     (not operand_left(7) and not operand_right(7) and adder_out(7));

  v_flag_sub_operation <= 
     --Overflow has occurred if we subtract a positive from a negative
     --and get a positive, or...
     (operand_left(7) and not operand_right(7) and not adder_out(7)) or 
     -- ... if we subtract a negative from a positive and get a negative.
     (not operand_left(7) and operand_right(7) and adder_out(7));

  -- Determine whether signed overflow occurred during the given addition.
  v_flag_adder <= v_flag_sub_operation when is_subtraction_operation = '1' else v_flag_add_operation;

  --
  -- Logical Operations
  -- 

  -- COM: One's compliment.
  com_operation <= not rd_value;

  -- AND: Bitwise AND.
  and_operation <= rd_value and rr_value;
  
  -- OR:  Bitwise OR
  or_operation <= rd_value or rr_value;

  -- EOR: Exclusive OR
  eor_operation <= rd_value xor rr_value;

  -- LSR: Logic Shift Right
  lsr_operation         <= '0' & rd_value(7 downto 1);
  v_flag_shift_right    <= flags_out.n xor flags_out.c;

  -- ASR: Arithmetic Shift Right
  asr_operation <= rd_value(7) & rd_value(7 downto 1);

  -- ROR: Rotate Right (through carry)
  ror_operation <= flags_in.c & rd_value(7 downto 1);

  -- SWAP: Nibble swap.
  swap_operation <= rd_value(3 downto 0) & rd_value(7 downto 4);


  --
  -- ALU output "multiplexer".
  --
  result <= 
    adder_out(7 downto 0)     when use_adder_output  = '1' else
    com_operation             when operation.is_com  = '1' else
    and_operation             when operation.is_and  = '1' else
    or_operation              when operation.is_or   = '1' else
    or_operation              when operation.is_ori  = '1' else
    eor_operation             when operation.is_eor  = '1' else
    lsr_operation             when operation.is_lsr  = '1' else
    ror_operation             when operation.is_ror  = '1' else
    asr_operation             when operation.is_asr  = '1' else
    swap_operation            when operation.is_swap = '1' else
    x"00";


  --
  -- Logic that determines the value of the flags according to the operation being performed.
  --

  -- S (Signed test)
  flags_out.s <= flags_out.n xor flags_out.v;

  -- N (Negative flag)
  flags_out.n <= result(7);

  -- Z (Zero flag)
  result_is_zero <= not or_reduce(result);
  flags_out.z <= result_is_zero and flags_in.z when only_clears_z_flag = '1' else result_is_zero;

  -- H (Half Carry)
  flags_out.h <= adder_carry_in(4) when adder_sets_extended_flags = '1' else '0';

  -- C (Carry flag)
  flags_out.c <= 
     adder_carry_in(8)    when (use_adder_output and not operation_does_not_set_carry)    = '1' else
     rd_value(0)          when is_right_shift_operation                                   = '1' else
     '1'                  when operation.is_com                                           = '1' else
     '0';

  -- V (Two's compliment overflow)
  flags_out.v <= 
    v_flag_adder          when (second_cycle_of_adiw or second_cycle_of_sbiw or adder_sets_extended_flags)          = '1' else
    v_flag_adder          when (operation.is_inc or operation.is_dec)                     = '1' else
    v_flag_shift_right    when (operation.is_lsr or operation.is_ror or operation.is_asr) = '1' else
    '0';
                       
end rtl;
