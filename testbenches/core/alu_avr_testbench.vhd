library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.AVRucPackage.all;
use work.simulation_aides.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--USE ieee.numeric_std.ALL;
 
entity alu_avr_testbench is
end alu_avr_testbench;

 
architecture behavior of alu_avr_testbench is 
 
    -- Component Declaration for the Unit Under Test (UUT)
 
    component alu_avr
    port(
         operation     : in  decoded_operation;
         rr_value      : in  byte;
         rd_value      : in  byte;
         alu_c_flag_in : in  std_logic;
         alu_z_flag_in : in  std_logic;
         adiw_st       : in  std_logic;
         sbiw_st       : in  std_logic;
         alu_data_out  : buffer byte;
         flags_out     : buffer flag_set
        );
    end component;

   --Inputs
   signal op            : decoded_operation := (others => '0');
   signal rd, rr        : byte              := (others => '0');
   signal c, z          : std_logic         := '0';
   signal adiw_st       : std_logic         := '0';
   signal sbiw_st       : std_logic         := '0';

 	--Outputs
   signal alu_data_out  : byte              := (others => '0');
   signal flags         : flag_set          := (others => '0');


  --
  -- Helper subprogram which validates an expected result and each of the provided flag values.
  --
  procedure validate_result_and_flags(operation_name : string; result : byte; expected_sreg : std_logic_vector(5 downto 0)) is
    variable h, s, v, n, z, c : std_logic;
  begin
    (h, s, v, n, z, c) := expected_sreg;
    wait for 1ps;
    assert alu_data_out = result report operation_name & ": expected result " & to_string(result) & " (" & to_int_string(result) & "), was " & to_string(alu_data_out) & " (" & to_int_string(alu_data_out) & ")." severity error;
    assert std_match(flags.s, s) report operation_name & ": expected S to be " & std_logic'image(s) & ", was " & std_logic'image(flags.s) & "."; 
    assert std_match(flags.h, h) report operation_name & ": expected H to be " & std_logic'image(h) & ", was " & std_logic'image(flags.h) & "."; 
    assert std_match(flags.v, v) report operation_name & ": expected V to be " & std_logic'image(v) & ", was " & std_logic'image(flags.v) & "."; 
    assert std_match(flags.n, n) report operation_name & ": expected N to be " & std_logic'image(n) & ", was " & std_logic'image(flags.n) & "."; 
    assert std_match(flags.z, z) report operation_name & ": expected Z to be " & std_logic'image(z) & ", was " & std_logic'image(flags.z) & "."; 
    assert std_match(flags.c, c) report operation_name & ": expected C to be " & std_logic'image(c) & ", was " & std_logic'image(flags.c) & "."; 
  end;

begin

  -- Instantiate the Unit Under Test (UUT)
  uut: alu_avr port map (
    operation      => op,
    rd_value       => rd,
    rr_value       => rr,
    alu_c_flag_in  => c,
    alu_z_flag_in  => z,
    adiw_st        => adiw_st,
    sbiw_st        => sbiw_st,
    alu_data_out   => alu_data_out,
    flags_out      => flags
  );

  -- Stimulus process
  stim_proc: process
  begin		

    -- Flag order is the same as in the SREG, HSVNZC.
    c <= '1';

    -- ADD / ADDI
    op <= (is_add => '1', others => '0');
    rd <= x"80"; rr <= x"80"; validate_result_and_flags("ADD $80+$80", x"00", "011011"); 
    rd <= x"0F"; rr <= x"0F"; validate_result_and_flags("ADD $0F+$0F", x"1E", "100000"); 
    rd <= x"81"; rr <= x"FF"; validate_result_and_flags("ADD $7F+$FF", x"80", "110101"); 

    -- ADC
    op <= (is_adc => '1', others => '0');
    rd <= x"80"; rr <= x"7F"; validate_result_and_flags("ADC $80+$7F+1", x"00", "100011"); 
    rd <= x"F0"; rr <= x"80"; validate_result_and_flags("ADC $F0+$80+1", x"71", "011001"); 
    rd <= x"81"; rr <= x"00"; validate_result_and_flags("ADC $81+$00+1", x"82", "010100"); 
  
    -- SUB / SUBI tests.
    op <= (is_sub => '1', others => '0');
    rd <= x"80"; rr <= x"02"; validate_result_and_flags("SUB $80-$02", x"7E", "111000");
    rd <= x"7F"; rr <= x"7F"; validate_result_and_flags("SUB $7F-$7F", x"00", "000010");
    rd <= x"00"; rr <= x"FF"; validate_result_and_flags("SUB $00-$FF", x"01", "100001");
    rd <= x"80"; rr <= x"00"; validate_result_and_flags("SUB $80-$00", x"80", "010100");
    
    -- SBC / SBCI / CPC
    op <= (is_sbc => '1', others => '0');
    rd <= x"80"; rr <= x"02"; validate_result_and_flags("SBC $80-$02", x"7D", "111000");

    -- INC instruction
    op <= (is_inc => '1', others => '0');
    rd <= x"7F"; validate_result_and_flags("INC $7F", x"80", "-0110-");
    rd <= x"FF"; validate_result_and_flags("INC $FF", x"00", "-0001-");
    rd <= x"80"; validate_result_and_flags("INC $80", x"81", "-1010-");

    -- DEC instruction
    op <= (is_dec => '1', others => '0');
    rd <= x"80"; validate_result_and_flags("DEC $80", x"7F", "-1100-");
    rd <= x"00"; validate_result_and_flags("DEC $00", x"FF", "-1010-");
    rd <= x"01"; validate_result_and_flags("DEC $01", x"00", "-0001-");

    -- NEG instruction
    op <= (is_neg => '1', others => '0');
    rd <= x"00"; validate_result_and_flags("NEG $00", x"00", "000010"); 
    rd <= x"01"; validate_result_and_flags("NEG $01", x"FF", "110101"); 
    rd <= x"80"; validate_result_and_flags("NEG $80", x"80", "001101");

    --COM
    op <= (is_com => '1', others => '0');
    rd <= x"55"; validate_result_and_flags("COM $55", x"AA", "-10101"); 
    rd <= x"FF"; validate_result_and_flags("COM $FF", x"00", "-00011"); 

    --AND/ANDI
    op <= (is_and => '1', others => '0');
    rd <= x"AA"; rr <= x"55"; validate_result_and_flags("AND $AA, $55", x"00", "-0001-"); 
    rd <= x"F0"; rr <= x"FF"; validate_result_and_flags("AND $F0, $FF", x"F0", "-1010-"); 

    --OR
    op <= (is_or => '1', others => '0');
    rd <= x"AA"; rr <= x"55"; validate_result_and_flags("OR $AA, $55", x"FF", "-1010-"); 
    rd <= x"00"; rr <= x"00"; validate_result_and_flags("OR $00, $00", x"00", "-0001-"); 

    --EOR
    op <= (is_eor => '1', others => '0');
    rd <= x"AA"; rr <= x"55"; validate_result_and_flags("EOR $AA, $55", x"FF", "-1010-"); 
    rd <= x"AA"; rr <= x"AA"; validate_result_and_flags("EOR $AA, $AA", x"00", "-0001-"); 

    --LSR
    op <= (is_lsr => '1', others => '0');
    rd <= x"01"; validate_result_and_flags("LSR $01", x"00", "-11011"); 
    rd <= x"F0"; validate_result_and_flags("LSR $F0", x"78", "-00000"); 

    --ROR (carry set)
    op <= (is_ror => '1', others => '0');
    rd <= x"01"; validate_result_and_flags("ROR $01", x"80", "-10101"); 
    rd <= x"F0"; validate_result_and_flags("ROR $F0", x"F8", "-01100"); 

    --ROR (carry clear)
    c <= '0';
    rd <= x"00"; validate_result_and_flags("ROR $00", x"00", "-00010");
    c <= '1';

    --ASR
    op <= (is_asr => '1', others => '0');
    rd <= x"FF"; validate_result_and_flags("ASR $FF", x"FF", "-10101"); 
    rd <= x"01"; validate_result_and_flags("ASR $01", x"00", "-11011"); 
    rd <= x"00"; validate_result_and_flags("ASR $00", x"00", "-00010"); 

    --SWAP
    op <= (is_swap => '1', others => '0');
    rd <= x"AB"; validate_result_and_flags("SWAP $AB", x"BA", "------"); 

    --Don't repeat the simulation process.
    report "Test complete; performed " & integer'image(now / 1 ps) & " tests." severity note;
    wait; 


  end process;

END;
