-- *****************************************************************************************
-- AVR constants and type declarations
-- Version 1.0A(Special version for the JTAG OCD)

-- Modified 3/23/2012

--  Authors:
--      -- Kyle J. Temkin, Binghamton University, <ktemkin@binghamton.edu>
--      -- Ruslan Leptenok, original core designer
--
-- *****************************************************************************************

library	IEEE;

use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

use WORK.SynthCtrlPack.all;

package AVRuCPackage is

  -- Old package
  type ext_mux_din_type is array(0 to CExtMuxInSize-1) of std_logic_vector(7 downto 0);
  subtype ext_mux_en_type  is std_logic_vector(0 to CExtMuxInSize-1);
  -- End of old package

  constant IOAdrWidth          : positive := 16;

  --
  -- Data types representing data in the AVR.
  --

  --Generic data types, used in general-purpose registers.
  subtype byte is std_logic_vector(7 downto 0);
  subtype word is std_logic_vector(15 downto 0);

  subtype encoded_instruction is std_logic_vector(15 downto 0);

  --
  -- Data types representing addresses in the AVR's various memory spaces.
  -- 

  -- I/O Addresses
  constant io_address_width    : positive := 16;
  subtype io_address is std_logic_vector(io_address_width - 1 downto 0);

  -- Program Memory Addresses
  constant program_address_width : positive := 16;
  subtype program_address is std_logic_vector(program_address_width - 1 downto 0);

  -- General-Purpose Register Addresses
  constant register_address_width : positive := 5;
  subtype register_address is std_logic_vector(register_address_width - 1 downto 0);

  --
  -- Data types used for system interconnections.
  --

  -- Set of system flags.
  type flag_set is record
    h : std_ulogic;
    s : std_ulogic;
    v : std_ulogic;
    n : std_ulogic;
    z : std_ulogic;
    c : std_ulogic;
  end record;

  function to_std_ulogic_vector(flags : flag_set) return std_ulogic_vector;
  function to_flag_set(flags : std_logic_vector(5 downto 0)) return flag_set;


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

  --
  -- Function for converting integer constants to I/O addresses.
  --
  function to_io_address (addr : integer) return io_address;

  -- I/O port addresses

  -- I/O register file
  constant RAMPZ_address  : io_address := to_io_address(16#3B#);
  constant SPL_address    : io_address := to_io_address(16#3D#);
  constant SPH_address    : io_address := to_io_address(16#3E#);
  constant SREG_address   : io_address := to_io_address(16#3F#);
  -- End of I/O register file

  -- UART
  constant UDR_address    : io_address := to_io_address(16#0C#);
  constant UBRR_address   : io_address := to_io_address(16#09#);
  constant USR_address    : io_address := to_io_address(16#0B#);
  constant UCR_address    : io_address := to_io_address(16#0A#);
  -- End of UART	

  -- Timer/Counter
  constant TCCR0_address  : io_address := to_io_address(16#33#);
  constant TCCR1A_address : io_address := to_io_address(16#2F#);
  constant TCCR1B_address : io_address := to_io_address(16#2E#);
  constant TCCR2_address  : io_address := to_io_address(16#25#);
  constant ASSR_address   : io_address := to_io_address(16#30#);
  constant TIMSK_address  : io_address := to_io_address(16#37#);
  constant TIFR_address   : io_address := to_io_address(16#36#);
  constant TCNT0_address  : io_address := to_io_address(16#32#);
  constant TCNT2_address  : io_address := to_io_address(16#24#);
  constant OCR0_address   : io_address := to_io_address(16#31#);
  constant OCR2_address   : io_address := to_io_address(16#23#);
  constant TCNT1H_address : io_address := to_io_address(16#2D#);
  constant TCNT1L_address : io_address := to_io_address(16#2C#);
  constant OCR1AH_address : io_address := to_io_address(16#2B#);
  constant OCR1AL_address : io_address := to_io_address(16#2A#);
  constant OCR1BH_address : io_address := to_io_address(16#29#);
  constant OCR1BL_address : io_address := to_io_address(16#28#);
  constant ICR1AH_address : io_address := to_io_address(16#27#);
  constant ICR1AL_address : io_address := to_io_address(16#26#);
  -- End of Timer/Counter	

  -- Service module
  constant MCUCR_address  : io_address := to_io_address(16#35#);
  constant EIMSK_address  : io_address := to_io_address(16#39#);
  constant EIFR_address   : io_address := to_io_address(16#38#);
  constant EICR_address   : io_address := to_io_address(16#3A#);
  constant MCUSR_address  : io_address := to_io_address(16#34#);
  constant XDIV_address   : io_address := to_io_address(16#3C#);
  -- End of service module

  -- EEPROM 
  constant EEARH_address  : io_address := to_io_address(16#1F#);
  constant EEARL_address  : io_address := to_io_address(16#1E#);
  constant EEDR_address   : io_address := to_io_address(16#1D#);
  constant EECR_address   : io_address := to_io_address(16#1C#);
  -- End of EEPROM 

  -- SPI
  constant SPDR_address   : io_address := to_io_address(16#0F#);
  constant SPSR_address   : io_address := to_io_address(16#0E#);
  constant SPCR_address   : io_address := to_io_address(16#0D#);
  -- End of SPI

  -- PORTA addresses 
  constant PORTA_address  : io_address := to_io_address(16#1B#);
  constant DDRA_address   : io_address := to_io_address(16#1A#);
  constant PINA_address   : io_address := to_io_address(16#19#);

  -- PORTB addresses 
  constant PORTB_address  : io_address := to_io_address(16#18#);
  constant DDRB_address   : io_address := to_io_address(16#17#);
  constant PINB_address   : io_address := to_io_address(16#16#);

  -- PORTC addresses 
  constant PORTC_address  : io_address := to_io_address(16#15#);
  constant DDRC_address   : io_address := to_io_address(16#14#);
  constant PINC_address   : io_address := to_io_address(16#13#);

  -- PORTD addresses 
  constant PORTD_address  : io_address := to_io_address(16#12#);
  constant DDRD_address   : io_address := to_io_address(16#11#);
  constant PIND_address   : io_address := to_io_address(16#10#);

  -- PORTE addresses 
  constant PORTE_address  : io_address := to_io_address(16#03#);
  constant DDRE_address   : io_address := to_io_address(16#02#);
  constant PINE_address   : io_address := to_io_address(16#01#);

  -- PORTF addresses
  constant PORTF_address  : io_address := to_io_address(16#07#);
  constant DDRF_address   : io_address := to_io_address(16#08#);
  constant PINF_address   : io_address := to_io_address(16#00#);

  -- Analog to digital converter
  constant ADCL_address  : io_address := to_io_address(16#04#);
  constant ADCH_address  : io_address := to_io_address(16#05#);
  constant ADCSR_address : io_address := to_io_address(16#06#);
  constant ADMUX_address : io_address := to_io_address(16#07#);

  -- Analog comparator
  constant ACSR_address  : io_address := to_io_address(16#08#);

  -- Watchdog
  constant WDTCR_address : io_address := to_io_address(16#21#);

  -- JTAG OCDR (ATmega128)
  constant OCDR_address   : io_address := to_io_address(16#22#);

  -- JTAG OCDR (ATmega16)
  --constant OCDR_address   : io_address := to_io_address(16#31#);

  -- ***************************************************************************************

  -- Function declaration
  function log2(Number : positive) return natural;

end AVRuCPackage;

package	body AVRuCPackage is

  --
  -- Function for converting integer constants to I/O addresses.
  --
  function to_io_address(addr : integer) return io_address is
    begin
    return std_logic_vector(to_unsigned(addr, io_address_width));
  end to_io_address;


  --
  -- Converts a flag_set to a standard logic vector, in the same
  -- format used for pushing/popping the SREG.
  -- 
  function to_std_ulogic_vector(flags : flag_set) return std_ulogic_vector is
  begin
    return std_ulogic_vector'(flags.h, flags.s, flags.v, flags.n, flags.z, flags.c);
  end function;

  --
  -- Converts a std_ulogic_vector to a flag_set.
  -- Accepts a vector in (HSVNZC) order; the same order as in the AVR's status register.
  -- If extra bits are provided (e.g. the I/T bits), they will be ignored.
  --
  function to_flag_set(flags : std_logic_vector) return flag_set is
  begin
    return flag_set'(h => flags(5), s => flags(4), v => flags(3), n => flags(2), z => flags(1), c => flags(0));
  end function;

  --
  -- Log-base-2 convenience function.
  -- Intended for use in computing bus sizes. 
  --
  function log2(Number : positive) return natural is
    variable power_of_two : positive;
  begin

    power_of_two := 1;

    --If we were passed the special-case value of 1, return 0.
    if number = 1 then 
      return 0;

    --Otherwise, loop until we find the number which multiplies to 2.
    else 

      --Loop through all possible bases...
      for i in 1 to integer'high loop

        --Find the next power of two.
        power_of_two := 2 * power_of_two; 

        --If we've found a power-of-two that will fit this number, return its base.
        if power_of_two >= number then 
          return i;
        end if;

      end loop;

    --This _should_ never be triggered, but silences an ISE warning.
    return 0;

    end if;	

  end log2;	

end AVRuCPackage;	
