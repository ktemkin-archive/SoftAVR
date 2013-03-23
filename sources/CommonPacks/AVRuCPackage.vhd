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
    c : std_logic;
    z : std_logic;
    n : std_logic;
    v : std_logic;
    s : std_logic;
    h : std_logic;
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
    end if;	

  end log2;	

end AVRuCPackage;	
