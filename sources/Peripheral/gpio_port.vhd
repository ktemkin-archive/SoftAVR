--**********************************************************************************************
--  General Purpose I/O Port (Parallel Port) Peripheral for the AVR8 Soft-Core
--  Version 0.8
--  Modified 12.27.2012
--
--  Authors:
--      -- Kyle J. Temkin, Binghamton University, <ktemkin@binghamton.edu>
--      -- Ruslan Leptenok, original core designer
--
--**********************************************************************************************

library IEEE;
use IEEE.std_logic_1164.all;

use WORK.AVRuCPackage.all;
use WORK.SynthCtrlPack.all; -- Synthesis control
use WORK.SynchronizerCompPack.all; -- Component declarations for the synchronizers 

entity gpio_port is 
    generic(
        port_number   : natural;
        
        --Data direction "output mask"; allows individual bits of the port to be made input-only.
        --Any bit set to '0' will be input-only; this allows us to safely attach ports to input
        --devices, like buttons or switches.
        output_enable : std_logic_vector(7 downto 0) := x"FF" 
    ); 
	port(
        -- System-wide connections: clock and reset.
        clk	                : in std_logic;
        reset_not           : in std_logic;

        --
        -- Port control signals: read/write enable.
        --
        read_enable         : in std_logic;
        write_enable        : in std_logic;

        --
        -- Peripheral connections to the system data bus.
        --

        --Bus "address": specifies which component is currently in control of the bus.
        bus_addr            : in std_logic_vector(15 downto 0);

        --Bus input- the signal currently being observed on the system data bus.
        bus_in              : in std_logic_vector(7 downto 0);

        --Bus "drive" control output.
        --When true, this component is currently trying to drive the system data bus.
        driving_bus         : out std_logic; 

        --Bus output- the signal currently being driven on the system data bus.
        --When "driving_bus" is false, this value is meaningless.
        bus_out             : out std_logic_vector(7 downto 0);

        --
        -- GPIO connections to the outside world.
        --
        output_port         : out std_logic_vector(7 downto 0);
        data_direction      : out std_logic_vector(7 downto 0);
        input_pins          : in  std_logic_vector(7 downto 0)
    );
end gpio_port;

architecture rtl of gpio_port is

    --
    -- Internal registers for the output port register (PORT) and data direction register (DDR).
    --
    -- These store the most recent output value for the port, and the current
    -- port "polarity", respectively.
    --
    signal output_port_register    : std_logic_vector(output_port'range);
    signal data_direction_register : std_logic_vector(data_direction'range);

    --
    -- Internal, synchronized version of the input pins (PIN) for this port.
    --
    signal input_pins_synchronized_first_stage, input_pins_synchronized : std_logic_vector(input_pins'range);

    --
    -- Combinational signals which indicate whether the data bus address is equal to the
    -- the address for each of the three component registers.
    --
    signal output_port_selected, data_direction_selected, input_pins_selected : std_logic;

begin

    --Determine if any of the internal registers should be active on the data bus by
    --checking the address which currently "owns" the bus, which is provided as an input.
    --If the address is equal to that of any of our internal registers, than that register should 
    --have "control" of the bus.
    output_port_selected     <= '1' when bus_addr = port_addresses(port_number).port_address else '0';
    data_direction_selected  <= '1' when bus_addr = port_addresses(port_number).ddr_address else '0';	
    input_pins_selected      <= '1' when bus_addr = port_addresses(port_number).pin_address else '0';	
        
    --Determine if the GPIO port is currently trying to "talk" on the bus (i.e. drive the bus's value).
    --The GPIO port "talks" whenever one of its registers has control of the bus, and the port's read enable is '1'.
    driving_bus <= (output_port_selected or data_direction_selected or input_pins_selected) and read_enable;

    --
    -- Data direction register (DDR register).
    --
    process(clk, reset_not)
    begin
        --If the asynchronous reset is asserted, clear the data direction register;
        --this resets all of the port's pins to input mode.
        if reset_not = '0' then                  
            data_direction_register <= (others => '0'); 

        --If the CPU is trying to write to the data direction register,
        --clock in the new value for the data direction register from the data bus.
        elsif rising_edge(clk) and data_direction_selected = '1' and write_enable = '1' then

            --Before the new data direction is stored, force any "read-only" bits to zero.
            data_direction_register <= bus_in and output_enable;

        end if;

    end process;		

    --
    -- Output-mode data register (PORT register).
    -- 
    process(clk, reset_not)
    begin

        --If our reset signal is asserted, asynchronously reset the output port
        --to all zeroes.
        if reset_not = '0' then  
            output_port_register <= (others => '0'); 

        --Otherwise, if the CPU is trying to write to the output port,
        --clock in the new value from the data bus.
        elsif rising_edge(clk) and output_port_selected = '1' and write_enable = '1' then 
            output_port_register <= bus_in;
        end if;

    end process;		

    --Input pin synchronizer:
    --
    --Note that we synchronize on a falling and a rising edge- this compromise allows the microprocessor
    --to respond to new inputs more quickly, at the cost of an increased risk of metastability.
    --
    input_pins_synchronized_first_stage  <= (others => '0') when reset_not = '0' else input_pins when falling_edge(clk);
    input_pins_synchronized <= (others => '0') when reset_not = '0' else input_pins_synchronized_first_stage when rising_edge(clk);

    --
    -- Data Bus Output Multiplexer
    -- Determines the value of the output bus. 
    --
    bus_out <= output_port_register    when output_port_selected = '1'    else
               data_direction_register when data_direction_selected = '1' else
               input_pins_synchronized when input_pins_selected = '1'      else
               (others => '0');


    --Output the current value of the output port and data direction in parallel.
    output_port <= output_port_register;     
    data_direction <= data_direction_register;     

end RTL;
