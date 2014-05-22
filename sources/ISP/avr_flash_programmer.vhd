--  
-- AVR-910 Compliant In-System Programmer for Soft-IP AVR-compatible microcontrollers.
--
--
-- The MIT License (MIT)
-- 
-- Copyright (c) 2014 Kyle J. Temkin
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.
-- 
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--TODO: move this to the relevant library!
library avr_programmer;
use avr_programmer.constructs.all;


entity avr_flash_programmer is
  generic(
    
    --The clock frequency at which the device is being run.
    --Handled by 
    clock_frequency    : positive          := 32000000;

    --The bit rate at which the programmer will communnicate.
    baud_rate          : positive          := 19200;

    --The signature of the microcontroller to report.
    signature          : std_ulogic_vector := x"1E_97_01"; --ATmega103

    --AVR-910 device code for the device being targeted.
    device_id          : std_ulogic_vector := x"41"; --ATmega103

    --The "programmer type", as reported upon request.
    --programmer_type    : programmer_id     := (x"41", x"56", x"52", x"50", x"52", x"4F", x"47"); --"AVRPROG"
    programmer_type    : programmer_id     := (x"50", x"41", x"50", x"49", x"4C", x"49", x"4F"); --"PAPILIO"

    --Hardware version.
    --This should be overwritten to include your own hardware.
    hardware_version   : std_ulogic_vector(15 downto 0) := x"3031";

    --Bootloader version.
    --Unless you have a good reason to; this should likely not be overridden,
    --as this is the hardware ID of the bootloader.
    bootloader_version : std_ulogic_vector(15 downto 0) := x"3032"

  );
  port(
    
    --System-wide inputs.
    clk     : in std_ulogic;
    reset   : in std_ulogic := '0';
    
    --UART control interface.
    rx      : in  std_ulogic;
    tx      : out std_ulogic;

    --Flash memory control I/O.
    erase   : out std_ulogic;

    --TODO: Fix width.
    address         : buffer std_ulogic_vector(15 downto 0) := (others => '0');
    data_from_flash : in  std_ulogic_vector(15 downto 0);
    data_to_flash   : out std_ulogic_vector(15 downto 0);
    write_to_flash  : out std_ulogic;

    --CPU-related signals.
    in_programming_mode : buffer std_ulogic := '0';

    --The current value of the low-fuse register.
    low_fuse_in         : std_ulogic_vector(7 downto 0) := x"AA";
    high_fuse_in        : std_ulogic_vector(7 downto 0) := x"BB";
    extended_fuse_in    : std_ulogic_vector(7 downto 0) := x"CC"
      
  );
end avr_flash_programmer;


architecture behavioral of avr_flash_programmer is
  
  --Signals which convey transmitted and received data to/from the UART.
  signal data_to_transmit, received_data : std_ulogic_vector(7 downto 0);

  --UART control signals.
  signal request_transmit, transmit_request_acknowledged : std_ulogic; 
  signal data_waiting, data_was_waiting_last_cycle       : std_ulogic; 
  signal new_data_received, data_reciept_handled         : std_ulogic;

  -- FSM state type for the main controller.
  type programmer_state_type is (
    WAIT_FOR_COMMAND, 
    SEND_ID, 
    SEND_VERSION_MSB, 
    SEND_VERSION_LSB,
    SEND_PROGRAM_BYTE_LOW,
    SEND_SIGNATURE_MIDDLE,
    SEND_SIGNATURE_END,
    RECEIVE_ADDR_HIGH, 
    RECEIVE_ADDR_LOW,
    RECEIVE_DEVICE_ID,
    RECEIVE_UNIVERSAL_COMMAND,
    RECEIVE_PROGRAM_BYTE_LOW,
    RECEIVE_PROGRAM_BYTE_HIGH,
    PROCESS_UNIVERSAL_COMMAND,
    ACKNOWLEDGE_COMMAND, 
    TERMINATE_STRING_AND_RESTART,
    INCREMENT_ADDRESS_AND_END,
    INCREMENT_ADDRESS_AND_ACKNOWLEDGE,
    WAIT_FOR_TRANSMIT
  );
  signal state                : programmer_state_type := WAIT_FOR_COMMAND;


  --Special "target" register which stores the state to advance to after a transmit.
  signal post_transmit_state  : programmer_state_type := WAIT_FOR_COMMAND;

  --The version to be sent during the "send version" states.
  signal version_to_transmit  : std_ulogic_vector(hardware_version'range) := (others => '0');

  --
  -- "ID transmission" sub-fsm signals.
  --

  signal programmer_id_character    : byte;
  signal position_in_programmer_id  : integer range 0 to 6 := 0;
  signal transmitting_programmer_id, transmitting_last_byte_of_programmer_id : std_ulogic;


  --
  --Signals for the "universal command" sub-FSM:
  --

  --Register which stores the full universal command that's been sent.
  signal universal_command                 : std_ulogic_vector(31 downto 0);
  signal universal_command_bytes_remaining : integer range 0 to 4 := 0; 
  signal receiving_universal_command       : boolean;


  --Universal (SPI) command constants.
  constant READ_LOW_FUSE_COMMAND      : std_ulogic_vector(31 downto 0) := "010100000000000000000000--------";
  constant READ_HIGH_FUSE_COMMAND     : std_ulogic_vector(31 downto 0) := "010110000000100000000000--------";
  constant READ_EXTENDED_FUSE_COMMAND : std_ulogic_vector(31 downto 0) := "010100000000100000000000--------";

  --Temporary, fixed constants 
  --constant low_fuse_in         : std_ulogic_vector(7 downto 0) := x"AA";
  --constant high_fuse_in        : std_ulogic_vector(7 downto 0) := x"BB";
  --constant extended_fuse_in    : std_ulogic_vector(7 downto 0) := x"CC";

begin

  --
  -- Instantiate the main UART, which handles communications
  -- with the programming PC.
  -- 
  PC_CONNECTION: entity work.uart
  generic map (
    baud_rate           => baud_rate,
    clock_frequency     => clock_frequency
  )
  port map(  
    -- general
    clock               => clk,
    reset               => reset,
    data_stream_in      => data_to_transmit,
    data_stream_in_stb  => request_transmit,
    data_stream_in_ack  => transmit_request_acknowledged,

    data_stream_out     => received_data,
    data_stream_out_stb => data_waiting,
    data_stream_out_ack => data_reciept_handled,
    tx                  => tx,
    rx                  => rx
  );

  --Create a simple signal which goes high for exactly one cycle when new data is ready.
  data_was_waiting_last_cycle <= data_waiting when rising_edge(clk);
  new_data_received           <= data_waiting and not data_was_waiting_last_cycle; 

  --Create the main controller logic for the main FSM.
  process(clk)
  begin

    if rising_edge(clk) then

      --Set control signals to '0' unless explicitly asserted.
      data_reciept_handled       <= '0';    
      request_transmit           <= '0';
      transmitting_programmer_id <= '0';

      erase                      <= '0';
      write_to_flash             <= '0';

      case state is

        --
        -- Idle state:
        -- Wait for information to be retrieved via serial.
        --
        when WAIT_FOR_COMMAND =>

          --Assume that we want the default behavior: after a transmit,
          --we return to this "wait for command" state.
          post_transmit_state <= WAIT_FOR_COMMAND;

          --If we've recieved new data...
          if new_data_received = '1' then

            --... indicate that the data was handled...
            data_reciept_handled <= '1';
         
            --... and move to the appropriate handler routine. 
            case received_data is

              --
              --"P": enter programmming mode.
              --
              when x"50" =>
                in_programming_mode <= '1';
                state <= ACKNOWLEDGE_COMMAND;


              --
              --"L": leave programming mode
              --
              when x"4C" =>
                in_programming_mode <= '0';
                state <= ACKNOWLEDGE_COMMAND;


              --
              --"a": A query which determines whether the device supports auto-incrementation
              --     of addresses.
              --
              when x"61" =>
               
                --Respond "Y", as we do support auto-increment.
                data_to_transmit <= x"59";
                state <= WAIT_FOR_TRANSMIT;


              --
              -- "b": Check for buffer support.
              -- Currently, we do not support buffering. If we did, we would respond "Y", and then two bytes indicating buffer size.
              --
              when x"62" =>

                --Respond "N", as we temporarily do not support buffering.
                data_to_transmit <= x"4E";
                state <= WAIT_FOR_TRANSMIT;

              --
              -- "p": Return the programmer type. We're a serial programmer, so respond with "S" for serial.
              --
              when x"70" =>
                data_to_transmit <= x"53";
                state <= WAIT_FOR_TRANSMIT;

              --
              --"A": Set the current "working address": the current address in (typically flash) memory.
              --
              when x"41" =>
                state <= RECEIVE_ADDR_HIGH;

              --
              --"S": Send the programmer's ID string.
              --
              when x"53" => 
                state <= SEND_ID;

              --
              -- 'v'
              -- Host has requested the hardware version.
              --
              when x"76" =>

                --Load the hardware version into the "version to transmit"
                --register.
                version_to_transmit <= hardware_version;

                --... and move to the transmit version state.
                state <= SEND_VERSION_MSB;

              --              
              --'V'
              -- Host has requested the bootloader version.
              --
              when x"56" =>

                --Load the bootloader version into the "version to transmit"
                --register.
                version_to_transmit <= bootloader_version;
                state <= SEND_VERSION_MSB;

              --
              -- 't'
              -- Host has requested that we send all supportde device IDs.
              --
              when x"74" =>

                --Send the single "supported" device ID...
                data_to_transmit    <= device_id;

                --... followed by a null terminator. After we're finished, await the next command.
                post_transmit_state <= TERMINATE_STRING_AND_RESTART;
                state <= WAIT_FOR_TRANSMIT;

              --
              -- 'T': Specifies the device ID to be programmed.
              --
              when x"54" =>
                state <= RECEIVE_DEVICE_ID;


              --
              -- 'e': Perform a full chip erase.
              --
              when x"65" =>

                --Set the erase flag high for a single cycle.
                erase <= '1';
                state <= ACKNOWLEDGE_COMMAND;


              --
              -- 'c': Store the lower part of a program memory word, for wriitng.
              --
              when x"63" =>
                state <= RECEIVE_PROGRAM_BYTE_LOW;

              --
              -- 'c': Store the lower part of a program memory word, for wriitng.
              --
              when x"43" =>
                state <= RECEIVE_PROGRAM_BYTE_HIGH;


              --
              -- 'R': Read the current program memory address.
              when x"52"=>

                --Set up transmission of the MSB of the current program memory word.
                data_to_transmit <= data_from_flash(15 downto 8);
                state <= WAIT_FOR_TRANSMIT;

                --... followed by transmission of the LSB.
                post_transmit_state <= SEND_PROGRAM_BYTE_LOW;
                

              --
              -- 'm': Perform a page write.
              --
              when x"6D" =>

                --Since we're not actually writing to flash (at this stage), acknowledge all page
                --writes, but don't do anything.
                state <= ACKNOWLEDGE_COMMAND;

              --
              -- 's': -- Host requests the microcontroller's signature.
              --
              when x"73" =>

                --Send the first byte of the device's signature...
                data_to_transmit <= signature(7 downto 0);
                state <= WAIT_FOR_TRANSMIT;

                ---... and continue to send the second byte of the device's signature.
                post_transmit_state <= SEND_SIGNATURE_MIDDLE;


              --
              -- ".": Host is sending a new four-byte "universal" (SPI programmer) command.
              --
              when x"2E" =>

                --Since we're using a four-byte command, set the "bytes to recieve" register to 4...
                universal_command_bytes_remaining <= 4;

                --... and move into the "receive" state.
                state <= RECEIVE_UNIVERSAL_COMMAND;


              --In all other cases, respond with a question mark.
              when others =>
                data_to_transmit <= x"3F"; -- "?"
                state <= WAIT_FOR_TRANSMIT;

            end case;

          end if;



        --
        -- Send programmer ID.
        -- Delegate control to the ID child FSM.
        --
        when SEND_ID =>

          --Send the active bit of the programmer id.
          --When 
          data_to_transmit <= programmer_type(position_in_programmer_id);
          request_transmit <= '1';

          --Notify the child FSM.
          transmitting_programmer_id <= '1';

          --Once we're finished enquing the last byte of the programmer ID,
          --move to the WAIT_FOR_TRANSMIT state, and start over.
          if transmitting_last_byte_of_programmer_id = '1' then
            state <= WAIT_FOR_TRANSMIT;
          end if;


        --
        -- Send the most-significant-byte of the hardwaver version.
        --
        when SEND_VERSION_MSB =>

          --Transmit the MSB of the hardware version...
          data_to_transmit <= version_to_transmit(15 downto 8);
          --request_transmit <= '1';

          --... and then move on to the LSB of the version.
          --after the transmit completes.
          post_transmit_state <= SEND_VERSION_LSB;
          state <= WAIT_FOR_TRANSMIT;


        --
        -- Send the least-significant-byte of the hardwaver version.
        --
        when SEND_VERSION_LSB =>

          --Transmit the LSB of the hardware version...
          data_to_transmit <= version_to_transmit(7 downto 0);
          --request_transmit <= '1';

          --... and then wait for the next command.
          post_transmit_state <= WAIT_FOR_COMMAND;
          state <= WAIT_FOR_TRANSMIT;


        --
        -- Send the middle bye of the device's signature.
        --
        when SEND_SIGNATURE_MIDDLE =>

          --Transmit the middle byte of the signature...
          data_to_transmit <= signature(15 downto 8);
          --request_transmit <= '1';

          --... and continue to the last byte.
          post_transmit_state <= SEND_SIGNATURE_END;
          state <= WAIT_FOR_TRANSMIT;


        --
        -- Send the final bye of the device's signature.
        --
        when SEND_SIGNATURE_END =>

          --Transmit the middle byte of the signature...
          data_to_transmit <= signature(23 downto 16);
          --request_transmit <= '1';

          --... and wait for the next command.
          post_transmit_state <= WAIT_FOR_COMMAND;
          state <= WAIT_FOR_TRANSMIT;


        --
        -- Recieve Address High.
        -- Recieve and set the target address's high bit.
        --
        when RECEIVE_ADDR_HIGH =>

          -- Once we've received a byte of data,
          -- use it to set the high byte of the address, then
          -- move to the low byte.
          if new_data_received = '1' then
            address(15 downto 8) <= received_data;
            data_reciept_handled <= '1';
            state <= RECEIVE_ADDR_LOW;
          end if;
             

        --
        -- Recieve Address High.
        -- Recieve and set the target address's high bit.
        --
        when RECEIVE_ADDR_LOW =>

          -- Once we've received a byte of data,
          -- use it to set the high byte of the address, then
          -- move to the low byte.
          if new_data_received = '1' then
            address(7 downto 0) <= received_data;
            data_reciept_handled <= '1';
            state <= ACKNOWLEDGE_COMMAND;
          end if;


        --
        -- Receive Device ID.
        -- Receive the target device ID from the host, and validate it.
        --
        when RECEIVE_DEVICE_ID =>


          --Wait here until we've recieved a byte of data from the host.
          --Once we've received a byte of data...
          if new_data_received = '1' then

            --Mark the recipet as handled, so the UART can continue.
            data_reciept_handled <= '1';

            --If we've recieved the suppored device id,
            --acknowledge the command.
            if received_data = device_id then
              state <= ACKNOWLEDGE_COMMAND;

            --Otherwise, return to waiting for a command.
            --The programmer should time out.
            else
              state <= WAIT_FOR_COMMAND;
            end if;

          end if;



        --
        -- Acknowledge valid command.
        -- A valid command has been completed; send back an acknowledgement.
        --
        when ACKNOWLEDGE_COMMAND =>

          --Ensure that we wind up back in the "wait for command" state
          --after this given send.
          post_transmit_state <= WAIT_FOR_COMMAND;

          --Set up the UART to send back a carriage return "\r".
          data_to_transmit <= x"0D";
          state <= WAIT_FOR_TRANSMIT;


        --
        -- Terminates a string or list of bytes by sending the null character.
        -- A valid command has been completed; send back an acknowledgement.
        --
        when TERMINATE_STRING_AND_RESTART =>

          --Ensure that we wind up back in the "wait for command" state
          --after this given send.
          post_transmit_state <= WAIT_FOR_COMMAND;

          --Set up the UART to send back a carriage return "\r".
          data_to_transmit <= x"00";
          state <= WAIT_FOR_TRANSMIT;


        --
        -- Handle receipt of the lower byte of a program memory word.
        --
        when RECEIVE_PROGRAM_BYTE_LOW =>

          --Once we recieve a new byte of data.
          if new_data_received = '1' then

            data_reciept_handled <= '1';

            --Place the given byte on the appropriate bits of the
            --input to the program memory.
            data_to_flash(7 downto 0) <= received_data;
             
            --... and acknowledge the command.
            state <= ACKNOWLEDGE_COMMAND; 

          end if;


        --
        -- Handle receipt of the lower byte of a program memory word.
        --
        when RECEIVE_PROGRAM_BYTE_HIGH =>

          --Once we recieve a new byte of data.
          if new_data_received = '1' then

            data_reciept_handled <= '1';

            --Place the given byte on the appropriate bits of the
            --input to the program memory...
            data_to_flash(15 downto 8) <= received_data;

            --.. set up a flash write...
            write_to_flash <= '1';

            state <= INCREMENT_ADDRESS_AND_ACKNOWLEDGE;

          end if;



        --
        -- Send the LSB of the current program memory byte.
        --
        when SEND_PROGRAM_BYTE_LOW =>

          --Schedule transmission of the LSB...
          data_to_transmit <= data_from_flash(7 downto 0);
          state <= WAIT_FOR_TRANSMIT;

          --... followed by a wait for the next command.
          post_transmit_state <= INCREMENT_ADDRESS_AND_END;



        --
        -- Automatically increment the target address, nd 
        --
        when INCREMENT_ADDRESS_AND_END =>

          --Automatically increment the address.
          address <= std_ulogic_vector(unsigned(address) + 1);

          --... and wait for the next command.
          state <= WAIT_FOR_COMMAND;


        --
        -- Automatically increment the target address, nd 
        --
        when INCREMENT_ADDRESS_AND_ACKNOWLEDGE =>

          --Automatically increment the address.
          address <= std_ulogic_vector(unsigned(address) + 1);

          --... and wait for the next command.
          state <= ACKNOWLEDGE_COMMAND;


        --
        -- Recieve a universal (SPI) command from the host.
        --
        when RECEIVE_UNIVERSAL_COMMAND =>

          --If we've just received a new byte of the command...
          if new_data_received = '1' then

            data_reciept_handled <= '1';

            --Shift in the new byte...
            universal_command <= universal_command(23 downto 0) & received_data;

            --... decrease the total number of bytes left to recieve...
            universal_command_bytes_remaining <= universal_command_bytes_remaining - 1;

            --... and, if this was the last remaining byte, move to process the universal command.
            if universal_command_bytes_remaining = 1 then
              state <= PROCESS_UNIVERSAL_COMMAND;
            end if;

          end if;


        --
        -- Process (or delegate) a recieved universal command.
        --
        when PROCESS_UNIVERSAL_COMMAND =>

          --Transmit the requested data, and then acknowledge the command.
          state <= WAIT_FOR_TRANSMIT;
          post_transmit_state <= ACKNOWLEDGE_COMMAND;


          --If the universal command was a "read low fuse" command,
          --read the low fuse.
          if std_match(universal_command, READ_LOW_FUSE_COMMAND) then 
            data_to_transmit <= low_fuse_in;

          elsif std_match(universal_command, READ_HIGH_FUSE_COMMAND) then
            data_to_transmit <= high_fuse_in;

          elsif std_match(universal_command, READ_EXTENDED_FUSE_COMMAND) then
            data_to_transmit <= extended_fuse_in;

          
          --If we don't recognize the command, move directly to the WAIT_FOR_COMMAND state.
          else
            state <= WAIT_FOR_COMMAND;
          end if;



        
        --
        -- Wait for transmission.
        -- A transmission has been started; wait until it's complete, and
        -- and then start from the beginning.
        --
        when WAIT_FOR_TRANSMIT =>

          --Continue to request transmission until the UART
          --acknowledges that our transmission is complete.
          request_transmit <= '1';

          --Once we've recieved an acknowledgement,
          --move to wait for our next command.
          if transmit_request_acknowledged = '1' then
            state <= post_transmit_state;
          end if;

      end case;
    end if;
  end process;



  --
  -- ID transmission sub-FSM.
  -- 
  process(clk)
  begin
    if rising_edge(clk) then

      --If we're not supposed to be transmitting, 
      if reset = '1' or transmitting_programmer_id = '0' then
        position_in_programmer_id <= 0;

      --If we've just finished transmitting a bit of the programmer id...
      elsif transmit_request_acknowledged = '1' and transmitting_programmer_id = '1' then
        position_in_programmer_id <= position_in_programmer_id + 1;
      end if;

    end if;
  end process;

  --Determine when we're transmitting the last byte of the programmer ID.
  transmitting_last_byte_of_programmer_id <= '1' when (position_in_programmer_id = programmer_id'high) else '0';





end behavioral;

