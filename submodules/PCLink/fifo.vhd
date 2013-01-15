----------------------------------------------------------------------------------
-- Simple Synchronous FIFO
--
-- Author: Kyle J. Temkin, <ktemkin@binghamton.edu>
-- Copyright (c) Kyle J. Temkin,  2013 Binghamton University
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.-
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity fifo is
  generic
  (
    --The width of the _count_ signal, which will store the amount of 
    --full elements in the FIFO. This signal is used to set the size
    --of the FIFO; a FIFO with count_bits equal to 5 will have a depth
    --of 32.
    count_bits      : integer := 5;

    --The width of each element in the FIFO.
    register_width  : integer := 8 
  );
  port(

    --System clock.
    clk      : in std_logic;

    --Active-high reset; clears the contents of the FIF0.
    reset    : in std_logic;

    --The data inputs and outputs of for the FIFO.
    data_in  : in   std_logic_vector(register_width - 1 downto 0);
    data_out : out  std_logic_vector(register_width - 1 downto 0);

    --The total amount of data elements currently stored in the FIFO.
    count    : out std_logic_vector(count_bits - 1 downto 0);

    --Enqueue and dequeue control signals; adds and removes an element
    --from the FIFO, respectively.
    enqueue  : in std_logic;
    dequeue  : in std_logic;

    --Status flags; note when the queue is empty (count == 0) and full
    --(count is at its maximum value) respectively.
    empty    : out std_logic;
    full     : out std_logic
  );
end fifo;

architecture Behavioral of fifo is

  --Quick reference to the total amount of elements in the FIFO.
  constant element_count : integer := 2 ** count_bits;

  --Create an array type, which will represent the contents of the FIFO.
  type queue is array (element_count - 1 downto 0) 
    of std_logic_vector(register_width - 1 downto 0);

  
  --The array of registers which will house our FIFO data.
  signal element : queue;

  --Register wihch stores our current plcae in the FIFO.
  signal count_reg : integer;
  signal write_target : integer;

begin

  --Determine the register which will be updated on the next
  --write operation.
  write_target <= 
    count_reg - 1 when dequeue = '1' and enqueue = '1' 
    else count_reg;

  --Empty and full flags: indicate when the FIFO is empty and full,
  --respectively.
  empty <= '1' when count_reg = 0 else '0';
  full  <= '1' when count_reg = (element_count - 1) else '0';

  --Count output; outputs the current value of the count signal.
  count <= std_logic_vector(to_unsigned(count_reg, count_bits));
  
  --Data output.
  data_out <= element(0);


  FIFO_BODY:
  process(clk)
  begin

    --Trigger on rising edge of the clock.
    if rising_edge(clk) then

      if reset = '1' then 
        element <= (others => (others => '0'));
      else

        --If the enqueue operation is selected...
        if enqueue = '1' then

          --Add the new element in the new free space.
          element(write_target) <= data_in;

          --If only the write flag is set, increment the element count.
          if dequeue = '0' then
            count_reg <= count_reg + 1;
          end if;

        end if;

        --If the read flag is set, remove an element from the queue.
        if dequeue = '1' then

          --Shift each element one towards zero.
          --Note that this isn't "expensive", as it would be in software-
          --we're simultaneously adjusting the value of each element, in parallel.
          for i in 0 to element_count - 1 loop
            element(i) <= element(i + 1);
          end loop;

          --If only the read flag is set, decrement the count.
          if enqueue = '0' then
            count_reg <= count_reg - 1;
          end if;

        end if;

      end if;
    end if;
  end process;


end Behavioral;

