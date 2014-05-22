--************************************************************************************************
-- 8Kx16(8 KB) PM RAM for AVR Core(Xilinx)
-- Version 0.1
-- Designed by Ruslan Lepetenok 
-- Modified by Jack Gassett for use with Papilio
-- Modified 11.06.2009
--************************************************************************************************

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library	unisim;
use unisim.vcomponents.all;

entity program_memory is 
  generic(
    memory_size  : positive := 8
  );
  port(

    --Control signals: our system clock and global enable.
    clk      : in std_ulogic;
    enable   : in std_ulogic;  

    --Operation flags.
    write    : in std_ulogic;
    erase    : in std_ulogic;

    --Operation flags

    --The address to be affected.
    --address : in std_logic_vector(integer(ceil(log2(real(memory_size *10)))) - 1 downto 0);
    address : in std_logic_vector(15 downto 0);

    --TODO: Replace me with a word type?
    data_in  : in std_logic_vector(15 downto 0);
    data_out : out std_logic_vector(15 downto 0)
  );
end program_memory;

architecture volatile of program_memory is

  --Define an array, which will represent the output of each block prior to their
  --entry into the output select mux.
  type block_ram_outputs is array(memory_size -1 downto 0) of std_logic_vector(data_out'range);
  signal output_of_block : block_ram_outputs;

  --Intermediary signals: these handle 
  signal write_enable_for     : std_logic_vector(memory_size - 1 downto 0);
  signal block_is_selected    : std_logic_vector(memory_size - 1 downto 0);


begin

  --
  -- Generate each of the block RAMs for the given device.
  --
  GENERATE_BLOCK_RAM:
  for i in 0 to memory_size - 1 generate

      --Enable the given register iff the address selected is within
      --its range.
      block_is_selected(i) <= 
        '1' when unsigned(address(address'high downto 10)) = i else
        '0';
      write_enable_for(i)  <= block_is_selected(i) and write;

      --Create the block RAM register itself...
      RAM: ramb16_s18
      generic map(write_mode => "WRITE_FIRST", srval => x"FF")
      port map(
        clk  => clk,
        do   => output_of_block(i),
        di   => data_in,
        addr => address(9 downto 0),
        dip  => "11",
        en   => enable,
        ssr  => erase,
        we   => write_enable_for(i)
      ); 
        
              
  end generate;

  --Select the output for the appropriate block.
  data_out <= output_of_block(to_integer(unsigned(address(address'high downto 10))));


end volatile;
