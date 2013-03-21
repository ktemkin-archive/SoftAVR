----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    21:03:20 02/11/2013 
-- Design Name: 
-- Module Name:    sseg_driver - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
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

entity sseg_driver is

  port(
    --clock
    clk : in std_logic;

    --numeric inputs
    leftmost, left_center, right_center, rightmost : in std_logic_vector(3 downto 0);
    
    --decimal points
    leftmost_dp, left_center_dp, right_center_dp, rightmost_dp : in std_logic;

    --sseg outputs
    cathodes : out std_logic_vector(6 downto 0);
    dp : out std_logic;
    anodes : out std_logic_vector(3 downto 0)
  );

end sseg_driver;

architecture Behavioral of sseg_driver is

  --Display multiplexing signals:

  --Clock cycle count; used as a slow clock.
  signal cycle_count : unsigned(16 downto 0);

  --Stores the current display number.
  signal current_display : unsigned(1 downto 0) := (others => '0');

  --Signal that holds the current mux output.
  signal current_nibble : std_logic_vector(3 downto 0);

  --Signal that holds the inverted value of the decimal point.
  signal dp_not : std_logic;

begin

  --Divide the system clock to get a nice, slow mux clock.
  cycle_count <= cycle_count + 1 when rising_edge(clk);

  --Move through each of the four displays.
  current_display <= current_display + 1 when rising_edge(cycle_count(12));

  --Select the value to be displayed.
  with current_display select current_nibble <= 
    leftmost     when "00",
    left_center  when "01",
    right_center when "10",
    rightmost    when others;

  --Select the decimal point to be displayed.
  dp <= not dp_not;
  with current_display select dp_not <= 
    leftmost_dp     when "00",
    left_center_dp  when "01",
    right_center_dp when "10",
    rightmost_dp    when others;

  --Create the anode values.
  with current_display select anodes <= 
    "0111" when "00",
    "1011" when "01",
    "1101" when "10",
    "1110" when others;

  --Determine the main cathode values.
	with current_nibble select cathodes <=  
    "1111001" when "0001",   --1
    "0100100" when "0010",   --2
    "0110000" when "0011",   --3
    "0011001" when "0100",   --4
    "0010010" when "0101",   --5
    "0000010" when "0110",   --6
    "1111000" when "0111",   --7
    "0000000" when "1000",   --8
    "0010000" when "1001",   --9
    "0001000" when "1010",   --A
    "0000011" when "1011",   --b
    "1000110" when "1100",   --C
    "0100001" when "1101",   --d
    "0000110" when "1110",   --E
    "0001110" when "1111",   --F
    "1000000" when others;   --0


end Behavioral;

