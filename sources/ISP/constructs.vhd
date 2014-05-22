--
--	Package File Template
--
--	Purpose: This package defines supplemental types, subtypes, 
--		 constants, and functions 
--
--   To use any of the example code shown below, uncomment the lines and modify as necessary
--

library IEEE;
use IEEE.STD_LOGIC_1164.all;

package constructs is

  subtype byte is std_ulogic_vector(7 downto 0);
  type programmer_id is array(0 to 6) of byte;

end constructs;

package body constructs is
end constructs;
