library IEEE;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package simulation_aides is

  --
  -- Function that converts a std_logic_vector to a string.
  --
  function to_string(vector : std_logic_vector) return string;

  function to_int_string(vector : std_logic_vector) return string;

end simulation_aides;

package body simulation_aides is

   function to_string (vector : std_logic_vector) return string is 
      variable result : string (1 to vector'length); 
      variable v : std_logic_vector (result'range) := vector; 
   begin 
      for i in result'range loop 
         case v(i) is 
            when 'U' => 
               result(i) := 'U'; 
            when 'X' => 
               result(i) := 'X'; 
            when '0' => 
               result(i) := '0'; 
            when '1' => 
               result(i) := '1'; 
            when 'Z' => 
               result(i) := 'Z'; 
            when 'W' => 
               result(i) := 'W'; 
            when 'L' => 
               result(i) := 'L'; 
            when 'H' => 
               result(i) := 'H'; 
            when '-' => 
               result(i) := '-'; 
         end case; 
      end loop; 
      return result; 
   end; 

  function to_int_string(vector : std_logic_vector) return string is
  begin
    return integer'image(to_integer(unsigned(vector)));
  end;
 
end simulation_aides;
