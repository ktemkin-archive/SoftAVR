--************************************************************************************************
--  Primary fetch/decode unit (main decoder).
--  [In desperate need of a refactor.]
--  (ATMega103 compatible.)
--
--  Authors:
--      -- Kyle J. Temkin, Binghamton University, <ktemkin@binghamton.edu>
--      -- Ruslan Leptenok, original core designer
--************************************************************************************************

library ieee;
use ieee.std_logic_1164.all;

entity generic_register is port(

  --Global control signals.
  clk        : in std_logic;
  ld         : in std_logic := '1';
  reset      : in std_logic;

  --Data input and output.
  d     : in std_logic_vector;
  q     : out std_logic_vector
);
end;

architecture behavioral of generic_register is 
begin

  --Short behavioral description of our register.
  q <= 
    (q'range => '0') when reset = '0' else
    d when rising_edge(clk) and ld = '1';

end;
