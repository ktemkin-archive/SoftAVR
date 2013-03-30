--************************************************************************************************
--  Generic AVR status register definitions.
--
--  Authors:
--      -- Kyle J. Temkin, Binghamton University, <ktemkin@binghamton.edu>
--************************************************************************************************

library ieee;
use ieee.std_logic_1164.all;

package flags is

  -- Set of system flags.
  type flag_set is record
    h : std_ulogic;
    s : std_ulogic;
    v : std_ulogic;
    n : std_ulogic;
    z : std_ulogic;
    c : std_ulogic;
  end record;

  -- Set of system flags, with the two not-quite-flags included:
  -- the interrupt enable (i) and the transfer bit (t).
  type extended_flag_set is record
    i : std_ulogic;
    t : std_ulogic;
    h : std_ulogic;
    s : std_ulogic;
    v : std_ulogic;
    n : std_ulogic;
    z : std_ulogic;
    c : std_ulogic;
  end record;

  function to_std_ulogic_vector(flags : flag_set) return std_ulogic_vector;
  function to_flag_set(flags : std_logic_vector) return flag_set;
  function to_extended_flag_set(flags : std_logic_vector) return extended_flag_set;

end;

package body flags is

  --
  -- Converts a flag_set to a standard logic vector, in the same
  -- format used for pushing/popping the SREG.
  -- 
  function to_std_ulogic_vector(flags : flag_set) return std_ulogic_vector is
  begin
    return std_ulogic_vector'(flags.h, flags.s, flags.v, flags.n, flags.z, flags.c);
  end;

  --
  -- Converts a flag_set to a standard logic vector, in the same
  -- format used for pushing/popping the SREG.
  -- 
  function to_std_ulogic_vector(flags : extended_flag_set) return std_ulogic_vector is
  begin
    return std_ulogic_vector'(flags.i, flags.t, flags.h, flags.s, flags.v, flags.n, flags.z, flags.c);
  end;

  --
  -- Converts a std_ulogic_vector to a flag_set.
  -- Accepts a vector in (HSVNZC) order; the same order as in the AVR's status register.
  -- If extra bits are provided (e.g. the I/T bits), they will be ignored.
  --
  function to_flag_set(flags : std_logic_vector) return flag_set is
  begin
    return flag_set'(h => flags(5), s => flags(4), v => flags(3), n => flags(2), z => flags(1), c => flags(0));
  end;

  --
  -- Converts a std_ulogic_vector to a flag_set.
  -- Accepts a vector in (ITHSVNZC) order; the same order as in the AVR's status register.
  -- If extra bits are provided (e.g. the I/T bits), they will be ignored.
  --
  function to_extended_flag_set(flags : std_logic_vector) return extended_flag_set is
  begin
    return extended_flag_set'(i => flags(7), t=> flags(6), h => flags(5), s => flags(4), 
                              v => flags(3), n => flags(2), z => flags(1), c => flags(0));
  end;

end;
