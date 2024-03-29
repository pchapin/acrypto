---------------------------------------------------------------------------
-- FILE    : assertions.adb
-- SUBJECT : Assertion support subprograms.
-- AUTHOR  : (C) Copyright 2009 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
with Ada.Text_IO;

package body Assertions is

   procedure Assert(Condition : Boolean; Message : String) is
   begin
      if not Condition then
         Ada.Text_IO.Put_Line("      ASSERTION FAILED: " & Message);
      end if;
   end Assert;

end Assertions;
