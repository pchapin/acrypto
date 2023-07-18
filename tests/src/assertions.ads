---------------------------------------------------------------------------
-- FILE    : assertions.ads
-- SUBJECT : Assertion support subprograms.
-- AUTHOR  : (C) Copyright 2009 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------

package Assertions is
   procedure Assert(Condition : Boolean; Message : String);
end Assertions;
