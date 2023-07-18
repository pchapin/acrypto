---------------------------------------------------------------------------
-- FILE    : acrypto_tests.adb
-- SUBJECT : Driver for all unit tests.
-- AUTHOR  : (C) Copyright 2009 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

with ACO.Math.Very_Longs.Test;
with Test_Block_Ciphers;
with Test_Stream_Ciphers;
with Test_Hashers;

procedure Acrypto_Tests is
begin
   Put_Line("Executing Very_Long Tests");
   ACO.Math.Very_Longs.Test.Execute;

   New_Line(1);
   Put_Line("Executing Block Cipher Tests");
   Test_Block_Ciphers.Execute;

   New_Line(1);
   Put_Line("Executing Stream Cipher Tests");
   Test_Stream_Ciphers.Execute;

   New_Line(1);
   Put_Line("Executing Hash Tests");
   Test_Hashers.Execute;
end Acrypto_Tests;
