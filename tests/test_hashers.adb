---------------------------------------------------------------------------
-- FILE    : test_hashes.adb
-- SUBJECT : Test package for ACO.Crypto.Hash
-- AUTHOR  : (C) Copyright 2009 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

with Ada.Text_IO;
with ACO;                  use ACO;
with ACO.Crypto.Hash;      use ACO.Crypto.Hash;
with ACO.Crypto.Hash.None; use ACO.Crypto.Hash.None;
with Assertions;

package body Test_Hashers is
   use Assertions;

   procedure Test_Null_Hasher is
      Hashing_Object : Null_Hasher;
   begin
      Hashing_Object.Start_Hashing;
      Hashing_Object.Compute_Hash( (0, 1, 2, 3, 4, 5, 6, 7) );
      Hashing_Object.Finish_Hashing;
      declare
         Result : Hash_Value := Hashing_Object.Retrieve_Hash;
      begin
         Assert(To_String(Result) = "00", "Unexpected hash value");
      end;
   end Test_Null_Hasher;


   procedure Test_SHA1 is separate;


   procedure Execute is
   begin
      Ada.Text_IO.Put_Line("... Null hasher"); Test_Null_Hasher;
      Ada.Text_IO.Put_Line("... SHA1 hasher"); Test_SHA1;
   end Execute;

end Test_Hashers;
