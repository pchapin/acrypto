---------------------------------------------------------------------------
-- FILE    : test_stream_ciphers.adb
-- SUBJECT : Test package for ACO.Crypto.Block_Cipher
-- AUTHOR  : (C) Copyright 2009 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------

with Ada.Text_IO;
with ACO;                           use ACO;
with ACO.Crypto.Stream_Cipher;      use ACO.Crypto.Stream_Cipher;
with ACO.Crypto.Stream_Cipher.None; use ACO.Crypto.Stream_Cipher.None;
with ACO.Crypto.Block_Cipher.Blowfish;
with Assertions;

package body Test_Stream_Ciphers is
   use Assertions;

   procedure Round_Trip(E, D : in out Stream_Cipher'Class) is
      Data_1 : Octet;
      Data_2 : Octet;
   begin
      -- Loop 129 times: *not* a multiple of any block size (right?)
      for I in Natural range 0 .. 128 loop
         Data_1 := Octet(I);
         E.Encrypt(Data_1);
         D.Decrypt(Data_1);
         Assert(Data_1 = Octet(I), "In-place encrypt/decrypt round trip failed");
      end loop;

      for I in Natural range 0 .. 128 loop
         Data_1 := Octet(I);
         E.Encrypt(Data_1, Data_2);
         D.Decrypt(Data_2, Data_1);
         Assert(Data_1 = Octet(I), "Copying encrypt/decrypt round trip failed");
      end loop;

      for I in Natural range 0 .. 128 loop
         Data_1 := Octet(I);
         E.Encrypt(Data_1);
         D.Decrypt(Data_1, Data_2);
         Assert(Data_2 = Octet(I), "In-place encrypt to copying decrypt round trip failed");
      end loop;

      for I in Natural range 0 .. 128 loop
         Data_1 := Octet(I);
         E.Encrypt(Data_1, Data_2);
         D.Decrypt(Data_2);
         Assert(Data_2 = Octet(I), "Copying encrypt to in-place decrypt round trip failed");
      end loop;
   end Round_Trip;


   procedure Test_Null_Cipher is
      E_Cipher : Null_Cipher;
      D_Cipher : Null_Cipher;
   begin
      Round_Trip(E_Cipher, D_Cipher);
   end Test_Null_Cipher;


   -- Use a real block cipher during the CFB mode tests.
   E_Block_Cipher : aliased ACO.Crypto.Block_Cipher.Blowfish.Blowfish_Cipher;
   D_Block_Cipher : aliased ACO.Crypto.Block_Cipher.Blowfish.Blowfish_Cipher;

   procedure Test_CFB_Mode is
      E_CFB : CFB_Mode;
      D_CFB : CFB_Mode;
      Key   : Octet_Array(0..7) := (8, 7, 6, 5, 4, 3, 2, 1);  -- Value not important.
      IV    : Octet_Array(0..7) := (1, 2, 3, 4, 5, 6, 7, 8);  -- Value not important.
   begin
      ACO.Crypto.Block_Cipher.Blowfish.Make(E_Block_Cipher, Key);
      ACO.Crypto.Block_Cipher.Blowfish.Make(D_Block_Cipher, Key);
      Make(E_CFB, E_Block_Cipher'Access, IV);
      Make(D_CFB, D_Block_Cipher'Access, IV);
      Round_Trip(E_CFB, D_CFB);
   end Test_CFB_Mode;


   --  Register test routines to call:
   procedure Execute is
   begin
      Ada.Text_IO.Put_Line("... Null cipher"); Test_Null_Cipher;
      Ada.Text_IO.Put_Line("... CFB mode");    Test_CFB_Mode;
   end Execute;

end Test_Stream_Ciphers;
