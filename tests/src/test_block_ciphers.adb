---------------------------------------------------------------------------
-- FILE    : test_block_ciphers.adb
-- SUBJECT : Test package for ACO.Crypto.Block_Cipher
-- AUTHOR  : (C) Copyright 2009 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

with Ada.Text_IO;
with ACO;                          use ACO;
with ACO.Crypto.Block_Cipher;      use ACO.Crypto.Block_Cipher;
with ACO.Crypto.Block_Cipher.None; use ACO.Crypto.Block_Cipher.None;
with Assertions;

package body Test_Block_Ciphers is
   use Assertions;

   procedure Round_Trip(E, D : in out Block_Cipher'Class) is

      Block_Count : constant := 4;
      Octet_Count : Natural := Block_Size(E)/Octet'Size;
      Data_1      : Octet_Array(0 .. Block_Count*Octet_Count - 1);
      Data_2      : Octet_Array(0 .. Block_Count*Octet_Count - 1);
   begin
      Assert(E.Block_Size = D.Block_Size, "Inconsistent ciphers in Round_Trip");

      -- In-place encryption to in-place decryption.
      for I in Data_1'Range loop
         Data_1(I) := Octet(I);
      end loop;
      for I in Natural range 0 .. Block_Count - 1 loop
         E.Encrypt(Data_1(I*Octet_Count .. (I+1)*Octet_Count - 1));
         D.Decrypt(Data_1(I*Octet_Count .. (I+1)*Octet_Count - 1));
      end loop;
      for I in Data_1'Range loop
         Assert(Data_1(I) = Octet(I), "In-place encrypt/decrypt round trip failed");
      end loop;

      -- Copying encryption to copying decryption.
      for I in Data_1'Range loop
         Data_1(I) := Octet(I);
      end loop;
      for I in Natural range 0 .. Block_Count - 1 loop
         E.Encrypt(Data_1(I*Octet_Count .. (I+1)*Octet_Count - 1),
                   Data_2(I*Octet_Count .. (I+1)*Octet_Count - 1));
         D.Decrypt(Data_2(I*Octet_Count .. (I+1)*Octet_Count - 1),
                   Data_1(I*Octet_Count .. (I+1)*Octet_Count - 1));
      end loop;
      for I in Data_1'Range loop
         Assert(Data_1(I) = Octet(I), "Copying encrypt/decrypt round trip failed");
      end loop;

      -- In-place encryption to copying decryption.
      for I in Data_1'Range loop
         Data_1(I) := Octet(I);
      end loop;
      for I in Natural range 0 .. Block_Count - 1 loop
         E.Encrypt(Data_1(I*Octet_Count .. (I+1)*Octet_Count - 1));
         D.Decrypt(Data_1(I*Octet_Count .. (I+1)*Octet_Count - 1),
                   Data_2(I*Octet_Count .. (I+1)*Octet_Count - 1));
      end loop;
      for I in Data_2'Range loop
         Assert(Data_2(I) = Octet(I), "In-place encrypt to copying decrypt round trip failed");
      end loop;

      -- Copying encryption to in-place decryption.
      for I in Data_1'Range loop
         Data_1(I) := Octet(I);
      end loop;
      for I in Natural range 0 .. Block_Count - 1 loop
         E.Encrypt(Data_1(I*Octet_Count .. (I+1)*Octet_Count - 1),
                   Data_2(I*Octet_Count .. (I+1)*Octet_Count - 1));
         D.Decrypt(Data_2(I*Octet_Count .. (I+1)*Octet_Count - 1));
      end loop;
      for I in Data_2'Range loop
         Assert(Data_2(I) = Octet(I), "Copying encrypt to in-place decrypt round trip failed");
      end loop;
   end Round_Trip;


   E_Cipher : aliased Null_Cipher;
   D_Cipher : aliased Null_Cipher;

   procedure Test_Null_Cipher is
   begin
      Make(E_Cipher, 64);
      Make(D_Cipher, 64);
      Round_Trip(E_Cipher, D_Cipher);
   end Test_Null_Cipher;


   procedure Test_CBC_Mode is
      E_CBC : CBC_Mode;
      D_CBC : CBC_Mode;
      IV    : Octet_Array(0..7) :=  (1, 2, 3, 4, 5, 6, 7, 8);
   begin
      Make(E_Cipher, 64);
      Make(D_Cipher, 64);
      Make(E_CBC, E_Cipher'Access, IV);
      Make(D_CBC, D_Cipher'Access, IV);
      Round_Trip(E_CBC, D_CBC);
   end Test_CBC_Mode;


   procedure Test_AES is separate;
   procedure Test_Blowfish is separate;


   procedure Execute is
   begin
      Ada.Text_IO.Put_Line("... Null cipher"); Test_Null_Cipher;
      Ada.Text_IO.Put_Line("... CBC mode");    Test_CBC_Mode;
      Ada.Text_IO.Put_Line("... AES");         Test_AES;
      Ada.Text_IO.Put_Line("... Blowfish");    Test_Blowfish;
   end Execute;

end Test_Block_Ciphers;
