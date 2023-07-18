---------------------------------------------------------------------------
-- FILE    : generic_block_cipher.adb
-- SUBJECT : A benchmark procedure for block ciphers.
-- AUTHOR  : (C) Copyright 2008 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Calendar;
with Ada.Float_Text_IO;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with ACO.Access_Types;
with ACO.Crypto.Block_Cipher;

procedure Generic_Block_Cipher(E_Cipher : in out Cipher_Type; D_Cipher : in out Cipher_Type) is

   Workspace_Size  : constant := 1024*1024;
   Pass_Count      : constant := 200;
   Workload        : constant := (Pass_Count * Workspace_Size) / (1024 * 1024);  -- In MiB.
   Workspace       : ACO.Access_Types.Octet_Array_Access;
   Byte_Block_Size : Natural := Block_Size(E_Cipher) / 8;
   Block_Count     : Natural := Workspace_Size / Byte_Block_Size;
   Start_Time      : Ada.Calendar.Time;
   Stop_Time       : Ada.Calendar.Time;
   Result_Okay     : Boolean := True;
   use type Ada.Calendar.Time;
   use type ACO.Octet;

   procedure Free is
     new Ada.Unchecked_Deallocation(ACO.Octet_Array, ACO.Access_Types.Octet_Array_Access);
begin
   Workspace := new ACO.Octet_Array(0 .. Workspace_Size - 1);
   for I in Workspace'Range loop
      Workspace(I) := 16#00#;
   end loop;

   Start_Time := Ada.Calendar.Clock;
   for Pass in 1 .. Pass_Count loop
      for I in 0 .. Block_Count - 1 loop
         Encrypt(E_Cipher, Workspace(I*Byte_Block_Size .. (I+1)*Byte_Block_Size - 1));
      end loop;
   end loop;
   Stop_Time := Ada.Calendar.Clock;

   Ada.Text_IO.Put("Encryption Rate (Nondispatching) = ");
   Ada.Float_Text_IO.Put(Float(Workload)/Float(Stop_Time - Start_Time));
   Ada.Text_IO.Put(" MiB/s");
   Ada.Text_IO.New_Line;

   Start_Time := Ada.Calendar.Clock;
   for Pass in 1 .. Pass_Count loop
      for I in reverse 0 .. Block_Count - 1 loop
         Decrypt(D_Cipher, Workspace(I*Byte_Block_Size .. (I+1)*Byte_Block_Size - 1));
      end loop;
   end loop;
   Stop_Time := Ada.Calendar.Clock;

   Ada.Text_IO.Put("Decryption Rate (Nondispatching) = ");
   Ada.Float_Text_IO.Put(Float(Workload)/Float(Stop_Time - Start_Time));
   Ada.Text_IO.Put(" MiB/s");
   Ada.Text_IO.New_Line;

   for I in Workspace'Range loop
      if Workspace(I) /= 16#00# then Result_Okay := False; end if;
   end loop;
   if Result_Okay then
      Ada.Text_IO.Put_Line("PASSED!");
   else
      Ada.Text_IO.Put_Line("FAILED!");
   end if;

   Free(Workspace);
end Generic_Block_Cipher;
