---------------------------------------------------------------------------
-- FILE    : dispatching_block_cipher.adb
-- SUBJECT : A benchmark procedure for block ciphers.
-- AUTHOR  : (C) Copyright 2012 by Peter Chapin
--
-- This procedure can benchmark any block cipher by dispatching to the core algorithm.
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
with Ada.Calendar;
with Ada.Float_Text_IO;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with ACO.Access_Types;
with ACO.Crypto.Block_Cipher;

procedure Dispatching_Block_Cipher
  (E_Cipher : in out ACO.Crypto.Block_Cipher.Block_Cipher'Class;
   D_Cipher : in out ACO.Crypto.Block_Cipher.Block_Cipher'Class) is

   Workspace_Size  : constant := 1024*1024;
   Pass_Count      : constant := 200;
   Workload        : constant := (Pass_Count * Workspace_Size) / (1024 * 1024);  -- In MiB.

   Workspace       : ACO.Access_Types.Octet_Array_Access;
   Byte_Block_Size : Natural := E_Cipher.Block_Size / 8;
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
         E_Cipher.Encrypt(Workspace(I*Byte_Block_Size .. (I+1)*Byte_Block_Size - 1));
      end loop;
   end loop;
   Stop_Time := Ada.Calendar.Clock;

   Ada.Text_IO.Put("Encryption Rate (Dispatching) = ");
   Ada.Float_Text_IO.Put(Float(Workload)/Float(Stop_Time - Start_Time));
   Ada.Text_IO.Put(" MiB/s");
   Ada.Text_IO.New_Line;

   Start_Time := Ada.Calendar.Clock;
   for Pass in 1 .. Pass_Count loop
      for I in reverse 0 .. Block_Count - 1 loop
         D_Cipher.Decrypt(Workspace(I*Byte_Block_Size .. (I+1)*Byte_Block_Size - 1));
      end loop;
   end loop;
   Stop_Time := Ada.Calendar.Clock;

   Ada.Text_IO.Put("Decryption Rate (Dispatching) = ");
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
end Dispatching_Block_Cipher;
