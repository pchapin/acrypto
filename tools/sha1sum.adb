---------------------------------------------------------------------------
-- FILE    : sha1sum.ads
-- SUBJECT : Program to compute the SHA1 hash of a file.
-- AUTHOR  : (C) Copyright 2010 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Sequential_IO;
with Ada.Text_IO;

with ACO;
with ACO.Quadruple_Octet_Operations;
with ACO.Crypto.Algorithms.SHA1;

procedure SHA1Sum is
   type Byte_Counter_Type is range 0 .. 2**63 - 1;
   package Octet_IO is new Ada.Sequential_IO(Element_Type => ACO.Octet);
   package Byte_Counter_IO is new Ada.Text_IO.Integer_IO(Byte_Counter_Type);
   package SHA1 renames ACO.Crypto.Algorithms.SHA1;

   Input_File   : Octet_IO.File_Type;
   Byte         : ACO.Octet;
   Byte_Counter : Byte_Counter_Type := 0;

   subtype Buffer_Index_Type is Positive range 1 .. 64;
   Buffer       : array(Buffer_Index_Type) of ACO.Octet;
   Buffer_Index : Buffer_Index_Type := Buffer_Index_Type'First;

   Hash_Engine  : SHA1.State;
   Message      : SHA1.Message_Block;
   Result_Hash  : SHA1.Hash_Array;

   -- Copies the full buffer of octets into the SHA1 message buffer.
   procedure Prepare_Message is
      Buffer_Index : Buffer_Index_Type;
      Value        : ACO.Quadruple_Octet;

      use type ACO.Quadruple_Octet;
      use type SHA1.Block_Index;
   begin
      for I in Message'Range loop
         Buffer_Index := Natural(4 * I) + Buffer_Index_Type'First;
         Value := 0;
         for J in Buffer_Index .. Buffer_Index + 3 loop
            Value := Value or ACO.Quadruple_Octet_Operations.Shift_Left
              (ACO.Quadruple_Octet(Buffer(J)), 8 * (3 - (J - Buffer_Index)));
         end loop;
         Message(I) := Value;
      end loop;
   end Prepare_Message;


   -- Display the final hash in a nice way.
   procedure Put(Hash : in SHA1.Hash_Array) is
      Value : ACO.Quadruple_Octet;
      Mask  : ACO.Quadruple_Octet;
      Digit : ACO.Quadruple_Octet;
      Digit_Table : array(0 .. 15) of Character :=
        ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

      use type ACO.Quadruple_Octet;
   begin
      for I in Hash'Range loop
         Value := Hash(I);
         Mask  := 16#F0000000#;
         for J in 1 .. 8 loop
            Digit := ACO.Quadruple_Octet_Operations.Shift_Right(Value and Mask, 4*(8 - J));
            Mask  := ACO.Quadruple_Octet_Operations.Shift_Right(Mask, 4);
            Ada.Text_IO.Put(Digit_Table(Natural(Digit)));
         end loop;
      end loop;
   end Put;

begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line("Usage: sha1sum file_name");
   else
      SHA1.Prepare(Hash_Engine);
      Octet_IO.Open(Input_File, Octet_IO.In_File, Ada.Command_Line.Argument(1));
      while not Octet_IO.End_Of_File(Input_File) loop
         Octet_IO.Read(Input_File, Byte);
         Byte_Counter := Byte_Counter + 1;
         Buffer(Buffer_Index) := Byte;
         if Buffer_Index /= Buffer_Index_Type'Last then
            Buffer_Index := Buffer_Index + 1;
         else
            Prepare_Message;
            SHA1.Update_Hash(Hash_Engine, Message, 512);
            Buffer_Index := Buffer_Index_Type'First;
         end if;

         -- Print progress message.
         if Byte_Counter rem (1024 * 1024) = 0 then
            Ada.Text_IO.Put(Ada.Characters.Latin_1.CR);
            Ada.Text_IO.Put("Bytes processed = ");
            Byte_Counter_IO.Put(Byte_Counter, Width => 0);
         end if;
      end loop;
      Octet_IO.Close(Input_File);
      Prepare_Message;
      SHA1.Update_Hash(Hash_Engine, Message, 8 * (Buffer_Index - Buffer_Index_Type'First));
      SHA1.Partake(Hash_Engine, Result_Hash);

      -- Display final results.
      Ada.Text_IO.Put(Ada.Characters.Latin_1.CR);
      Ada.Text_IO.Put("Bytes processed = ");
      Byte_Counter_IO.Put(Byte_Counter, Width => 0);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("SHA1 = ");
      Put(Result_Hash);
      Ada.Text_IO.New_Line;
   end if;

exception
   when Octet_IO.Status_Error =>
      Ada.Text_IO.Put_Line("File already open (this error is unexpected)");

   when Octet_IO.Name_Error | Octet_IO.Use_Error =>
      Ada.Text_IO.Put_Line("Unable to open " & Ada.Command_Line.Argument(1) & " for reading");
end SHA1Sum;
