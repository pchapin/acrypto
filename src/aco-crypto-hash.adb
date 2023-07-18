---------------------------------------------------------------------------
-- FILE    : aco-crypto-hash.adb
-- SUBJECT : Implementation of top level hash interface package.
-- AUTHOR  : (C) Copyright 2008 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ACO.Octet_Operations;  use ACO.Octet_Operations;

package body ACO.Crypto.Hash is

   function To_String(Value : Hash_Value) return String is
      Hex_Digits : array(Octet range 0 .. 15) of Character :=
        ( '0', '1', '2', '3', '4', '5', '6', '7',
          '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

      Result : Unbounded_String;
   begin
      for I in Value.Hash_Buffer'Range loop
         Append(Result, Hex_Digits(Shift_Right(Value.Hash_Buffer(I) and 16#F0#, 4)) );
         Append(Result, Hex_Digits(            Value.Hash_Buffer(I) and 16#0F#    ) );
      end loop;
      return To_String(Result);
   end To_String;


   procedure Start_Hashing(Hash_Object : in out Stream_Hasher) is
   begin
      Hash_Object.Engine.Start_Hashing;
   end Start_Hashing;


   procedure Finish_Hashing(Hash_Object : in out Stream_Hasher) is
   begin
      Hash_Object.Engine.Finish_Hashing;
   end Finish_Hashing;


   function Retrieve_Hash(Hash_Object : Stream_Hasher) return Hash_Value is
   begin
      return Hash_Object.Engine.Retrieve_Hash;
   end Retrieve_Hash;


   procedure Read(Hash_Object : in out Stream_Hasher;
                  Item        : out Ada.Streams.Stream_Element_Array;
                  Last        : out Ada.Streams.Stream_Element_Offset) is
   begin
      raise Read_Hash;
   end Read;


   -- This is a horrible hack of type conversions. Is there a better way?
   procedure Write(Hash_Object : in out Stream_Hasher;
                   Item        : in Ada.Streams.Stream_Element_Array) is
      Buffer : Octet_Array(Natural(Item'First) .. Natural(Item'Last));
   begin
      for I in Buffer'Range loop
         Buffer(I) := Octet(Item(Ada.Streams.Stream_Element_Offset(I)));
      end loop;
      Hash_Object.Engine.Compute_Hash(Buffer);
   end Write;

end ACO.Crypto.Hash;
