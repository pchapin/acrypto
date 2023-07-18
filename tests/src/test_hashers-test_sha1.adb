---------------------------------------------------------------------------
-- FILE    : test_hashers-test_sha1.adb
-- SUBJECT : Procedure to test the SHA1 implementation.
-- AUTHOR  : (C) Copyright 2010 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
with ACO.Crypto.Algorithms.SHA1;
with ACO.Quadruple_Octet_Operations;

separate( Test_Hashers )

procedure Test_SHA1 is
   use ACO.Crypto.Algorithms.SHA1;

   Hasher : State;
   Block  : Message_Block;
   Result : Hash_Array;
   Example_String : String := "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq";

   Expected_Result : array(1 .. 3) of Hash_Array :=
     ((16#A9993E36#, 16#4706816A#, 16#BA3E2571#, 16#7850C26C#, 16#9CD0D89D#),
      (16#84983E44#, 16#1C3BD26E#, 16#BAAE4AA1#, 16#F95129E5#, 16#E54670F1#),
      (16#34AA973C#, 16#D4C4DAA4#, 16#F61EEB2B#, 16#DBAD2731#, 16#6534016F#));
begin
   -- Example 1 from FIPS 180-1.
   Prepare(Hasher);
   Block(0) := 16#61626300#;
   Update_Hash(Hasher, Block, 24);
   Partake(Hasher, Result);
   Assert(Result = Expected_Result(1), "Bad hash (example 1)");

   -- Example 2 from FIPS 180-1
   Prepare(Hasher);
   for I in Block_Index range 0 .. 13 loop
      declare
         Temp : ACO.Quadruple_Octet := 0;
      begin
         for J in 0 .. 3 loop
            Temp := ACO.Quadruple_Octet_Operations.Shift_Left(Temp, 8);
            Temp := Temp + Character'Pos(Example_String(4*Integer(I) + J + 1));
         end loop;
         Block(I) := Temp;
      end;
   end loop;
   Update_Hash(Hasher, Block, 448);
   Partake(Hasher, Result);
   Assert(Result = Expected_Result(2), "Bad hash (example 2)");

   -- Example 3 from FIPS 180-1.
   Prepare(Hasher);
   for I in 1 .. 15625 loop
      for J in Block'Range loop
         Block(J) := 16#61616161#;
      end loop;
      Update_Hash(Hasher, Block, 512);
   end loop;
   Partake(Hasher, Result);
   Assert(Result = Expected_Result(3), "Bad hash (example 3)");
end Test_SHA1;
