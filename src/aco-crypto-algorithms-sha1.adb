---------------------------------------------------------------------------
-- FILE    : aco-crypto-algorithms-sha1.adb
-- SUBJECT : Body of a package holding the raw SHA1 hash algorithm.
-- AUTHOR  : (C) Copyright 2014 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with ACO.Bit_Operations;
with ACO.Quadruple_Octet_Operations;
with ACO.Octuple_Octet_Operations;

package body ACO.Crypto.Algorithms.SHA1 is
   -- All_Checks is a little heavy handed. It includes things that SPARK's proofs don't cover (such as stack overflow).
   -- The full list is long so this should probably just consider what is actually done in the code below.
   -- Benchmark experiments show that the pragmas below actually slow the program down!
   --pragma Suppress(Division_Check);
   --pragma Suppress(Index_Check);
   --pragma Suppress(Length_Check);
   --pragma Suppress(Overflow_Check);
   --pragma Suppress(Range_Check);

   -- We can't just ignore everything because we want precondition checks on visible subprograms (called by non-SPARK units).
   -- A full list is long so this should probably just consider what is actually done in the code below.
   pragma Assertion_Policy
     (Assert => Ignore,
      Post   => Ignore,
      Loop_Invariant => Ignore);

   type W_Index is range 0 .. 79;
   type W_Array is array(W_Index) of ACO.Quadruple_Octet;

   -- See FIPS 180-1 for the specifics of this function. The names used here reflect that source.
   function F(T : W_Index; B, C, D : ACO.Quadruple_Octet) return ACO.Quadruple_Octet is
      Result : ACO.Quadruple_Octet;
   begin
      case T is
         when 0 .. 19 =>
            Result := (B and C) or ((not B) and D);

         when 20 .. 39 =>
            Result := B xor C xor D;

         when 40 .. 59 =>
            Result := (B and C) or (B and D) or (C and D);

         when 60 .. 79 =>
            Result := B xor C xor D;
      end case;
      return Result;
   end F;


   -- This function returns a constant based on the value of T.
   function K(T : W_Index) return ACO.Quadruple_Octet is
      Result : ACO.Quadruple_Octet;
   begin
      case T is
         when  0 .. 19 => Result := 16#5A827999#;
         when 20 .. 39 => Result := 16#6ED9EBA1#;
         when 40 .. 59 => Result := 16#8F1BBCDC#;
         when 60 .. 79 => Result := 16#CA62C1D6#;
      end case;
      return Result;
   end K;


   procedure Prepare(S : out State) is
   begin
      S.Total_Bit_Count := 0;
      S.H := (0 => 16#67452301#, 1 => 16#EFCDAB89#, 2 => 16#98BADCFE#, 3 => 16#10325476#, 4 => 16#C3D2E1F0#);
      S.Active := True;
   end Prepare;


   procedure Internal_Update_Hash(S : in out State; M : in Message_Block)
     with
       Depends => (S => (S, M))
   is
      W    : W_Array := W_Array'(others => 0);
      Temp : ACO.Quadruple_Octet;
      A, B, C, D, E : ACO.Quadruple_Octet;
   begin
      -- Part (a)
      for T in W_Index range 0 .. 15 loop
         W(T) := M(Block_Index(T));
      end loop;

      -- Part (b)
      for T in W_Index range 16 .. 79 loop
         W(T) := Quadruple_Octet_Operations.Rotate_Left(Value => W(T - 3) xor W(T - 8) xor W(T - 14) xor W(T - 16), Count => 1);
      end loop;

      -- Part (c)
      A := S.H(0);
      B := S.H(1);
      C := S.H(2);
      D := S.H(3);
      E := S.H(4);

      -- Part (d)
      for T in W_Index range 0 .. 79 loop
         Temp := Quadruple_Octet_Operations.Rotate_Left(Value => A, Count => 5);
         Temp := Temp + F(T, B, C, D) + E + W(T) + K(T);
         E := D;
         D := C;
         C := Quadruple_Octet_Operations.Rotate_Left(Value => B, Count => 30);
         B := A;
         A := Temp;
      end loop;

      -- Part (e)
      S.H(0) := S.H(0) + A;
      S.H(1) := S.H(1) + B;
      S.H(2) := S.H(2) + C;
      S.H(3) := S.H(3) + D;
      S.H(4) := S.H(4) + E;
   end Internal_Update_Hash;


   procedure Update_Hash(S : in out State; M : in out Message_Block; Number_Of_Bits : in Message_Bit_Count) is

      -- This procedure places a 1 bit at the end of the message and zeros out the rest of the message block. The message length
      -- (in bits) must be put at the end of the block, but we must first verify that there is sufficient space. That step is
      -- not handled here.
      --
      procedure Compute_Message_Padding
        with
          Global  => (Input => Number_Of_Bits, In_Out => M),
          Depends => (M =>+ Number_Of_Bits),
          Pre     => (Number_Of_Bits < Message_Bit_Count'Last)
      is
         Padding_Start_Word  : Block_Index;
         Padding_Offset      : Natural;
         Padding_On_Mask     : ACO.Quadruple_Octet;
         Padding_Off_Mask    : ACO.Quadruple_Octet;
      begin
         Padding_Start_Word := Block_Index(Number_Of_Bits / 32);
         Padding_Offset := (32 - (Number_Of_Bits mod 32)) - 1;
         Padding_On_Mask  := Quadruple_Octet_Operations.Shift_Left(16#00000001#, Padding_Offset);
         Padding_Off_Mask := Quadruple_Octet_Operations.Shift_Left(16#FFFFFFFF#, Padding_Offset);
         M(Padding_Start_Word) := M(Padding_Start_Word) or  Padding_On_Mask;
         M(Padding_Start_Word) := M(Padding_Start_Word) and Padding_Off_Mask;
         for I in Padding_Start_Word + 1 .. Block_Index'Last loop
            M(I) := 0;
         end loop;
      end Compute_Message_Padding;

   begin -- Update_Hash
      if S.Active then
         -- TODO: If Total_Bit_Count wraps around, bad things will happen.
         S.Total_Bit_Count := S.Total_Bit_Count + ACO.Octuple_Octet(Number_Of_Bits);

         if Number_Of_Bits = Message_Bit_Count'Last then
            Internal_Update_Hash(S, M);
         else
            -- If there ie enough space for the message length at the end of this block...
            if Number_Of_Bits <= Message_Bit_Count'Last - 65 then
               Compute_Message_Padding;
            else
               -- Otherwise put the message length in an empty block by itself.
               Compute_Message_Padding;
               Internal_Update_Hash(S, M);
               M := Message_Block'(others => 0);
            end if;
            ACO.Bit_Operations.Split64_To_Word32(S.Total_Bit_Count, Most => M(14), Least => M(15));
            Internal_Update_Hash(S, M);
            S.Active := False;  -- Sorry. Can't accept any more blocks after doing padding!
         end if;
      end if;
   end Update_Hash;


   procedure Partake(S : in out State; Result : out Hash_Array) is
      M : Message_Block := (0 => 16#80000000#, others => 0); -- Used only if padding needed.
   begin
      -- If still active, that means the last block was exactly 512 bits. Pad now.
      if S.Active then
         ACO.Bit_Operations.Split64_To_Word32(S.Total_Bit_Count, Most => M(14), Least => M(15));
         Internal_Update_Hash(S, M);
         S.Active := False;  -- Sorry. Can't accept any more blocks after doing padding!
      end if;
      Result := S.H;
   end Partake;

end ACO.Crypto.Algorithms.SHA1;

