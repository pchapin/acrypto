---------------------------------------------------------------------------
-- FILE    : aco-math-very_longs.adb
-- SUBJECT : Implementation of an extended precision integer package.
-- AUTHOR  : (C) Copyright 2015 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with ACO.Octet_Operations;
with ACO.Double_Octet_Operations;

package body ACO.Math.Very_Longs is

   subtype Hex_Digit_Type is ACO.Octet range 0 .. 15;

   function Get_Hex_Digit(Raw_Digit : Hex_Digit) return Hex_Digit_Type is
   begin
      return (case Raw_Digit is
         when '0' .. '9' => (Character'Pos(Raw_Digit) - Character'Pos('0')),
         when 'A' .. 'F' => (Character'Pos(Raw_Digit) - Character'Pos('A')) + 10,
         when 'a' .. 'f' => (Character'Pos(Raw_Digit) - Character'Pos('a')) + 10);
   end Get_Hex_Digit;


   -- Similar to "+" except that final carry is written to Carry.
   procedure Add_And_Carry
     (L, R : in Very_Long; Result : out Very_Long; Carry : out ACO.Double_Octet)
     with
       Depends => (Result =>+ (L, R), Carry => (L, R)),
       Pre  => L.Length = R.Length and L.Length = Result.Length
   is
      L_Digit : ACO.Double_Octet;
      R_Digit : ACO.Double_Octet;
      Sum     : ACO.Double_Octet;
   begin
      Result.Long_Digits := (others => 16#00#);
      Carry := 0;
      for I in L.Long_Digits'Range loop
         L_Digit := ACO.Double_Octet(L.Long_Digits(I));
         R_Digit := ACO.Double_Octet(R.Long_Digits(I));
         Sum     := L_Digit + R_Digit + Carry;
         Carry   := Double_Octet_Operations.Shift_Right(Sum, Digit_Bits);
         Result.Long_Digits(I) := ACO.Octet(Sum and 16#00FF#);
      end loop;
   end Add_And_Carry;


   -- Similar to "-" except that final borrow is written to Borrow.
   procedure Subtract_And_Borrow
     (L, R : in Very_Long; Result : out Very_Long; Borrow : out ACO.Double_Octet)
     with
       Depends => (Result =>+ (L, R), Borrow => (L, R)),
       Pre  => L.Length = R.Length and L.Length = Result.Length
   is
      L_Digit    : ACO.Double_Octet;
      R_Digit    : ACO.Double_Octet;
      Difference : ACO.Double_Octet;
   begin
      Result.Long_Digits := (others => 16#00#);
      Borrow := 0;
      for I in L.Long_Digits'Range loop
         L_Digit    := ACO.Double_Octet(L.Long_Digits(I));
         R_Digit    := ACO.Double_Octet(R.Long_Digits(I));
         Difference := (L_Digit - R_Digit) - Borrow;
         if (Difference and 16#FF00#) /= 0 then
            Borrow := 1;
         else
            Borrow := 0;
         end if;
         Result.Long_Digits(I) := ACO.Octet(Difference and 16#00FF#);
      end loop;
   end Subtract_And_Borrow;


   ----------------------
   -- Visible Subprograms
   ----------------------

   --
   -- Constructors
   --

   function Make(Number : Natural; Length : Digit_Index_Type) return Very_Long is
      Result : Very_Long(Length);
      Temp   : Natural;
   begin
      Result.Long_Digits := (others => 16#00#);
      Temp := Number;
      for Index in Result.Long_Digits'Range loop
         Result.Long_Digits(Index) := ACO.Octet(Temp rem 256);
         Temp := Temp / 256;
      end loop;
      return Result;
   end Make;


   function Make(Number : Hex_String) return Very_Long is
      Result       : Very_Long(Number'Length/2);
      Index        : Digit_Index_Type;
      String_Index : Positive;
      H_Digit      : Hex_Digit_Type;
      L_Digit      : Hex_Digit_Type;
   begin
      Result.Long_Digits := (others => 16#00#);
      Index := Result.Long_Digits'Last;
      String_Index := Number'First;
      loop
         -- The number of hex digits left corresponds to the space left in Result.Long_Digits.
         pragma Loop_Invariant
           ((Number'Last - String_Index + 1)/2 = Positive(Index - Result.Long_Digits'First + 1));
         pragma Loop_Invariant(String_Index in Number'Range);
         pragma Loop_Invariant(Index in Result.Long_Digits'Range);

         H_Digit := Get_Hex_Digit(Number(String_Index));
         L_Digit := Get_Hex_Digit(Number(String_Index + 1));
         pragma Assert(Index in Result.Long_Digits'Range);
         Result.Long_Digits(Index) := 16 * H_Digit + L_Digit;
         exit when Index = Result.Long_Digits'First;
         Index := Index - 1;
         String_Index := String_Index + 2;
      end loop;
      return Result;
   end Make;


   --
   -- Relational Operators
   --

   function "<"(L, R : Very_Long) return Boolean is
      Result : Boolean := False;  -- Use this value if they are equal.
   begin
      for I in reverse L.Long_Digits'Range loop
         if L.Long_Digits(I) < R.Long_Digits(I) then
            Result := True;
            exit;
         end if;
         if L.Long_Digits(I) > R.Long_Digits(I) then
            Result := False;
            exit;
         end if;
      end loop;

      return Result;
   end "<";


   function "<="(L, R : Very_Long) return Boolean is
   begin
      return (L < R) or (L = R);
   end "<=";


   function ">"(L, R : Very_Long) return Boolean is
   begin
      return not (L <= R);
   end ">";


   function ">="(L, R : Very_Long) return Boolean is
   begin
      return not (L < R);
   end ">=";


   function Number_Of_Digits(Number : Very_Long) return Digit_Count_Type
     with
       Refined_Post =>
         (Number_Of_Digits'Result <= Number.Length) and
         (if Number_Of_Digits'Result > 0 then
            Number.Long_Digits(Number_Of_Digits'Result) /= 0) and
         (for all I in (Number_Of_Digits'Result + 1) .. Number.Long_Digits'Last =>
            Number.Long_Digits(I) = 0)
   is
      Digit_Count : Digit_Count_Type := 0;
   begin
      if not Is_Zero(Number) then
         for I in Number.Long_Digits'Range loop
            if Number.Long_Digits(I) /= 0 then
               Digit_Count := I;
            end if;

            pragma Loop_Invariant
              ((if Digit_Count > 0 then
                  (Number.Long_Digits(Digit_Count) /= 0 and Digit_Count in 1 .. I)) and
               (if I > Digit_Count then
                  (for all J in Digit_Count + 1 .. I => Number.Long_Digits(J) = 0)));
         end loop;
      end if;
      return Digit_Count;
   end Number_Of_Digits;


   --
   -- Arithmetic Operators
   --

   function "+"(L, R : Very_Long) return Very_Long is
      Result  : Very_Long(L.Length);
      L_Digit : ACO.Double_Octet;
      R_Digit : ACO.Double_Octet;
      Sum     : ACO.Double_Octet;
      Carry   : ACO.Double_Octet;
   begin
      Result.Long_Digits := (others => 16#00#);
      Carry := 0;
      for I in L.Long_Digits'Range loop
         L_Digit := ACO.Double_Octet(L.Long_Digits(I));
         R_Digit := ACO.Double_Octet(R.Long_Digits(I));
         Sum     := L_Digit + R_Digit + Carry;
         Carry   := Double_Octet_Operations.Shift_Right(Sum, Digit_Bits);
         Result.Long_Digits(I) := ACO.Octet(Sum and 16#00FF#);
      end loop;
      return Result;
   end "+";


   function "-"(L, R : Very_Long) return Very_Long is
      Result     : Very_Long(L.Length);
      L_Digit    : ACO.Double_Octet;
      R_Digit    : ACO.Double_Octet;
      Difference : ACO.Double_Octet;
      Borrow     : ACO.Double_Octet;
   begin
      Result.Long_Digits := (others => 16#00#);
      Borrow := 0;
      for I in L.Long_Digits'Range loop
         L_Digit    := ACO.Double_Octet(L.Long_Digits(I));
         R_Digit    := ACO.Double_Octet(R.Long_Digits(I));
         Difference := (L_Digit - R_Digit) - Borrow;
         if (Difference and 16#FF00#) /= 0 then
            Borrow := 1;
         else
            Borrow := 0;
         end if;
         Result.Long_Digits(I) := ACO.Octet(Difference and 16#00FF#);
      end loop;
      return Result;
   end "-";


   -- This is "Algorithm M" from Knuth's "The Art of Computer Programming, Volume 2: Semi-
   -- numerical Algorithms" (third edition, published by Addison-Wesley, copyright 1998, pages
   -- 268-270).
   --
   function "*"(L, R : Very_Long) return Very_Long is
      Result : Very_Long(L.Length);
      L_Digit : ACO.Double_Octet;
      R_Digit : ACO.Double_Octet;
      T_Digit : ACO.Double_Octet;
      Temp    : ACO.Double_Octet;
      Carry   : ACO.Double_Octet;
   begin
      -- Prepare Result's digit array.
      Result.Long_Digits := (others => 16#00#);

      -- Do the multiplication.
      for J in R.Long_Digits'Range loop
         Carry := 0;
         for I in L.Long_Digits'Range loop
            L_Digit := ACO.Double_Octet(L.Long_Digits(I));
            R_Digit := ACO.Double_Octet(R.Long_Digits(J));
            if I + J - 1 in Result.Long_Digits'Range then
               T_Digit := ACO.Double_Octet(Result.Long_Digits(I + J - 1));
               Temp    := (L_Digit * R_Digit) + T_Digit + Carry;
               Result.Long_Digits(I + J - 1) := ACO.Octet(Temp and 16#00FF#);
               Carry   := Double_Octet_Operations.Shift_Right(Temp, Digit_Bits);
            end if;
         end loop;
      end loop;
      return Result;
   end "*";


   function Multiply(L, R : Very_Long) return Very_Long is
      Result  : Very_Long(L.Length + R.Length);
      L_Digit : ACO.Double_Octet;
      R_Digit : ACO.Double_Octet;
      T_Digit : ACO.Double_Octet;
      Temp    : ACO.Double_Octet;
      Carry   : ACO.Double_Octet;
   begin
      -- Prepare Result's digit array.
      Result.Long_Digits := (others => 16#00#);

      -- Do the multiplication.
      for J in R.Long_Digits'Range loop
         Carry := 0;
         for I in L.Long_Digits'Range loop
            L_Digit  := ACO.Double_Octet(L.Long_Digits(I));
            R_Digit  := ACO.Double_Octet(R.Long_Digits(J));
            T_Digit  := ACO.Double_Octet(Result.Long_Digits(I + J - 1));
            Temp     := (L_Digit * R_Digit) + T_Digit + Carry;
            Result.Long_Digits(I + J - 1) := ACO.Octet(Temp and 16#00FF#);
            Carry    := Double_Octet_Operations.Shift_Right(Temp, Digit_Bits);
         end loop;
         Result.Long_Digits(L.Length + J) := ACO.Octet(Carry and 16#00FF#);
      end loop;
      return Result;
   end Multiply;


   procedure Divide
     (Dividend  : in  Very_Long;
      Divisor   : in  Very_Long;
      Quotient  : out Very_Long;
      Remainder : out Very_Long) is separate;

   --
   -- Field F_p Arthemtic Operators
   --

   function Add_Fp(L, R : Very_Long; Prime : Very_Long) return Very_Long is
      Result : Very_Long(L.Length);
      Carry  : ACO.Double_Octet;
   begin
      Add_And_Carry(L, R, Result, Carry);
      if Carry = 1 or else (Result >= Prime) then
         Result := Result - Prime;
      end if;
      return Result;
   end Add_Fp;


   function Subtract_Fp(L, R : Very_Long; Prime : Very_Long) return Very_Long is
      Result : Very_Long(L.Length);
      Borrow : ACO.Double_Octet;
   begin
      Subtract_And_Borrow(L, R, Result, Borrow);
      if Borrow = 1 then
         Result := Result + Prime;
      end if;
      return Result;
   end Subtract_Fp;


   --
   -- Bit Access
   --

   function Get_Bit(Number : in Very_Long; Bit_Number : in Bit_Index_Type) return Bit is
      Digit_Number : Digit_Index_Type := Digit_Index_Type(1 + Bit_Number / Digit_Bits);
      Bit_Position : Bit_Index_Type := Bit_Number mod Digit_Bits;
      Mask   : ACO.Octet := Octet_Operations.Shift_Left(1, Natural(Bit_Position));
      Digit  : ACO.Octet;
      Result : Bit;
   begin
      if Digit_Number > Number.Length then
         Result := 0;
      else
         Digit := Number.Long_Digits(Digit_Number);
         if (Digit and Mask) /= 0 then
            Result := 1;
         else
            Result := 0;
         end if;
      end if;
      return Result;
   end Get_Bit;


   procedure Put_Bit
     (Number : in out Very_Long; Bit_Number : in Bit_Index_Type; Bit_Value  : in Bit) is

      Digit_Number : Digit_Index_Type := Digit_Index_Type(1 + Bit_Number / Digit_Bits);
      Bit_Position : Bit_Index_Type := Bit_Number mod Digit_Bits;
      Digit : ACO.Octet;
   begin
      Digit := Number.Long_Digits(Digit_Number);
      if Bit_Value = 1 then
         Digit := Digit or Octet_Operations.Shift_Left(1, Natural(Bit_Position));
      else
         Digit := Digit and not Octet_Operations.Shift_Left(1, Natural(Bit_Position));
      end if;
      Number.Long_Digits(Digit_Number) := Digit;
   end Put_Bit;

end ACO.Math.Very_Longs;
