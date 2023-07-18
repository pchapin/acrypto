---------------------------------------------------------------------------
-- FILE    : aco-math-very_longs-divide.adb
-- SUBJECT : Implementation of procedure Divide
-- AUTHOR  : (C) Copyright 2015 by Peter Chapin
--
-- It is a mystery of the universe why integer division is so much more complicated to do than
-- the other three arithmetic operations. This procedure implements "Algorithm D" from Knuth's
-- "The Art of Computer Programming, Volume 2: Semi-numerical Algorithms" (third edition,
-- published by Addison-Wesley, copyright 1998, pages 272-276).
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
separate(ACO.Math.Very_Longs)

procedure Divide
  (Dividend  : in  Very_Long;
   Divisor   : in  Very_Long;
   Quotient  : out Very_Long;
   Remainder : out Very_Long) is

   -- Divisor has N digits. This subtype is used for values of N.
   -- The divisor must have at least one digit.
   subtype Divisor_Digits_Count_Type is Digit_Index_Type range 1 .. Divisor.Length;

   -- Dividend has 2*Divisor.Length = M + N digits. This subtype is used for values of M.
   -- Quotient has M + 1 digits.
   subtype Quotient_Digits_Count_Type is
     Digit_Index_Type range Divisor.Length .. 2*Divisor.Length - 1;

   -- This subtype is used for shift distances during the normalization and unnormalization.
   subtype Shift_Type is Natural range 0 .. 7;

   -- Refer to the document on multiprecision division to understand the names here.
   N               : Divisor_Digits_Count_Type;    -- Number of non-zero digits in divisor.
   M               : Quotient_Digits_Count_Type;   -- Number of digits in dividend is m + n.
   Shift_Distance  : Shift_Type;                   -- Number of bits to shift divisor.
   U               : Very_Long(Dividend.Length + 1);  -- Normalized dividend.
   V               : Very_Long(Divisor.Length);       -- Normalized divisor.
   Q_Hat           : ACO.Double_Octet;
   Current_Borrow  : ACO.Double_Octet;

   -- Gives shift distance required to move a 1 bit into the most significant position of Digit.
   function Get_Shift_Distance(Digit : ACO.Octet) return Shift_Type
     with Pre => Digit /= 0
   is
      Mask     : constant ACO.Octet := 16#01#;
      Distance : Shift_Type := 0;
   begin
      for I in Shift_Type loop
         if (Digit and Octet_Operations.Shift_Left(Mask, I)) /= 0 then
            Distance := Shift_Type'Last - I;
         end if;
      end loop;
      return Distance;
   end Get_Shift_Distance;

   -- Shifts Number to the left by Distance bits, putting the result into Result.
   procedure Full_Left_Shift
     (Number : in Very_Long; Result : out Very_Long; Distance : in Shift_Type)
     with
       Depends => (Result =>+ (Number, Distance)),
       Pre => Result.Length = Number.Length + 1
   is
      Old_Overflow  : ACO.Octet := 16#00#;
      New_Overflow  : ACO.Octet;
      Overflow_Mask : ACO.Octet;
   begin
      Result.Long_Digits := (others => 16#00#);
      Overflow_Mask := Octet_Operations.Shift_Left(16#FF#, 8 - Distance);
      for I in Number.Long_Digits'Range loop
         New_Overflow :=
           Octet_Operations.Shift_Right(Number.Long_Digits(I) and Overflow_Mask, 8 - Distance);
         Result.Long_Digits(I) := Octet_Operations.Shift_Left(Number.Long_Digits(I), Distance);
         Result.Long_Digits(I) := Result.Long_Digits(I) or Old_Overflow;
         Old_Overflow := New_Overflow;
      end loop;
      Result.Long_Digits(Result.Long_Digits'Last) := Old_Overflow;
   end Full_Left_Shift;

   -- Shifts Number to the left by Distance bits. The final overflow is dropped (should be zero
   -- when used as below).
   procedure Left_Shift(Number : in out Very_Long; Distance : in Shift_Type)
     with Depends => (Number =>+ Distance)
   is
      Old_Overflow  : ACO.Octet := 16#00#;
      New_Overflow  : ACO.Octet;
      Overflow_Mask : ACO.Octet;
   begin
      Overflow_Mask := Octet_Operations.Shift_Left(16#FF#, 8 - Distance);
      for I in Number.Long_Digits'Range loop
         New_Overflow :=
           Octet_Operations.Shift_Right(Number.Long_Digits(I) and Overflow_Mask, 8 - Distance);
         Number.Long_Digits(I) := Octet_Operations.Shift_Left(Number.Long_Digits(I), Distance);
         Number.Long_Digits(I) := Number.Long_Digits(I) or Old_Overflow;
         Old_Overflow := New_Overflow;
      end loop;
   end Left_Shift;

   -- Shifts Number to the right by Distance bits. The final overflow is dropped (should be zero
   -- (I think) when used as below).
   procedure Right_Shift(Number : in out Very_Long; Distance : in Shift_Type)
     with Depends => (Number =>+ Distance)
   is
      Old_Overflow  : ACO.Octet := 16#00#;
      New_Overflow  : ACO.Octet;
      Overflow_Mask : ACO.Octet;
   begin
      Overflow_Mask := Octet_Operations.Shift_Right(16#FF#, 8 - Distance);
      for I in reverse Number.Long_Digits'Range loop
         New_Overflow := Number.Long_Digits(I) and Overflow_Mask;
         Number.Long_Digits(I) :=
           Octet_Operations.Shift_Right(Number.Long_Digits(I), Distance);
         Number.Long_Digits(I) :=
           Number.Long_Digits(I) or Octet_Operations.Shift_Left(Old_Overflow, 8 - Distance);
         Old_Overflow := New_Overflow;
      end loop;
   end Right_Shift;

   procedure D3(J : in Digit_Index_Type)
     with
       Global => (Input => (N, U, V), Output => Q_Hat),
       Depends => (Q_Hat => (J, N, U, V)),
       Pre => (N > 1 and J + N <= U.Length)
   is
      Temporary_Digit : ACO.Double_Octet;
      R_Hat           : ACO.Double_Octet;
   begin
      Temporary_Digit :=
        256 * ACO.Double_Octet(U.Long_Digits(J + N)) +
              ACO.Double_Octet(U.Long_Digits(J + N - 1));
      Q_Hat := Temporary_Digit / ACO.Double_Octet(V.Long_Digits(N));
      -- Q_Hat has at most 9 bits.

      R_Hat := Temporary_Digit mod ACO.Double_Octet(V.Long_Digits(N));
      -- R_Hat has at most 8 bits.

      if Q_Hat = 256 or
        (Q_Hat * ACO.Double_Octet(V.Long_Digits(N - 1)) >
           256 * R_Hat + ACO.Double_Octet(U.Long_Digits((J + N) - 2))) then

         Q_Hat := Q_Hat - 1;
         R_Hat := R_Hat + ACO.Double_Octet(V.Long_Digits(N));
      end if;
      if R_Hat < 256 then
         -- This is the same test as above. Should it be moved into a separate subprogram?
         if Q_Hat = 256 or
           (Q_Hat * ACO.Double_Octet(V.Long_Digits(N - 1)) >
              256 * R_Hat + ACO.Double_Octet(U.Long_Digits((J + N) - 2))) then

            Q_Hat := Q_Hat - 1;
         end if;
      end if;
      -- At this point Q_Hat should be only an eight bit number.
   end D3;

   procedure D4(J : in Digit_Index_Type)
     with
       Global => (Input => (N, Q_Hat, V), Output => Current_Borrow, In_Out => U),
       Depends => (Current_Borrow => (J, N, Q_Hat, U, V), U => (J, N, Q_Hat, U, V))
   is
      Carry           : ACO.Double_Octet;
      Product         : ACO.Double_Octet;
      Temporary_Digit : ACO.Double_Octet;
      Future_Borrow   : ACO.Double_Octet;
   begin
      Carry := 0;
      Current_Borrow := 0;
      for Divisor_Index in Digit_Index_Type range 1 .. N loop
         Product := Q_Hat * ACO.Double_Octet(V.Long_Digits(Divisor_Index)) + Carry;
         Carry := Double_Octet_Operations.Shift_Right(Product, 8);

         Temporary_Digit := Product and 16#00FF#;
         if (ACO.Double_Octet(U.Long_Digits(J + Divisor_Index - 1)) >=
               Temporary_Digit + Current_Borrow) then

            Future_Borrow := 0;
         else
            Future_Borrow := 1;
         end if;
         U.Long_Digits(J + Divisor_Index - 1) :=
           ACO.Octet(((ACO.Double_Octet(U.Long_Digits(J + Divisor_Index - 1)) - Temporary_Digit) - Current_Borrow) and 16#00FF#);
         Current_Borrow := Future_Borrow;
      end loop;

      -- The last digit is handled as a special case.
      if ACO.Double_Octet(U.Long_Digits(J + N)) >= Carry + Current_Borrow then
         Future_Borrow := 0;
      else
         Future_Borrow := 1;
      end if;
      U.Long_Digits(J + N) :=
        ACO.Octet(((ACO.Double_Octet(U.Long_Digits(J + N)) - Carry) - Current_Borrow) and 16#00FF#);
      Current_Borrow := Future_Borrow;
   end D4;

   procedure D5(J : in Digit_Index_Type)
     with
       Global => (Input => (Current_Borrow, N, Q_Hat, V), In_Out => (Quotient, U)),
       Depends =>
         (Quotient => (Current_Borrow, J, Q_Hat, Quotient), U => (Current_Borrow, J, N, U, V))
   is
      Carry : ACO.Double_Octet;
      Sum   : ACO.Double_Octet;
   begin
      Quotient.Long_Digits(J) := ACO.Octet(Q_Hat);  -- This is where Q_Hat must be eight bits!
      if Current_Borrow /= 0 then

         -- D6 (Add Back)
         Quotient.Long_Digits(J) := Quotient.Long_Digits(J) - 1;
         Carry := 0;
         for Divisor_Index in Digit_Index_Type range 1 .. N loop
            Sum := ACO.Double_Octet(U.Long_Digits(J + Divisor_Index - 1)) +
              ACO.Double_Octet(V.Long_Digits(Divisor_Index)) + Carry;
            U.Long_Digits(J + Divisor_Index - 1) := ACO.Octet(Sum and 16#00FF#);
            Carry := Double_Octet_Operations.Shift_Right(Sum, 8);
         end loop;
         U.Long_Digits(J + N) := U.Long_Digits(J + N) + ACO.Octet(Carry and 16#00FF#);
      end if;
   end D5;

begin  -- Divide
   Quotient.Long_Digits  := (others => 16#00#);
   Remainder.Long_Digits := (others => 16#00#);

   -- D0
   N := Number_Of_Digits(Divisor);
   M := Dividend.Length - N;

   -- D1 (Normalize)
   Shift_Distance := Get_Shift_Distance(Divisor.Long_Digits(N));
   Full_Left_Shift(Dividend, U, Shift_Distance);
   V := Divisor;
   Left_Shift(V, Shift_Distance);

   -- D2 (Initialize J)
   for J in reverse Digit_Index_Type range 1 .. M + 1 loop
      -- D3 (Calculate Q_Hat)
      D3(J);

      -- D4 (Multiply and Subtract)
      D4(J);

      -- D5 (Test Remainder)
      D5(J);

      -- D7 (Loop on J)
   end loop;

   -- D8 (Unnormalize)
   for Divisor_Index in Digit_Index_Type range 1 .. N loop
      Remainder.Long_Digits(Divisor_Index) := U.Long_Digits(Divisor_Index);
   end loop;
   Right_Shift(Remainder, Shift_Distance);
end Divide;
