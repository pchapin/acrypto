---------------------------------------------------------------------------
-- FILE    : aco-math-very_longs-test.adb
-- SUBJECT : Test package for ACO.Math.Very_Longs
-- AUTHOR  : (C) Copyright 2015 by Peter C. Chapin
--
-- To-Do:
--
-- Test cases need to be reviewed for coverage. Testing procedures can probably be improved
-- (common code factored out (use a generic?)... more table driven relational tests, etc). It
-- would probably be a good idea to take test case data from a file. That would make it easier
-- to extend the tests later and avoid putting a large number of huge strings into this source
-- module.
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Text_IO;
with Assertions;

-- This silences warnings related to the initialization of Hex_Strings.
pragma Warnings(Off, "static expression fails static predicate check");
pragma Warnings(Off, "expression is no longer considered static");

package body ACO.Math.Very_Longs.Test is
   use Assertions;
   use type ACO.Double_Octet;

   subtype Hex_String_192 is Hex_String(1 .. 48);
   subtype Hex_String_384 is Hex_String(1 .. 96);
   subtype Very_Long_192 is Very_Long(24);
   subtype Very_Long_384 is Very_Long(48);

   --
   -- Helper subprograms
   --

   procedure Check_Bits(N : in Very_Long; Expected : in Hex_String_192) is
      Digit_Index : Positive;
      Bit_Number : Natural;
      Expected_Bit : Bit;

      type Bit_Array is array(0 .. 3) of Bit;
      Decimal_Bits : array(Character range '0' .. '9') of Bit_Array :=
        ( (0, 0, 0, 0), (0, 0, 0, 1), (0, 0, 1, 0), (0, 0, 1, 1), (0, 1, 0, 0),
          (0, 1, 0, 1), (0, 1, 1, 0), (0, 1, 1, 1), (1, 0, 0, 0), (1, 0, 0, 1) );
      Hexadecimal_Bits : array(Character range 'A' .. 'F') of Bit_Array :=
        ( (1, 0, 1, 0), (1, 0, 1, 1), (1, 1, 0, 0),
          (1, 1, 0, 1), (1, 1, 1, 0), (1, 1, 1, 1) );

   begin
      for I in Bit_Index_Type range 0 .. Bit_Index_Type(8*N.Length - 1) loop
         Digit_Index := 48 - Natural(I / 4);
         Bit_Number  := Natural(I mod 4);
         case Ada.Characters.Handling.To_Upper(Expected(Digit_Index)) is
            when '0' .. '9' =>
               Expected_Bit := Decimal_Bits(Expected(Digit_Index))(3 - Bit_Number);
            when 'A' .. 'F' =>
               Expected_Bit := Hexadecimal_Bits(Expected(Digit_Index))(3 - Bit_Number);
            when others =>
               -- This should never happen because of the Dynamic_Predicate on Hex_String.
               -- However, the Ada compiler can't statically know that, hence this case.
               --
               raise Program_Error;
         end case;
        Assert(Get_Bit(N, I) = Expected_Bit, "Bad bit");
      end loop;
   end Check_Bits;

   --
   -- Test procedures
   --

   procedure Test_Natural_Constructor is
      type Test_Case is
         record
            N : Integer;
            Expected : Hex_String_192;
         end record;

      subtype Test_Index is Integer range 1 .. 5;
      Test_Cases : array(Test_Index) of Test_Case :=
        (( N =>            1, Expected => "000000000000000000000000000000000000000000000001"),
         ( N =>     16#FFFF#, Expected => "00000000000000000000000000000000000000000000FFFF"),
         ( N =>    16#10000#, Expected => "000000000000000000000000000000000000000000010000"),
         ( N => 16#5A5A5A5A#, Expected => "00000000000000000000000000000000000000005A5A5A5A"),
         ( N => 16#7FFFFFFF#, Expected => "00000000000000000000000000000000000000007FFFFFFF"));

      Number : Very_Long_192;
   begin
      Number := Make(Number => 0, Length => 24);
      for I in Test_Index loop
         Number := Make(Number => Test_Cases(I).N, Length => 24);
         Check_Bits(Number, Test_Cases(I).Expected);
      end loop;
   end Test_Natural_Constructor;


   procedure Test_String_Constructor is
      type Test_Case is
         record
            N : Hex_String_192;
         end record;

      subtype Test_Index is Integer range 1 .. 4;
      Test_Cases : array(Test_Index) of Test_Case :=
        ((N => "000000000000000000000000000000000000000000000005"),
         (N => "000000000000000000000000000000000000000100000000"),
         (N => "00000000000000000000000000000000FFFFFFFFFFFFFFFF"),
         (N => "000000000000000000000000000000010000000000000000"));

      Number : Very_Long_192;
   begin
      for I in Test_Index loop
         Number := Make(Test_Cases(I).N);
         Check_Bits(Number, Test_Cases(I).N);
      end loop;
   end Test_String_Constructor;


   procedure Test_Relationals is
      Number1, Number2 : Very_Long_192;
   begin
      Number1 := Make(Number => 0, Length => 24);
      Number2 := Make(Number => 2, Length => 24);

      -- Compare: 0 op 2 (for op in { <, =, <=, >, >= }).
      Assert(    (Number1 <  Number2), "Bad relational ( 1)");
      Assert(not (Number1 =  Number2), "Bad relational ( 2)");
      Assert(    (Number1 <= Number2), "Bad relational ( 3)");
      Assert(not (Number1 >  Number2), "Bad relational ( 4)");
      Assert(not (Number1 >= Number2), "Bad relational ( 5)");

      -- Compare them in the other order.
      Assert(not (Number2 <  Number1), "Bad relational ( 6)");
      Assert(not (Number2 =  Number1), "Bad relational ( 7)");
      Assert(not (Number2 <= Number1), "Bad relational ( 8)");
      Assert(    (Number2 >  Number1), "Bad relational ( 9)");
      Assert(    (Number2 >= Number1), "Bad relational (10)");

      -- Compare: 1 op 2
      Number1 := Make(Number => 1, Length => 24);
      Assert(    (Number1 <  Number2), "Bad relational (11)");
      Assert(not (Number1 =  Number2), "Bad relational (12)");
      Assert(    (Number1 <= Number2), "Bad relational (13)");
      Assert(not (Number1 >  Number2), "Bad relational (14)");
      Assert(not (Number1 >= Number2), "Bad relational (15)");

      -- Compare: 2 op 2
      Number1 := Make(Number => 2, Length => 24);
      Assert(not (Number1 <  Number2), "Bad relational (16)");
      Assert(    (Number1 =  Number2), "Bad relational (17)");
      Assert(    (Number1 <= Number2), "Bad relational (18)");
      Assert(not (Number1 >  Number2), "Bad relational (19)");
      Assert(    (Number1 >= Number2), "Bad relational (20)");
   end Test_Relationals;


   procedure Test_Addition is
      type Test_Case is
         record
            X : Hex_String_192;
            Y : Hex_String_192;
            Z : Hex_String_192;  -- Z = X + Y
         end record;

      subtype Test_Index is Integer range 1 .. 9;
      Test_Cases : array(Test_Index) of Test_Case :=
        (1 => (X => "000000000000000000000000000000000000000000000000",
               Y => "000000000000000000000000000000000000000000000001",
               Z => "000000000000000000000000000000000000000000000001"),

         2 => (X => "000000000000000000000000000000000000000000000001",
               Y => "000000000000000000000000000000000000000000000000",
               Z => "000000000000000000000000000000000000000000000001"),

         3 => (X => "000000000000000000000000000000000000000000000001",
               Y => "000000000000000000000000000000000000000000000002",
               Z => "000000000000000000000000000000000000000000000003"),

         4 => (X => "000000000000000000000000000001234567890987654321",
               Y => "000000000000000000000000000000000000000000000000",
               Z => "000000000000000000000000000001234567890987654321"),

         5 => (X => "000000000000000000000000000001234567890987654321",
               Y => "000000000000000000000000000001234567890987654321",
               Z => "000000000000000000000000000002468ACF12130ECA8642"),

         6 => (X => "00000000000000000000000000000000000000000000FFFF",
               Y => "000000000000000000000000000000000000000000000001",
               Z => "000000000000000000000000000000000000000000010000"),

         7 => (X => "0000000000000000000000000000000000000000ffffffff",  -- Check lower case.
               Y => "000000000000000000000000000000000000000000000001",
               Z => "000000000000000000000000000000000000000100000000"),

         8 => (X => "00000000000000000000000000000000ffFFffFFffFFffFF",
               Y => "000000000000000000000000000000000000000000000001",
               Z => "000000000000000000000000000000010000000000000000"),

         9 => (X => "FFFFFFFFFfFfFfFfFfFfFfFfFfFffFfFfFfFfFfFfFfFfFfF",
               Y => "000000000000000000000000000000000000000000000001",
               Z => "000000000000000000000000000000000000000000000000"));

      X, Y, Sum : Very_Long_192;
      Expected : Very_Long_192;
   begin
      for I in Test_Index loop
         X := Make(Test_Cases(I).X);
         Y := Make(Test_Cases(I).Y);
         Expected := Make(Test_Cases(I).Z);
         Sum := X + Y;
         Assert(Sum = Expected, "Bad addition in case #" & Integer'Image(I));
      end loop;
   end Test_Addition;


   procedure Test_Subtraction is
      type Test_Case is
         record
            X : Hex_String_192;
            Y : Hex_String_192;
            Z : Hex_String_192;  -- Z = X - Y
         end record;

      subtype Test_Index is Integer range 1 .. 9;
      Test_Cases : array(Test_Index) of Test_Case :=
        (1 => (X => "000000000000000000000000000000000000000000000001",
               Y => "000000000000000000000000000000000000000000000001",
               Z => "000000000000000000000000000000000000000000000000"),

         2 => (X => "000000000000000000000000000000000000000000000001",
               Y => "000000000000000000000000000000000000000000000000",
               Z => "000000000000000000000000000000000000000000000001"),

         3 => (X => "000000000000000000000000000000000000000000000003",
               Y => "000000000000000000000000000000000000000000000001",
               Z => "000000000000000000000000000000000000000000000002"),

         4 => (X => "000000000000000000000000000001234567890987654321",
               Y => "000000000000000000000000000000000000000000000000",
               Z => "000000000000000000000000000001234567890987654321"),

         5 => (X => "000000000000000000000000000002468ACF12130ECA8642",
               Y => "000000000000000000000000000001234567890987654321",
               Z => "000000000000000000000000000001234567890987654321"),

         6 => (X => "000000000000000000000000000000000000000000010000",
               Y => "000000000000000000000000000000000000000000000001",
               Z => "00000000000000000000000000000000000000000000FFFF"),

         7 => (X => "000000000000000000000000000000000000000100000000",
               Y => "000000000000000000000000000000000000000000000001",
               Z => "0000000000000000000000000000000000000000ffffffff"),   -- Check lower case.

         8 => (X => "000000000000000000000000000000010000000000000000",
               Y => "000000000000000000000000000000000000000000000001",
               Z => "00000000000000000000000000000000ffFFffFFffFFffFF"),

         9 => (X => "000000000000000000000000000000000000000000000000",
               Y => "000000000000000000000000000000000000000000000001",
               Z => "FfFfFfFfFfFfFfFfFfFfFfFfFfFffFfFfFfFfFfFfFfFfFfF"));

      X, Y, Difference : Very_Long_192;
      Expected : Very_Long_192;
   begin
      for I in Test_Index loop
         X := Make(Test_Cases(I).X);
         Y := Make(Test_Cases(I).Y);
         Expected := Make(Test_Cases(I).Z);
         Difference := X - Y;
         Assert(Difference = Expected, "Bad subtraction in case #" & Integer'Image(I));
      end loop;
   end Test_Subtraction;


   procedure Test_Multiplication is
      type Test_Case is
         record
            X : Hex_String_192;
            Y : Hex_String_192;
            Z : Hex_String_192;  -- Z = X * Y
         end record;

      subtype Test_Index is Integer range 1 .. 5;
      Test_Cases : array(Test_Index) of Test_Case :=
        (1 => (X => "000000000000000000000000000000000000000000000000",
               Y => "000000000000000000000000000000000000000000000000",
               Z => "000000000000000000000000000000000000000000000000"),

         2 => (X => "000000000000000000000000000000000000000000000001",
               Y => "000000000000000000000000000000000000000000000001",
               Z => "000000000000000000000000000000000000000000000001"),

         3 => (X => "000000000000000000000000000000000000000000000001",
               Y => "000000000000000000000000000000000000000000000002",
               Z => "000000000000000000000000000000000000000000000002"),

         4 => (X => "000000000000000000000000000000000000000000000002",
               Y => "000000000000000000000000000000000000000000000001",
               Z => "000000000000000000000000000000000000000000000002"),

         5 => (X => "00000000000000000000000000000000112210F4B16C1CB1",
               Y => "000000000000000000000000000000004ECF8A71CED0C6E2",
               Z => "0000000000000000054644FA3EEF2D71E014744CFD723A42"));

      X, Y, Product : Very_Long_192;
      Expected : Very_Long_192;
   begin
      for I in Test_Index loop
         X := Make(Test_Cases(I).X);
         Y := Make(Test_Cases(I).Y);
         Expected := Make(Test_Cases(I).Z);
         Product := X * Y;
         Assert(Product = Expected, "Bad multiplication in case #" & Integer'Image(I));
      end loop;
   end Test_Multiplication;


   procedure Test_Multiplication_Full is
      type Test_Case is
         record
            X : Hex_String_192;
            Y : Hex_String_192;
            Z : Hex_String_384;  -- Z = X * Y
         end record;

      -- Test cases were computed with the 'dc' program.
      subtype Test_Index is Integer range 1 .. 2;
      Test_Cases : array(Test_Index) of Test_Case :=
        (1 => (X => "00000000000000000000000000000000112210F4B16C1CB1",
               Y => "000000000000000000000000000000004ECF8A71CED0C6E2",
               Z => "0000000000000000000000000000000000000000000000000000000000000000054644FA3EEF2D71E014744CFD723A42"),

         2 => (X => "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
               Y => "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
               Z => "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE000000000000000000000000000000000000000000000001"));

      X, Y : Very_Long_192;
      Product : Very_Long_384;
      Expected : Very_Long_384;
   begin
      for I in Test_Index loop
         X := Make(Test_Cases(I).X);
         Y := Make(Test_Cases(I).Y);
         Expected := Make(Test_Cases(I).Z);
         Product := Multiply(X, Y);
         Assert(Product = Expected, "Bad full multiplication in case #" & Integer'Image(I));
      end loop;
   end Test_Multiplication_Full;


   procedure Test_Division is
      type Test_Case is
         record
            U : Hex_String_384;
            V : Hex_String_192;
            Q : Hex_String_384;  -- Q = floor(U / V);
            R : Hex_String_192;       -- R = U mod V;
         end record;

      -- Just "typical" test cases for now. Must also exercise the "Add Back" step in Knuth's
      -- algorithm. That step occurs with low probability so it is necessary to construct
      -- special test cases to exercise it. Test cases below created with the 'dc' program on
      -- Ubuntu Linux 12.04.
      --
      subtype Test_Index is Integer range 1 .. 12;
      Test_Cases : array(Test_Index) of Test_Case :=
        -- Non-trivial with pattern.
        (1 => (U => "010203040506070809101112131415161718192021222324252627282930313233343536373839404142434445464748",
               V => "00112233445566778899AABBCCDDEEFF0011223344556677",
               Q => "00000000000000000000000000000000000000000000000F0F0F0F0F0F0F0F0F690F0F0F0F0FFF0F0F0F690F0F0F8CB8",
               R => "000870D941B118811958202890FA67E9D23AD31179E28DC0"),

         -- "Random."
         2 => (U => "73D89CAA0082F227719B9992DD37782001844A43C784B99C512A83C422A34B860CAD328659287BB21FEE282A9DF2E483",
               V => "428BB453A749C933FDC291A77CBE892001B5882345829AC7",
               Q => "000000000000000000000000000000000000000000000001BDA85E584B55AB859B1C09C3FC3BB62C84478E492FDDC56B",
               R => "39A4E4D22AD4C10B9264E0B9D8CDA672E9CB9C05E8761056"),

         -- Random with MSD of V at 01.
         3 => (U => "73D89CAA0082F227719B9992DD37782001844A43C784B99C512A83C422A34B860CAD328659287BB21FEE282A9DF2E483",
               V => "018BB453A749C933FDC291A77CBE892001B5882345829AC7",
               Q => "00000000000000000000000000000000000000000000004AF245E1C2BC1C960C537D183E413EBBA181FEDF9330BA8924",
               R => "00C52A8CE858768071B369671E296953AE9EEBD32E2AA187"),

         -- Random with MSD of V at 7F.
         4 => (U => "73D89CAA0082F227719B9992DD37782001844A43C784B99C512A83C422A34B860CAD328659287BB21FEE282A9DF2E483",
               V => "7F8BB453A749C933FDC291A77CBE892001B5882345829AC7",
               Q => "000000000000000000000000000000000000000000000000E8847ADA09C5AA87714598019D0E62B296D3D7E3EF103639",
               R => "201C036A415218687F43B0CF3F2AA96884E0C18821C87434"),

         -- Random with MSD of V at 80.
         5 => (U => "73D89CAA0082F227719B9992DD37782001844A43C784B99C512A83C422A34B860CAD328659287BB21FEE282A9DF2E483",
               V => "808BB453A749C933FDC291A77CBE892001B5882345829AC7",
               Q => "000000000000000000000000000000000000000000000000E6B56B4B96E478E4114477F727940CD4E3116D085DBC239F",
               R => "48D3931409995E53691BB2A658DE8966F037B865A1878DEA"),

         -- Random with MSD of V at FF.
         6 => (U => "73D89CAA0082F227719B9992DD37782001844A43C784B99C512A83C422A34B860CAD328659287BB21FEE282A9DF2E483",
               V => "FF8BB453A749C933FDC291A77CBE892001B5882345829AC7",
               Q => "000000000000000000000000000000000000000000000000740D55028EBFAE1701477EEE2CF64D0428EFF6728935DF82",
               R => "9FED98AA125DA4842160F1C365245FCA03FADD380299F275"),

         -- Largest possible U, smallest possible V using the "full" algorithm.
         7 => (U => "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
               V => "000000000000000000000000000000000000000000000100",
               Q => "00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
               R => "0000000000000000000000000000000000000000000000FF"),

         -- Largest possible U, largest possible V.
         8 => (U => "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
               V => "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
               Q => "000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000001",
               R => "000000000000000000000000000000000000000000000000"),

         -- Smallest possible U, smallest possible V using the "full" algorithm.
         9 => (U => "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001",
               V => "000000000000000000000000000000000000000000000100",
               Q => "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
               R => "000000000000000000000000000000000000000000000001"),

         -- Smallest possible U, largest possible V.
        10 => (U => "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001",
               V => "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
               Q => "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
               R => "000000000000000000000000000000000000000000000001"),

      -- Partial U, similar V, quotient of 1.
        11 => (U => "000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000",
               V => "7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
               Q => "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001",
               R => "000000000000000000000000000000000000000000000001"),

      -- Partial U, similar V, quotient of 0.
        12 => (U => "0000000000000000000000000000000000000000000000007FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
               V => "800000000000000000000000000000000000000000000000",
               Q => "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
               R => "7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"));

      U : Very_Long_384;
      V : Very_Long_192;
      Q : Very_Long_384;
      R : Very_Long_192;
      Expected_Q : Very_Long_384;
      Expected_R : Very_Long_192;
   begin
      for I in Test_Index loop
         U := Make(Test_Cases(I).U);
         V := Make(Test_Cases(I).V);
         Expected_Q := Make(Test_Cases(I).Q);
         Expected_R := Make(Test_Cases(I).R);
         Divide(U, V, Q, R);
         Assert(Q = Expected_Q, "Bad division: incorrect quotient in case #" & Integer'Image(I));
         Assert(R = Expected_R, "Bad division: incorrect remainder in case #" & Integer'Image(I));
      end loop;
   end Test_Division;


   procedure Test_Addition_Fp is
      type Test_Case is
         record
            X : Hex_String_192;
            Y : Hex_String_192;
            Z : Hex_String_192;  -- Z = X + Y
         end record;

      subtype Test_Index is Integer range 1 .. 2;
      Test_Cases : array(Test_Index) of Test_Case :=
        (1 => (X => "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFE",   -- P - 1
               Y => "000000000000000000000000000000000000000000000001",
               Z => "000000000000000000000000000000000000000000000000"),

         2 => (X => "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFD",   -- P - 2
               Y => "000000000000000000000000000000000000000000000001",
               Z => "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFE")); -- P - 1

      P : Very_Long_192;
      X, Y, Sum : Very_Long_192;
      Expected : Very_Long_192;
   begin
      -- The prime used for the elliptic curve secp192r1
      P := Make("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF");

      for I in Test_Index loop
         X := Make(Test_Cases(I).X);
         Y := Make(Test_Cases(I).Y);
         Expected := Make(Test_Cases(I).Z);
         Sum := Add_Fp(X, Y, P);
         Assert(Sum = Expected, "Bad addition (Fp) in case #" & Integer'Image(I));
      end loop;
   end Test_Addition_Fp;


   procedure Test_Subtraction_Fp is
      type Test_Case is
         record
            X : Hex_String_192;
            Y : Hex_String_192;
            Z : Hex_String_192;  -- Z = X - Y
         end record;

      subtype Test_Index is Integer range 1 .. 2;
      Test_Cases : array(Test_Index) of Test_Case :=
        (1 => (X => "000000000000000000000000000000000000000000000000",
               Y => "000000000000000000000000000000000000000000000001",
               Z => "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFE"),  -- P - 1

         2 => (X => "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFE",   -- P - 1
               Y => "000000000000000000000000000000000000000000000001",
               Z => "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFD")); -- P - 2

      P : Very_Long_192;
      X, Y, Sum : Very_Long_192;
      Expected : Very_Long_192;
   begin
      -- The prime used for the elliptic curve secp192r1
      P := Make("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF");

      for I in Test_Index loop
         X := Make(Test_Cases(I).X);
         Y := Make(Test_Cases(I).Y);
         Expected := Make(Test_Cases(I).Z);
         Sum := Subtract_Fp(X, Y, P);
         Assert(Sum = Expected, "Bad subtraction (Fp) in case #" & Integer'Image(I));
      end loop;
   end Test_Subtraction_Fp;


   procedure Test_Bits is
      Number : Very_Long_192;
   begin
      Number := Make(Number => 0, Length => 24);

      -- Try setting the least significant bit.
      Put_Bit(Number, 0, 1);
      Assert(Get_Bit(Number, 0) = 1, "Unexpected bit 0 in Very_Long");

      -- Try something a little larger.
      Put_Bit(Number, 32, 1);
      Assert(Get_Bit(Number, 32) = 1, "Unexpected bit 32 in Very_Long");

      -- Create a 65 bit number (three long digits; one bit more than Long_Digit can hold.
      Put_Bit(Number, 64, 1);
      Assert(Get_Bit(Number, 64) = 1, "Unexpected bit 64 in Very_Long");

      -- Verify that I can get bits that are off the end of the number.
      Assert(Get_Bit(Number, 65) = 0, "Uninitialized bit is 1");

      -- Erase bits one at a time and verify.
      Put_Bit(Number, 64, 0);
      Put_Bit(Number, 32, 0);
      Put_Bit(Number, 0, 0);
      Assert(Number = Make(0, 24), "Unable to set bits to zero in Very_Long");
   end Test_Bits;


   procedure Execute is
   begin
      Ada.Text_IO.Put_Line("... Natural constructor");   Test_Natural_Constructor;
      Ada.Text_IO.Put_Line("... String constructor");    Test_String_Constructor;
      Ada.Text_IO.Put_Line("... Relational operators");  Test_Relationals;
      Ada.Text_IO.Put_Line("... Addition");              Test_Addition;
      Ada.Text_IO.Put_Line("... Subtraction");           Test_Subtraction;
      Ada.Text_IO.Put_Line("... Multiplication");        Test_Multiplication;
      Ada.Text_IO.Put_Line("... Multiplication (Full)"); Test_Multiplication_Full;
      Ada.Text_IO.Put_Line("... Division");              Test_Division;
      Ada.Text_IO.Put_Line("... Addition (Fp)");         Test_Addition_Fp;
      Ada.Text_IO.Put_Line("... Subtraction (Fp)");      Test_Subtraction_Fp;
      Ada.Text_IO.Put_Line("... Bit access");            Test_Bits;
   end Execute;

end ACO.Math.Very_Longs.Test;
