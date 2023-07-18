---------------------------------------------------------------------------
-- FILE    : benchmark_very_long.adb
-- SUBJECT : A benchmark procedure for Very_Long integers.
-- AUTHOR  : (C) Copyright 2015 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Calendar;
with Ada.Float_Text_IO;
with Ada.Text_IO;
with ACO.Math.Very_Longs; use ACO.Math.Very_Longs;

procedure Benchmark_Very_Long is
   use type Ada.Calendar.Time;

   subtype Very_Long_192 is Very_Long(24);

   Start_Time : Ada.Calendar.Time;
   Stop_Time : Ada.Calendar.Time;

   -- TODO: Many of the procedures below are very similar. Can they be made into a single
   -- parameterized procedure?

   procedure Benchmark_Addition is
      Addition_Count : constant := 100_000_000;

      X : Very_Long_192;
      Y : Very_Long_192;
      Z : Very_Long_192;
      Rate : Float;
   begin
      -- Make sure there is a carry at each stage.
      X := Make("A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0");
      Y := X;
      Start_Time := Ada.Calendar.Clock;
      for I in 1 .. Addition_Count loop
         Z := X + Y;
      end loop;
      Stop_Time := Ada.Calendar.Clock;
      Rate := Float(Addition_Count) / Float(Stop_Time - Start_Time);
      Ada.Text_IO.Put("Addition           : ");
      Ada.Float_Text_IO.Put(Rate);
      Ada.Text_IO.Put_Line(" operations/s");
   end Benchmark_Addition;

   procedure Benchmark_Subtraction is
      Subtraction_Count : constant := 100_000_000;

      X : Very_Long_192;
      Y : Very_Long_192;
      Z : Very_Long_192;
      Rate : Float;
   begin
      -- Make sure there is a borrow at each stage.
      X := Make("010101010101010101010101010101010101010101010100");
      Y := Make("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
      Start_Time := Ada.Calendar.Clock;
      for I in 1 .. Subtraction_Count loop
         Z := X - Y;
      end loop;
      Stop_Time := Ada.Calendar.Clock;
      Rate := Float(Subtraction_Count) / Float(Stop_Time - Start_Time);
      Ada.Text_IO.Put("Subtraction        : ");
      Ada.Float_Text_IO.Put(Rate);
      Ada.Text_IO.Put_Line(" operations/s");
   end Benchmark_Subtraction;

   procedure Benchmark_Multiplication is
      Multiplication_Count : constant := 10_000_000;

      X : Very_Long_192;
      Y : Very_Long_192;
      Z : Very_Long_192;
      Rate : Float;
   begin
      -- Make sure there is a carry at each stage.
      X := Make("ffffffffffffffffffffffffffffffffffffffffffffffff");
      Y := X;
      Start_Time := Ada.Calendar.Clock;
      for I in 1 .. Multiplication_Count loop
         Z := X * Y;
      end loop;
      Stop_Time := Ada.Calendar.Clock;
      Rate := Float(Multiplication_Count) / Float(Stop_Time - Start_Time);
      Ada.Text_IO.Put("Multiplication     : ");
      Ada.Float_Text_IO.Put(Rate);
      Ada.Text_IO.Put_Line(" operations/s");
   end Benchmark_Multiplication;

   -- The Benchmark_Full_Multiplication procedure differs from the ones above in that it uses
   -- true multiplication, and not modular multiplication.

   procedure Benchmark_Full_Multiplication is
      Multiplication_Count : constant := 10_000_000;

      X : Very_Long_192;
      Y : Very_Long_192;
      Z : Very_Long(Length => 48);
      Rate : Float;
   begin
      -- Make sure there is a carry at each stage.
      X := Make("ffffffffffffffffffffffffffffffffffffffffffffffff");
      Y := X;
      Start_Time := Ada.Calendar.Clock;
      for I in 1 .. Multiplication_Count loop
         Z := Multiply(X, Y);
      end loop;
      Stop_Time := Ada.Calendar.Clock;
      Rate := Float(Multiplication_Count) / Float(Stop_Time - Start_Time);
      Ada.Text_IO.Put("Full Multiplication: ");
      Ada.Float_Text_IO.Put(Rate);
      Ada.Text_IO.Put_Line(" operations/s");
   end Benchmark_Full_Multiplication;

   -- The Fp benchmarks differ from the first three in that they involve the prime number P.

   procedure Benchmark_Addition_Fp is
      Addition_Count : constant := 100_000_000;

      P : Very_Long_192;
      X : Very_Long_192;
      Y : Very_Long_192;
      Z : Very_Long_192;
      Rate : Float;
   begin
      -- The prime used with the elliptic curve secp192r1
      P := Make("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF");

      -- X = P - 1.
      X := Make("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFE");
      Y := Make("000000000000000000000000000000000000000000000001");
      Start_Time := Ada.Calendar.Clock;
      for I in 1 .. Addition_Count loop
         Z := Add_Fp(X, Y, P);
      end loop;
      Stop_Time := Ada.Calendar.Clock;
      Rate := Float(Addition_Count) / Float(Stop_Time - Start_Time);
      Ada.Text_IO.Put("Addition (Fp)      : ");
      Ada.Float_Text_IO.Put(Rate);
      Ada.Text_IO.Put_Line(" operations/s");
   end Benchmark_Addition_Fp;

   procedure Benchmark_Subtraction_Fp is
      Subtraction_Count : constant := 100_000_000;

      P : Very_Long_192;
      X : Very_Long_192;
      Y : Very_Long_192;
      Z : Very_Long_192;
      Rate : Float;
   begin
      -- The prime used with the elliptic curve secp192r1
      P := Make("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF");

      -- X = P - 1.
      X := Make("000000000000000000000000000000000000000000000000");
      Y := Make("000000000000000000000000000000000000000000000001");
      Start_Time := Ada.Calendar.Clock;
      for I in 1 .. Subtraction_Count loop
         Z := Subtract_Fp(X, Y, P);
      end loop;
      Stop_Time := Ada.Calendar.Clock;
      Rate := Float(Subtraction_Count) / Float(Stop_Time - Start_Time);
      Ada.Text_IO.Put("Subtraction (Fp)   : ");
      Ada.Float_Text_IO.Put(Rate);
      Ada.Text_IO.Put_Line(" operations/s");
   end Benchmark_Subtraction_Fp;

begin
   Benchmark_Addition;
   Benchmark_Subtraction;
   Benchmark_Multiplication;
   Benchmark_Full_Multiplication;
   Benchmark_Addition_Fp;
   Benchmark_Subtraction_Fp;
end Benchmark_Very_Long;
