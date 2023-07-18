---------------------------------------------------------------------------
-- FILE    : aco-math-very_longs.ads
-- SUBJECT : Specification of an extended precision integer package.
-- AUTHOR  : (C) Copyright 2015 by Peter Chapin
--
-- The integers provided here are unsigned modular integers. The operations are of a sort used
-- by cryptographic applications. This package is not intended to model mathematical integers.
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package ACO.Math.Very_Longs is
   
   -- Here "digit" means a base 256 extended digit.
   Digit_Bits : constant := 8;
   Maximum_Length : constant := 2**16;
   type    Digit_Count_Type is new Natural range 0 .. Maximum_Length;
   subtype Digit_Index_Type is Digit_Count_Type range 1 .. Maximum_Length;
   type    Very_Long(Length : Digit_Index_Type) is private;
   subtype Bit is Natural range 0 .. 1;
   type    Bit_Index_Type is new Natural range 0 .. 8*Maximum_Length - 1;

   -- Constructors.
   function Make(Number : Natural; Length : Digit_Index_Type) return Very_Long
     with Post => Make'Result.Length = Length;
   
   subtype Hex_Digit is Character
     with Static_Predicate => Hex_Digit in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F';
   
   subtype Hex_String is String
     with Dynamic_Predicate =>
       Hex_String'Length > 0       and
       Hex_String'Length rem 2 = 0 and
      (for all I in Hex_String'Range => Hex_String(I) in Hex_Digit);

   -- Since SPARK does not yet support Dynamic_Predicate, it is necessary to reassert the
   -- constraints on Hex_String as a precondition below. Otherwise there is little chance
   -- the proofs inside Make will go through.
   --
   function Make(Number : Hex_String) return Very_Long
     with
       Pre =>
         Number'Length > 0       and
         Number'Length rem 2 = 0 and
        (for all I in Number'Range => Number(I) in Hex_Digit),
       Post => Make'Result.Length = Number'Length/2;

   -- Relational operators. Only Very_Longs that are the same size can be compared.
   function "<" (L, R : Very_Long) return Boolean
     with Pre => L.Length = R.Length;

   function "<="(L, R : Very_Long) return Boolean
     with Pre => L.Length = R.Length;

   function ">" (L, R : Very_Long) return Boolean
     with Pre => L.Length = R.Length;

   function ">="(L, R : Very_Long) return Boolean
     with Pre => L.Length = R.Length;

   -- Returns True if Number is zero.
   function Is_Zero(Number : Very_Long) return Boolean;

   -- Returns the number of significant digits in Number.
   function Number_Of_Digits(Number : Very_Long) return Digit_Count_Type;

   -- Modular addition.
   function "+"(L, R : Very_Long) return Very_Long
     with
       Pre  => L.Length = R.Length,
       Post => "+"'Result.Length = L.Length;

   -- Modular subtraction.
   function "-"(L, R : Very_Long) return Very_Long
     with
       Pre  => L.Length = R.Length,
       Post => "-"'Result.Length = L.Length;

   -- Modular multiplication.
   function "*"(L, R : Very_Long) return Very_Long
     with
       Pre  => L.Length = R.Length,
       Post => "*"'Result.Length = L.Length;

   -- Ordinary (full) multiplication.
   function Multiply(L, R : Very_Long) return Very_Long
     with Post => Multiply'Result.Length = L.Length + R.Length;

   -- Ordinary division.
   procedure Divide
     (Dividend  : in  Very_Long;
      Divisor   : in  Very_Long;
      Quotient  : out Very_Long;
      Remainder : out Very_Long)
     with
       Depends => (Quotient =>+ (Dividend, Divisor), Remainder =>+ (Dividend, Divisor)),
       Pre => (Number_Of_Digits(Divisor) > 1)      and
              (Divisor.Length  = Remainder.Length) and
              (Dividend.Length = Quotient.Length ) and
              (Dividend.Length = 2*Divisor.Length);

   -- Field F_p arithmetic. Operations are mod p where p is prime.
   function Add_Fp(L, R : Very_Long; Prime : Very_Long) return Very_Long
     with
       Pre => (L.Length = R.Length and L.Length = Prime.Length) and then
              (L < Prime and R < Prime),
       Post => Add_Fp'Result.Length = L.Length and then Add_Fp'Result < Prime;

   function Subtract_Fp(L, R : Very_Long; Prime : Very_Long) return Very_Long
     with
       Pre => (L.Length = R.Length and L.Length = Prime.Length) and then
              (L < Prime and R < Prime),
       Post => Subtract_Fp'Result.Length = L.Length and then Subtract_Fp'Result < Prime;

   -- Bit access.
   function Get_Bit(Number : in Very_Long;  Bit_Number : in Bit_Index_Type) return Bit
     with Post =>
       (if Digit_Index_Type(Bit_Number/Digit_Bits + 1) > Number.Length then Get_Bit'Result = 0);
   
   procedure Put_Bit
     (Number : in out Very_Long; Bit_Number : in Bit_Index_Type; Bit_Value : in Bit)
     with
       Pre => Digit_Index_Type(Bit_Number/Digit_Bits + 1) <= Number.Length,
       Depends => (Number =>+ (Bit_Number, Bit_Value));

private
   type Digits_Array_Type is array(Digit_Index_Type range <>) of ACO.Octet;

   -- The bytes are stored in little endian order (the LSB is at index position zero).
   type Very_Long(Length : Digit_Index_Type) is
      record
         Long_Digits : Digits_Array_Type(1 .. Length);
      end record;

   function Is_Zero(Number : Very_Long) return Boolean is
     (for all I in Number.Long_Digits'Range => Number.Long_Digits(I) = 0);

end ACO.Math.Very_Longs;
