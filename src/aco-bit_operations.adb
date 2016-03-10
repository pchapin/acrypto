---------------------------------------------------------------------------
-- FILE    : aco-bit_operations.ads
-- SUBJECT : Bit manipulation operations that (currently) can't be proven by SPARK.
-- AUTHOR  : (C) Copyright 2014 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(Off);

with ACO.Octuple_Octet_Operations;
with ACO.Quadruple_Octet_Operations;

package body ACO.Bit_Operations is

   procedure Split32_To_Word8(Value : in ACO.Quadruple_Octet; Most, Middle_High, Middle_Low, Least : out ACO.Octet) is
   begin
      Most        := ACO.Octet(Quadruple_Octet_Operations.Shift_Right(Value and 16#FF000000#, 24));
      Middle_High := ACO.Octet(Quadruple_Octet_Operations.Shift_Right(Value and 16#00FF0000#, 16));
      Middle_Low  := ACO.Octet(Quadruple_Octet_Operations.Shift_Right(Value and 16#0000FF00#,  8));
      Least       := ACO.Octet(Value and 16#000000FF#);
   end Split32_To_Word8;


   procedure Split64_To_Word32(Value : in ACO.Octuple_Octet; Most, Least : out ACO.Quadruple_Octet) is
   begin
      Most  := ACO.Quadruple_Octet(Octuple_Octet_Operations.Shift_Right(Value, 32));
      Least := ACO.Quadruple_Octet(Value and 16#00000000FFFFFFFF#);
   end Split64_To_Word32;


end ACO.Bit_Operations;
