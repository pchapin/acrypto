---------------------------------------------------------------------------
-- FILE    : aco-octuple_octet_operations.ads
-- SUBJECT : Intrinsic and related operations for ACO.Octuple_Octet
-- AUTHOR  : (C) Copyright 2014 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package ACO.Octuple_Octet_Operations is

   function Shift_Left(Value : ACO.Octuple_Octet; Count : Natural) return ACO.Octuple_Octet
     with
       Import,
       Convention => Intrinsic,
       Global     => null;

   function Shift_Right(Value : ACO.Octuple_Octet; Count : Natural) return ACO.Octuple_Octet
     with
       Import,
       Convention => Intrinsic,
       Global     => null,
       Post       => Shift_Right'Result = Value / (2**Count);

   function Rotate_Left(Value : ACO.Octuple_Octet; Count : Natural) return ACO.Octuple_Octet
     with
       Import,
       Convention => Intrinsic,
       Global     => null;

   function Rotate_Right(Value : ACO.Octuple_Octet; Count : Natural) return ACO.Octuple_Octet
     with
       Import,
       Convention => Intrinsic,
       Global     => null;

   procedure Xor_Array
     (Accumulator  : in out ACO.Octuple_Octet_Array;
      Incoming     : in     ACO.Octuple_Octet_Array;
      Success_Flag : out    Boolean)
     with
       Depends => ((Accumulator, Success_Flag) => (Accumulator, Incoming));

end ACO.Octuple_Octet_Operations;
