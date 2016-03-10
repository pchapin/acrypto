---------------------------------------------------------------------------
-- FILE    : aco-quadruple_octet_operations.adb
-- SUBJECT : Intrinsic and related operations for ACO.Quadruple_Octet
-- AUTHOR  : (C) Copyright 2014 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body ACO.Quadruple_Octet_Operations is

   procedure Xor_Array
     (Accumulator  : in out ACO.Quadruple_Octet_Array;
      Incoming     : in     ACO.Quadruple_Octet_Array;
      Success_Flag : out    Boolean) is

      Incoming_Index : Natural;
   begin
      if Accumulator'Length /= Incoming'Length then
         Success_Flag := False;
      elsif Incoming'Length = 0 then
         Success_Flag := True;
      else
         Incoming_Index := Incoming'First;
         for Accumulator_Index in Accumulator'Range loop
            pragma Loop_Invariant(Incoming_Index in Incoming'Range);

            Accumulator(Accumulator_Index) := Accumulator(Accumulator_Index) xor Incoming(Incoming_Index);
            if Incoming_Index < Incoming'Last then
               Incoming_Index := Incoming_Index + 1;
            end if;
         end loop;
         Success_Flag := True;
      end if;
   end Xor_Array;

end ACO.Quadruple_Octet_Operations;
