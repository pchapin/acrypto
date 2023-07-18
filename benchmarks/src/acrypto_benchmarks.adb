---------------------------------------------------------------------------
-- FILE    : acrypto_benchmarks.adb
-- SUBJECT : The main program for the ACO benchmark tests.
-- AUTHOR  : (C) Copyright 2012 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Text_IO;
with Benchmark_Blowfish;
with Benchmark_Very_Long;

procedure Acrypto_Benchmarks is
begin
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("VERY_LONG benchmarks");
   Ada.Text_IO.Put_Line("====================");
   Benchmark_Very_Long;

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("BLOWFISH benchmarks");
   Ada.Text_IO.Put_Line("===================");
   Benchmark_Blowfish;
end Acrypto_Benchmarks;

