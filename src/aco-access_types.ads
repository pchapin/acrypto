---------------------------------------------------------------------------
-- FILE    : aco-access_types.ads
-- SUBJECT : Package for basic access types and their utilities.
-- AUTHOR  : (C) Copyright 2008 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Unchecked_Deallocation;

package ACO.Access_Types is

   type Octet_Array_Access is access Octet_Array;
   procedure Free_Octet_Array is new
     Ada.Unchecked_Deallocation(Octet_Array, Octet_Array_Access);

   type Double_Octet_Array_Access is access Double_Octet_Array;
   procedure Free_Double_Octet_Array is new
     Ada.Unchecked_Deallocation(Double_Octet_Array, Double_Octet_Array_Access);

   type Quadruple_Octet_Array_Access is access Quadruple_Octet_Array;
   procedure Free_Quadruple_Octet_Array is new
     Ada.Unchecked_Deallocation(Quadruple_Octet_Array, Quadruple_Octet_Array_Access);

   type Octuple_Octet_Array_Access is access Octuple_Octet_Array;
   procedure Free_Octuple_Octet_Array is new
     Ada.Unchecked_Deallocation(Octuple_Octet_Array, Octuple_Octet_Array_Access);

end ACO.Access_Types;
