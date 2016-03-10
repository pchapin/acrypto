---------------------------------------------------------------------------
-- FILE    : aco-crypto-hash-none.adb
-- SUBJECT : Implementation of a null hash function.
-- AUTHOR  : (C) Copyright 2008 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package body ACO.Crypto.Hash.None is

   function Retrieve_Hash(Hash_Object : Null_Hasher) return Hash_Value is
      Result : Hash_Value(1);
   begin
      Result.Hash_Buffer(1) := 0;
      return Result;
   end Retrieve_Hash;

end ACO.Crypto.Hash.None;
