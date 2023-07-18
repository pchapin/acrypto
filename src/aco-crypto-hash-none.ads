---------------------------------------------------------------------------
-- FILE    : aco-crypto-hash-none.ads
-- SUBJECT : Specification of a null hash function.
-- AUTHOR  : (C) Copyright 2008 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------

package ACO.Crypto.Hash.None is

   type Null_Hasher is new ACO.Crypto.Hash.Hasher with private;

   -- Puts hasher object in a state where it can accept data.
   overriding
   procedure Start_Hashing(Hash_Object : in out Null_Hasher) is null;

   -- Finalize the hash computation.
   overriding
   procedure Finish_Hashing(Hash_Object : in out Null_Hasher) is null;

   -- Return the resulting hash value.
   overriding
   function Retrieve_Hash(Hash_Object : Null_Hasher) return Hash_Value;

   -- Compute hash of data provided in blocks.
   overriding
   procedure Compute_Hash(Hash_Object : in out Null_Hasher;
                          Data_Block  : in Octet_Array) is null;

private

   type Null_Hasher is new ACO.Crypto.Hash.Hasher with null record;

end ACO.Crypto.Hash.None;
