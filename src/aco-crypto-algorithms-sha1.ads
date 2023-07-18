---------------------------------------------------------------------------
-- FILE    : aco-crypto-algorithms-sha1.ads
-- SUBJECT : Specification of a package holding the raw SHA1 hash algorithm.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package ACO.Crypto.Algorithms.SHA1 is
   type State is private;

   -- These types define the input blocks.
   type Block_Index is range 0 .. 15;
   type Message_Block is array(Block_Index) of ACO.Quadruple_Octet;
   subtype Message_Bit_Count is Natural range 0 .. 512;

   -- These types define the hash result.
   type Hash_Index is range 0 .. 4;
   type Hash_Array is array(Hash_Index) of ACO.Quadruple_Octet;

   procedure Prepare(S : out State)
     with
       Depends => (S => null);

   procedure Update_Hash(S : in out State; M : in out Message_Block; Number_Of_Bits : in Message_Bit_Count)
     with
       Depends => ((S, M) => (S, M, Number_Of_Bits));

   procedure Partake(S : in out State; Result : out Hash_Array)
     with
       Depends => ((S, Result) => S);

private

   type State is
      record
         Total_Bit_Count : ACO.Octuple_Octet;  -- Total number of bits hashed so far.
         H      : Hash_Array;  -- Current estimate of hash value.
         Active : Boolean;     -- True when this object can accept more data.
      end record;

end ACO.Crypto.Algorithms.SHA1;

