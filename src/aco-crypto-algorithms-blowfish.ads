---------------------------------------------------------------------------
-- FILE    : aco-crypto-algorithms-blowfish.ads
-- SUBJECT : Specification of a package holding the raw Blowfish algorithm.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package ACO.Crypto.Algorithms.Blowfish is
   type Blowfish_Algorithm is private;

   -- Blocks must be exactly 64 bits.
   subtype Block_Index_Type is Natural range 0 .. 7;
   subtype Block_Type is ACO.Octet_Array(Block_Index_Type);

   procedure Initialize
     (B       : out Blowfish_Algorithm;
      Key     : in  ACO.Octet_Array;
      Success : out Boolean)
     with
       Depends => ((B, Success) => Key);

   procedure Encrypt
     (B       : in     Blowfish_Algorithm;
      Block   : in out Block_Type;
      Success : in out Boolean)
     with
       Depends => ((Block, Success) =>+ B);

   procedure Decrypt
     (B       : in     Blowfish_Algorithm;
      Block   : in out Block_Type;
      Success : in out Boolean)
     with
       Depends => ((Block, Success) =>+ B);

private
   type PBox_Index_Type   is range 0 .. 17;
   type SBox_Index_Type   is mod 2**8;
   type SBoxes_Index_Type is range 0 .. 3;

   type PBox   is array(PBox_Index_Type)   of ACO.Quadruple_Octet;
   type SBox   is array(SBox_Index_Type)   of ACO.Quadruple_Octet;
   type Sboxes is array(SBoxes_Index_Type) of SBox;

   type Blowfish_Algorithm is
      record
         P : PBox;
         S : SBoxes;
         Is_Initialized : Boolean;
      end record;

end ACO.Crypto.Algorithms.Blowfish;
