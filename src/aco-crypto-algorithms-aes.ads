---------------------------------------------------------------------------
-- FILE    : aco-crypto-algorithms-aes.ads
-- SUBJECT : Specification of a package holding the raw AES algorithm.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- This version only supports 128 bit keys.
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package ACO.Crypto.Algorithms.AES is
   type AES_Algorithm is private;
   
   procedure Initialize
     (B     : out AES_Algorithm;
      Key   : in  ACO.Octet_Array)
     with
       Depends => (B => Key),
       Pre => Key'Length = 16;

   procedure Encrypt
     (B     : in     AES_Algorithm;
      Block : in out ACO.Octet_Array)
     with
       Depends => (Block =>+ B),
       Pre => Block'Length = 16;

   procedure Decrypt
     (B     : in     AES_Algorithm;
      Block : in out ACO.Octet_Array)
     with
       Depends => (Block =>+ B),
       Pre => Block'Length = 16;

private

   Nb : constant :=  4;  -- Block size = 4*Nb = 16 bytes (128 bits).
   Nk : constant :=  4;  -- Key size   = 4*Nk = 16 bytes (128 bits).
   Nr : constant := 10;  -- Number of rounds = 10 for the 128 bit key.

   subtype Round_Index_Type is Natural range 0 .. Nr;

   subtype State_Row_Index_Type is Natural range 0 .. 3;
   type State_Column_Index_Type is mod Nb;   -- A modular type makes row shifting easier.
   type State_Type is array(State_Row_Index_Type, State_Column_Index_Type) of ACO.Octet;

   subtype Key_Row_Index_Type is Natural range 0 .. 3;
   subtype Key_Column_Index_Type is Natural range 0 .. Nb*(Nr + 1) - 1;
   type Expanded_Key_Type is array(Key_Row_Index_Type, Key_Column_Index_Type) of ACO.Octet;

   type AES_Algorithm is
      record
         W : Expanded_Key_Type;
      end record;

end ACO.Crypto.Algorithms.AES;
