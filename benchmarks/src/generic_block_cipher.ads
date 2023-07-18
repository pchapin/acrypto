---------------------------------------------------------------------------
-- FILE    : generic_block_cipher.ads
-- SUBJECT : A benchmark procedure for block ciphers.
-- AUTHOR  : (C) Copyright 2012 by Peter Chapin
--
-- This generic procedure can benchmark any core algorithm by being statically instantiated for each algorithm.
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
with ACO;

generic
   type Cipher_Type is limited private;
   with function Block_Size(B : Cipher_Type) return Natural;
   with procedure Encrypt(B : in out Cipher_Type; Block : in out ACO.Octet_Array);
   with procedure Decrypt(B : in out Cipher_Type; Block : in out ACO.Octet_Array);
procedure Generic_Block_Cipher(E_Cipher : in out Cipher_Type; D_Cipher : in out Cipher_Type);
