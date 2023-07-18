---------------------------------------------------------------------------
-- FILE    : aco-crypto-block_cipher-aes.ads
-- SUBJECT : Interface to an AES block cipher type.
-- AUTHOR  : (C) Copyright 2013 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
private with ACO.Crypto.Algorithms.AES;

package ACO.Crypto.Block_Cipher.AES is
   type AES_Cipher is new Block_Cipher with private;

   not overriding procedure Make(B : out AES_Cipher; Key : in Octet_Array);

   overriding function Block_Size(B : AES_Cipher) return Natural;
   overriding procedure Encrypt(B : in out AES_Cipher; Block : in out Octet_Array);
   overriding procedure Decrypt(B : in out AES_Cipher; Block : in out Octet_Array);

private
   type AES_Cipher is new Block_Cipher with record
      Processor : ACO.Crypto.Algorithms.AES.AES_Algorithm;
   end record;

end ACO.Crypto.Block_Cipher.AES;
