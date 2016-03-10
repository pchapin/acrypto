---------------------------------------------------------------------------
-- FILE    : aco-crypto-block_cipher-blowfish.ads
-- SUBJECT : Interface to Blowfish block cipher type.
-- AUTHOR  : (C) Copyright 2010 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
private with ACO.Crypto.Algorithms.Blowfish;

package ACO.Crypto.Block_Cipher.Blowfish is
   type Blowfish_Cipher is new Block_Cipher with private;

   not overriding procedure Make(B : out Blowfish_Cipher; Key : in Octet_Array);

   overriding function Block_Size(B : Blowfish_Cipher) return Natural;
   overriding procedure Encrypt(B : in out Blowfish_Cipher; Block : in out Octet_Array);
   overriding procedure Decrypt(B : in out Blowfish_Cipher; Block : in out Octet_Array);

private
   type Blowfish_Cipher is new Block_Cipher with record
      Processor : ACO.Crypto.Algorithms.Blowfish.Blowfish_Algorithm;
   end record;

end ACO.Crypto.Block_Cipher.Blowfish;
