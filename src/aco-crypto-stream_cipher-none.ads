---------------------------------------------------------------------------
-- FILE    : aco-crypto-stream_cipher-none.ads
-- SUBJECT : General interface to stream cipher abstract types.
-- AUTHOR  : (C) Copyright 2008 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package ACO.Crypto.Stream_Cipher.None is

   type Null_Cipher is new Stream_Cipher with private;

   overriding
   procedure Encrypt(S : in out Null_Cipher; Data : in out Octet);

   overriding
   procedure Decrypt(S : in out Null_Cipher; Data : in out Octet);

private

   type Null_Cipher is new Stream_Cipher with null record;

end ACO.Crypto.Stream_Cipher.None;
