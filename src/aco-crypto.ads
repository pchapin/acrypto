---------------------------------------------------------------------------
-- FILE    : aco-crypto.ads
-- SUBJECT : Top level specification of the Crypto child package.
-- AUTHOR  : (C) Copyright 2008 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------

package ACO.Crypto is

   -- Used by the cipher types to indicate their current mode.
   type Cipher_Mode is (Encrypt_Mode, Decrypt_Mode, Neither_Mode);

end ACO.Crypto;
