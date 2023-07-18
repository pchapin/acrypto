---------------------------------------------------------------------------
-- FILE    : aco-crypto-exceptions.ads
-- SUBJECT : Specification of a package holding crypto exceptions.
-- AUTHOR  : (C) Copyright 2010 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package ACO.Crypto.Exceptions is

   -- Raised by the cipher types if the wrong mode is used.
   Bad_Cipher_Mode : exception;

   -- Raised by the cipher types if a bad key size is used.
   Bad_Key_Length : exception;

   -- Raised by the block cipher types if the wrong block size is used.
   Bad_Block_Size : exception;

end ACO.Crypto.Exceptions;
