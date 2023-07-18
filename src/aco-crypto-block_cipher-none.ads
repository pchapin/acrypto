---------------------------------------------------------------------------
-- FILE    : aco-crypto-block_cipher-none.ads
-- SUBJECT : Interface to the Null_Cipher block cipher type.
-- AUTHOR  : (C) Copyright 2008 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package ACO.Crypto.Block_Cipher.None is

   type Null_Cipher is new Block_Cipher with private;

   not overriding
   procedure Make(B : out Null_Cipher; Block_Size : in Natural);

   overriding
   function Block_Size(B : Null_Cipher) return Natural;

   overriding
   procedure Encrypt(B : in out Null_Cipher; Block : in out Octet_Array);

   overriding
   procedure Decrypt(B : in out Null_Cipher; Block : in out Octet_Array);

private

   type Null_Cipher is new Block_Cipher with
      record
         Size : Natural;
      end record;

end ACO.Crypto.Block_Cipher.None;
