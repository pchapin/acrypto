---------------------------------------------------------------------------
-- FILE    : aco-crypto-block_cipher-none.adb
-- SUBJECT : Implementation of a Null_Cipher block cipher type.
-- AUTHOR  : (C) Copyright 2008 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
with ACO.Crypto.Exceptions;

package body ACO.Crypto.Block_Cipher.None is

   --------------
   -- Null_Cipher
   --------------

   procedure Make(B : out Null_Cipher; Block_Size : in Natural) is
   begin
      B.Size := Block_Size;
   end Make;


   function Block_Size(B : Null_Cipher) return Natural is
   begin
      return B.Size;
   end Block_Size;


   procedure Encrypt(B : in out Null_Cipher; Block : in out Octet_Array) is
   begin
      if B.Mode = Decrypt_Mode then
         raise ACO.Crypto.Exceptions.Bad_Cipher_Mode;
      end if;
      B.Mode := Encrypt_Mode;
      if Block'Length * Octet'Size /= B.Size then
         raise ACO.Crypto.Exceptions.Bad_Block_Size;
      end if;
   end Encrypt;


   procedure Decrypt(B : in out Null_Cipher; Block : in out Octet_Array) is
   begin
      if B.Mode = Encrypt_Mode then
         raise ACO.Crypto.Exceptions.Bad_Cipher_Mode;
      end if;
      B.Mode := Decrypt_Mode;
      if Block'Length * Octet'Size /= B.Size then
         raise ACO.Crypto.Exceptions.Bad_Block_Size;
      end if;
   end Decrypt;

end ACO.Crypto.Block_Cipher.None;
