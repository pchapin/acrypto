---------------------------------------------------------------------------
-- FILE    : aco-crypto-stream_cipher-none.ads
-- SUBJECT : General interface to stream cipher abstract types.
-- AUTHOR  : (C) Copyright 2008 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
with ACO.Crypto.Exceptions;

package body ACO.Crypto.Stream_Cipher.None is

   procedure Encrypt(S : in out Null_Cipher; Data : in out Octet) is
   begin
      if S.Mode = Decrypt_Mode then
         raise ACO.Crypto.Exceptions.Bad_Cipher_Mode;
      end if;
      S.Mode := Encrypt_Mode;
   end Encrypt;

   procedure Decrypt(S : in out Null_Cipher; Data : in out Octet) is
   begin
      if S.Mode = Encrypt_Mode then
         raise ACO.Crypto.Exceptions.Bad_Cipher_Mode;
      end if;
      S.Mode := Decrypt_Mode;
   end Decrypt;

end ACO.Crypto.Stream_Cipher.None;
