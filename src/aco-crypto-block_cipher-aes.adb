---------------------------------------------------------------------------
-- FILE    : aco-crypto-block_cipher-aes.adb
-- SUBJECT : Implementation of an AES block cipher type.
-- AUTHOR  : (C) Copyright 2013 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
with ACO.Crypto.Exceptions;

package body ACO.Crypto.Block_Cipher.AES is

   procedure Make(B : out AES_Cipher; Key : in Octet_Array) is
   begin
      if Key'Length /= 16 then
         raise ACO.Crypto.Exceptions.Bad_Key_Length;
      end if;
      ACO.Crypto.Algorithms.AES.Initialize(B.Processor, Key);
   end Make;


   function Block_Size(B : AES_Cipher) return Natural is
   begin
      return 128;
   end Block_Size;


   procedure Encrypt(B : in out AES_Cipher; Block : in out Octet_Array) is
   begin
      if Block'Length /= 16 then
         raise ACO.Crypto.Exceptions.Bad_Block_Size;
      end if;
      ACO.Crypto.Algorithms.AES.Encrypt(B.Processor, Block);
   end Encrypt;


   procedure Decrypt(B : in out AES_Cipher; Block : in out Octet_Array) is
   begin
      if Block'Length /= 16 then
         raise ACO.Crypto.Exceptions.Bad_Block_Size;
      end if;
      ACO.Crypto.Algorithms.AES.Decrypt(B.Processor, Block);
   end Decrypt;


end ACO.Crypto.Block_Cipher.AES;
