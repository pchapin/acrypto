---------------------------------------------------------------------------
-- FILE    : aco-crypto-block_cipher-blowfish.adb
-- SUBJECT : Implementation of a Blowfish block cipher type.
-- AUTHOR  : (C) Copyright 2008 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with ACO.Crypto.Exceptions;

package body ACO.Crypto.Block_Cipher.Blowfish is

   procedure Make(B : out Blowfish_Cipher; Key : in Octet_Array) is
      Dummy : Boolean;  -- Ignore the success status of the underlying algorithm (for now).
   begin
      if Key'Length mod 4 /= 0 then
         raise ACO.Crypto.Exceptions.Bad_Key_Length;
      end if;
      ACO.Crypto.Algorithms.Blowfish.Initialize(B.Processor, Key, Dummy);
   end Make;


   function Block_Size(B : Blowfish_Cipher) return Natural is
   begin
      return 64;
   end Block_Size;


   procedure Encrypt(B : in out Blowfish_Cipher; Block : in out Octet_Array) is
      Dummy : Boolean := True;
   begin
      if Block'Length /= 8 then
         raise ACO.Crypto.Exceptions.Bad_Block_Size;
      end if;

      ACO.Crypto.Algorithms.Blowfish.Encrypt(B.Processor, Block, Dummy);
   end Encrypt;


   procedure Decrypt(B : in out Blowfish_Cipher; Block : in out Octet_Array) is
      Dummy : Boolean := True;
   begin
      if Block'Length /= 8 then
         raise ACO.Crypto.Exceptions.Bad_Block_Size;
      end if;

      ACO.Crypto.Algorithms.Blowfish.Decrypt(B.Processor, Block, Dummy);
   end Decrypt;


end ACO.Crypto.Block_Cipher.Blowfish;
