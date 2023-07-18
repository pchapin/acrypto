---------------------------------------------------------------------------
-- FILE    : aco-crypto-stream_cipher.adb
-- SUBJECT : Implementation of stream cipher abstract types.
-- AUTHOR  : (C) Copyright 2008 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
with ACO.Crypto.Exceptions;

package body ACO.Crypto.Stream_Cipher is

   ----------------
   -- Stream_Cipher
   ----------------

   -- In-place form.
   procedure Encrypt(S : in out Stream_Cipher; Data : in out Octet) is
      Temporary_Data : Octet;
   begin
      Encrypt(Stream_Cipher'Class(S), Data, Temporary_Data);
      Data := Temporary_Data;
   end Encrypt;

   procedure Decrypt(S : in out Stream_Cipher; Data : in out Octet) is
      Temporary_Data : Octet;
   begin
      Decrypt(Stream_Cipher'Class(S), Data, Temporary_Data);
      Data := Temporary_Data;
   end Decrypt;


   -- Copying form.
   procedure Encrypt(S : in out Stream_Cipher; Input_Data : in Octet; Output_Data : out Octet) is
   begin
      Output_Data := Input_Data;
      Encrypt(Stream_Cipher'Class(S), Output_Data);
   end Encrypt;

   procedure Decrypt(S : in out Stream_Cipher; Input_Data : in Octet; Output_Data : out Octet) is
   begin
      Output_Data := Input_Data;
      Decrypt(Stream_Cipher'Class(S), Output_Data);
   end Decrypt;


   -----------
   -- CFB_Mode
   -----------

   procedure Make
     (S          : out CFB_Mode;
      Underlying : in  Block_Cipher_Access;
      IV         : in  Octet_Array) is
   begin
      -- Make sure the IV is the right size.
      S.Size := Underlying.Block_Size;
      if IV'Length * Octet'Size /= S.Size then
         raise ACO.Crypto.Exceptions.Bad_Block_Size;
      end if;

      S.Initial := new Octet_Array(0 .. S.Size/Octet'Size - 1);
      S.Engine  := Underlying;
      S.Initial.all := IV;
   end Make;


   procedure Finalize(S : in out CFB_Mode) is
   begin
      Free_Octet_Array(S.Initial);
   end Finalize;


   procedure Encrypt(S : in out CFB_Mode; Data : in out Octet) is
      Temp : Octet_Array(0 .. S.Size/Octet'Size - 1) := S.Initial.all;
   begin
      if S.Mode = Decrypt_Mode then
         raise ACO.Crypto.Exceptions.Bad_Cipher_Mode;
      end if;
      S.Mode := Encrypt_Mode;

      S.Engine.Encrypt(Temp);
      Data := Temp(0) xor Data;

      for I in 0 .. S.Size / Octet'Size - 2 loop
         S.Initial(I) := S.Initial(I + 1);
      end loop;
      S.Initial(S.Initial'Last) := Data;
   end Encrypt;


   procedure Decrypt(S : in out CFB_Mode; Data : in out Octet) is
      Temp  : Octet_Array(0 .. S.Size/Octet'Size - 1) := S.Initial.all;
      Plain : Octet;
   begin
      if S.Mode = Encrypt_Mode then
         raise ACO.Crypto.Exceptions.Bad_Cipher_Mode;
      end if;
      S.Mode := Decrypt_Mode;

      S.Engine.Encrypt(Temp);
      Plain := Temp(0) xor Data;

      for I in 0 .. S.Size / Octet'Size - 2 loop
         S.Initial(I) := S.Initial(I + 1);
      end loop;
      S.Initial(S.Initial'Last) := Data;
      Data := Plain;
   end Decrypt;

end ACO.Crypto.Stream_Cipher;
