---------------------------------------------------------------------------
-- FILE    : aco-crypto-block_cipher.adb
-- SUBJECT : Implementation of block cipher abstract types.
-- AUTHOR  : (C) Copyright 2008 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with ACO.Octet_Operations;
with ACO.Crypto.Exceptions;

package body ACO.Crypto.Block_Cipher is

   ---------------
   -- Block_Cipher
   ---------------

   procedure Encrypt(B : in out Block_Cipher; Block : in out Octet_Array) is
      Temporary_Block : Octet_Array(Block'Range);
   begin
      Encrypt(Block_Cipher'Class(B), Block, Temporary_Block);
      Block := Temporary_Block;
   end Encrypt;


   procedure Decrypt(B : in out Block_Cipher; Block : in out Octet_Array) is
      Temporary_Block : Octet_Array(Block'Range);
   begin
      Decrypt(Block_Cipher'Class(B), Block, Temporary_Block);
      Block := Temporary_Block;
   end Decrypt;


   procedure Encrypt
     (B            : in out Block_Cipher;
      Input_Block  : in     Octet_Array;
      Output_Block : out    Octet_Array) is
   begin
      Output_Block := Input_Block;
      Encrypt(Block_Cipher'Class(B), Output_Block);
   end Encrypt;


   procedure Decrypt
     (B            : in out Block_Cipher;
      Input_Block  : in     Octet_Array;
      Output_Block : out    Octet_Array) is
   begin
      Output_Block := Input_Block;
      Decrypt(Block_Cipher'Class(B), Output_Block);
   end Decrypt;

   -----------
   -- CBC_Mode
   -----------

   procedure Make
     (B          : out CBC_Mode;
      Underlying : in  Block_Cipher_Access;
      IV         : in  Octet_Array) is
   begin
      -- Make sure the IV is the right size.
      B.Size := Underlying.Block_Size;
      if IV'Length * Octet'Size /= B.Size then
         raise ACO.Crypto.Exceptions.Bad_Block_Size;
      end if;

      -- Allocate temporary arrays.
      B.Initial := new Octet_Array(0 .. B.Size/Octet'Size - 1);
      B.Saved   := new Octet_Array(0 .. B.Size/Octet'Size - 1);
      B.Engine  := Underlying;
      B.Initial.all := IV;
   end Make;


   procedure Finalize(B : in out CBC_Mode) is
   begin
      Free_Octet_Array(B.Initial);
      Free_Octet_Array(B.Saved);
   end Finalize;


   function Block_Size(B : CBC_Mode) return Natural is
   begin
      return B.Engine.Block_Size;
   end Block_Size;

   procedure Encrypt(B : in out CBC_Mode; Data : in out Octet_Array) is
      Success : Boolean := True;
   begin
      if Data'Length * Octet'Size /= B.Size then
         raise ACO.Crypto.Exceptions.Bad_Block_Size;
      end if;
      if B.Mode = Decrypt_Mode then
         raise ACO.Crypto.Exceptions.Bad_Cipher_Mode;
      end if;
      B.Mode := Encrypt_Mode;

      ACO.Octet_Operations.Xor_Array(Data, B.Initial.all, Success);
      if not Success then raise Constraint_Error; end if;
      B.Engine.Encrypt(Data);
      B.Initial.all := Data;
   end Encrypt;


   procedure Decrypt(B : in out CBC_Mode; Data : in out Octet_Array) is
      Success : Boolean := True;
   begin
      if Data'Length * Octet'Size /= B.Size then
         raise ACO.Crypto.Exceptions.Bad_Block_Size;
      end if;
      if B.Mode = Encrypt_Mode then
         raise ACO.Crypto.Exceptions.Bad_Cipher_Mode;
      end if;
      B.Mode := Decrypt_Mode;

      B.Saved.all := Data;
      B.Engine.Decrypt(Data);
      ACO.Octet_Operations.Xor_Array(Data, B.Initial.all, Success);
      if not Success then raise Constraint_Error; end if;
      B.Initial.all := B.Saved.all;
   end Decrypt;


end ACO.Crypto.Block_Cipher;
