---------------------------------------------------------------------------
-- FILE    : aco-crypto-block_cipher.ads
-- SUBJECT : General interface to block cipher abstract types.
-- AUTHOR  : (C) Copyright 2010 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Finalization;
with ACO;              use ACO;
with ACO.Access_Types; use ACO.Access_Types;

package ACO.Crypto.Block_Cipher is

   --------------------
   -- Type Block_Cipher
   --------------------

   type Block_Cipher is abstract new Ada.Finalization.Controlled with private;
   type Block_Cipher_Access is access all Block_Cipher'Class;

   function Block_Size(B : Block_Cipher) return Natural is abstract;

   -- In-place form.
   procedure Encrypt(B : in out Block_Cipher; Block : in out Octet_Array);
   procedure Decrypt(B : in out Block_Cipher; Block : in out Octet_Array);

   -- Copying form.
   procedure Encrypt
     (B            : in out Block_Cipher;
      Input_Block  : in     Octet_Array;
      Output_Block : out    Octet_Array);
   procedure Decrypt
     (B            : in out Block_Cipher;
      Input_Block  : in     Octet_Array;
      Output_Block : out    Octet_Array);

   ----------------
   -- Type CBC_Mode
   ----------------

   type CBC_Mode is new Block_Cipher with private;

   not overriding
   procedure Make
     (B          : out CBC_Mode;
      Underlying : in  Block_Cipher_Access;
      IV         : in  Octet_Array);

   overriding
   procedure Finalize(B : in out CBC_Mode);

   overriding
   function Block_Size(B : CBC_Mode) return Natural;

   overriding
   procedure Encrypt(B : in out CBC_Mode; Data : in out Octet_Array);

   overriding
   procedure Decrypt(B : in out CBC_Mode; Data : in out Octet_Array);

private
   type Block_Cipher is abstract new Ada.Finalization.Controlled with
      record
         Mode : Cipher_Mode := Neither_Mode;
      end record;

   type CBC_Mode is new Block_Cipher with
      record
         Size    : Natural;
         Engine  : Block_Cipher_Access;
         Initial : Octet_Array_Access;
         Saved   : Octet_Array_Access;
      end record;

end ACO.Crypto.Block_Cipher;
