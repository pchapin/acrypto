---------------------------------------------------------------------------
-- FILE    : aco-crypto-stream_cipher.ads
-- SUBJECT : General interface to stream cipher abstract types.
-- AUTHOR  : (C) Copyright 2008 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
with Ada.Finalization;
with ACO;                     use ACO;
with ACO.Access_Types;        use ACO.Access_Types;
with ACO.Crypto.Block_Cipher; use ACO.Crypto.Block_Cipher;

package ACO.Crypto.Stream_Cipher is

   ---------------------
   -- Type Stream_Cipher
   ---------------------

   type Stream_Cipher is abstract new Ada.Finalization.Controlled with private;
   type Stream_Cipher_Access is access all Stream_Cipher'Class;

   -- In-place form.
   procedure Encrypt(S : in out Stream_Cipher; Data : in out Octet);
   procedure Decrypt(S : in out Stream_Cipher; Data : in out Octet);

   -- Copying form.
   procedure Encrypt(S : in out Stream_Cipher; Input_Data : in Octet; Output_Data : out Octet);
   procedure Decrypt(S : in out Stream_Cipher; Input_Data : in Octet; Output_Data : out Octet);

   ----------------
   -- Type CFB_Mode
   ----------------

   type CFB_Mode is new Stream_Cipher with private;

   not overriding
   procedure Make
     (S          : out CFB_Mode;
      Underlying : in  Block_Cipher_Access;
      IV         : in  Octet_Array);

   overriding
   procedure Finalize(S : in out CFB_Mode);

   overriding
   procedure Encrypt(S : in out CFB_Mode; Data : in out Octet);

   overriding
   procedure Decrypt(S : in out CFB_Mode; Data : in out Octet);

private
   type Stream_Cipher is abstract new Ada.Finalization.Controlled with
      record
         Mode : Cipher_Mode := Neither_Mode;
      end record;

   type CFB_Mode is new Stream_Cipher with
      record
         Size    : Natural;
         Engine  : Block_Cipher_Access;
         Initial : Octet_Array_Access;
      end record;

end ACO.Crypto.Stream_Cipher;
