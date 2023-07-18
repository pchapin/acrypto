---------------------------------------------------------------------------
-- FILE    : benchmark_blowfish.adb
-- SUBJECT : A benchmark procedure for Blowfish.
-- AUTHOR  : (C) Copyright 2012 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
with ACO.Crypto.Block_Cipher.Blowfish;
with Generic_Block_Cipher;
with Dispatching_Block_Cipher;

procedure Benchmark_Blowfish is
   procedure Nondispatching_Block_Cipher is
     new Generic_Block_Cipher
       (Cipher_Type => ACO.Crypto.Block_Cipher.Blowfish.Blowfish_Cipher,
        Block_Size  => ACO.Crypto.Block_Cipher.Blowfish.Block_Size,
        Encrypt     => ACO.Crypto.Block_Cipher.Blowfish.Encrypt,
        Decrypt     => ACO.Crypto.Block_Cipher.Blowfish.Decrypt);

   Blowfish_Encryption_Engine : ACO.Crypto.Block_Cipher.Blowfish.Blowfish_Cipher;
   Blowfish_Decryption_Engine : ACO.Crypto.Block_Cipher.Blowfish.Blowfish_Cipher;

   Key : ACO.Octet_Array(0 .. 7) := (1, 2, 3, 4, 5, 6, 7, 8);
begin
   ACO.Crypto.Block_Cipher.Blowfish.Make(Blowfish_Encryption_Engine, Key);
   ACO.Crypto.Block_Cipher.Blowfish.Make(Blowfish_Decryption_Engine, Key);
   Dispatching_Block_Cipher(Blowfish_Encryption_Engine, Blowfish_Decryption_Engine);
   Nondispatching_Block_Cipher(Blowfish_Encryption_Engine, Blowfish_Decryption_Engine);
end Benchmark_Blowfish;
