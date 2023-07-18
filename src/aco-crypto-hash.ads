---------------------------------------------------------------------------
-- FILE    : aco-crypto-hash.ads
-- SUBJECT : Package that defines the interface to the hash algorithms.
-- AUTHOR  : (C) Copyright 2008 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
with Ada.Streams;
with Interfaces;

package ACO.Crypto.Hash is

   -- This type holds the resulting hash.
   type Hash_Value(<>) is private;

   -- Converts a hash value into a displayable string.
   function To_String(Value : Hash_Value) return String;

   -------------------
   -- Hasher Interface
   -------------------

   type Hasher is interface;

   -- Puts hasher object in a state where it can accept data.
   procedure Start_Hashing(Hash_Object : in out Hasher) is null;

   -- Finalize the hash computation.
   procedure Finish_Hashing(Hash_Object : in out Hasher) is null;

   -- Return the resulting hash value.
   function Retrieve_Hash(Hash_Object : Hasher) return Hash_Value is abstract;

   -- Compute hash of data provided in blocks.
   procedure Compute_Hash(Hash_Object : in out Hasher;
                          Data_Block  : in Octet_Array) is abstract;

   ---------------------
   -- Stream Hasher Type
   ---------------------

   -- Reading a stream hasher object makes no sense.
   Read_Hash : exception;

   type Stream_Hasher(Engine : access Hasher'Class) is
      abstract new Ada.Streams.Root_Stream_Type with private;

   -- Puts hasher object in a state where it can accept data.
   procedure Start_Hashing(Hash_Object : in out Stream_Hasher);

   -- Finalize the hash computation.
   procedure Finish_Hashing(Hash_Object : in out Stream_Hasher);

   -- Return the resulting hash value.
   function Retrieve_Hash(Hash_Object : Stream_Hasher) return Hash_Value;

   -- Override Root_Stream_Type's read procedure to simply throw Read_Hash.
   overriding
   procedure Read(Hash_Object : in out Stream_Hasher;
                  Item        : out Ada.Streams.Stream_Element_Array;
                  Last        : out Ada.Streams.Stream_Element_Offset);

   -- Override Root_Stream_Type's write procedure to call the underlying engine.
   overriding
   procedure Write(Hash_Object : in out Stream_Hasher;
                   Item        : in Ada.Streams.Stream_Element_Array);

private

   -- Holds hash values in some suitable way.
   type Hash_Value(Size : Natural) is
      record
         Hash_Buffer : Octet_Array(1 .. Size);
      end record;

   type Stream_Hasher(Engine : access Hasher'Class) is
      abstract new Ada.Streams.Root_Stream_Type with null record;

end ACO.Crypto.Hash;
