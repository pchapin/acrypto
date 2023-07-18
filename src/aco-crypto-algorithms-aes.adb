---------------------------------------------------------------------------
-- FILE    : aco-crypto-algorithms-aes.adb
-- SUBJECT : Body of a package holding the raw AES algorithm.
-- AUTHOR  : (C) Copyright 2014 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <spicacality@kelseymountain.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

with ACO.Quadruple_Octet_Operations;

package body ACO.Crypto.Algorithms.AES is

   type S_Box_Type is array(ACO.Octet) of ACO.Octet;

   S_Box : constant S_Box_Type := S_Box_Type'
     (16#63#, 16#7c#, 16#77#, 16#7b#, 16#f2#, 16#6b#, 16#6f#, 16#c5#,
      16#30#, 16#01#, 16#67#, 16#2b#, 16#fe#, 16#d7#, 16#ab#, 16#76#,

      16#ca#, 16#82#, 16#c9#, 16#7d#, 16#fa#, 16#59#, 16#47#, 16#f0#,
      16#ad#, 16#d4#, 16#a2#, 16#af#, 16#9c#, 16#a4#, 16#72#, 16#c0#,

      16#b7#, 16#fd#, 16#93#, 16#26#, 16#36#, 16#3f#, 16#f7#, 16#cc#,
      16#34#, 16#a5#, 16#e5#, 16#f1#, 16#71#, 16#d8#, 16#31#, 16#15#,

      16#04#, 16#c7#, 16#23#, 16#c3#, 16#18#, 16#96#, 16#05#, 16#9a#,
      16#07#, 16#12#, 16#80#, 16#e2#, 16#eb#, 16#27#, 16#b2#, 16#75#,

      16#09#, 16#83#, 16#2c#, 16#1a#, 16#1b#, 16#6e#, 16#5a#, 16#a0#,
      16#52#, 16#3b#, 16#d6#, 16#b3#, 16#29#, 16#e3#, 16#2f#, 16#84#,

      16#53#, 16#d1#, 16#00#, 16#ed#, 16#20#, 16#fc#, 16#b1#, 16#5b#,
      16#6a#, 16#cb#, 16#be#, 16#39#, 16#4a#, 16#4c#, 16#58#, 16#cf#,

      16#d0#, 16#ef#, 16#aa#, 16#fb#, 16#43#, 16#4d#, 16#33#, 16#85#,
      16#45#, 16#f9#, 16#02#, 16#7f#, 16#50#, 16#3c#, 16#9f#, 16#a8#,

      16#51#, 16#a3#, 16#40#, 16#8f#, 16#92#, 16#9d#, 16#38#, 16#f5#,
      16#bc#, 16#b6#, 16#da#, 16#21#, 16#10#, 16#ff#, 16#f3#, 16#d2#,

      16#cd#, 16#0c#, 16#13#, 16#ec#, 16#5f#, 16#97#, 16#44#, 16#17#,
      16#c4#, 16#a7#, 16#7e#, 16#3d#, 16#64#, 16#5d#, 16#19#, 16#73#,

      16#60#, 16#81#, 16#4f#, 16#dc#, 16#22#, 16#2a#, 16#90#, 16#88#,
      16#46#, 16#ee#, 16#b8#, 16#14#, 16#de#, 16#5e#, 16#0b#, 16#db#,

      16#e0#, 16#32#, 16#3a#, 16#0a#, 16#49#, 16#06#, 16#24#, 16#5c#,
      16#c2#, 16#d3#, 16#ac#, 16#62#, 16#91#, 16#95#, 16#e4#, 16#79#,

      16#e7#, 16#c8#, 16#37#, 16#6d#, 16#8d#, 16#d5#, 16#4e#, 16#a9#,
      16#6c#, 16#56#, 16#f4#, 16#ea#, 16#65#, 16#7a#, 16#ae#, 16#08#,

      16#ba#, 16#78#, 16#25#, 16#2e#, 16#1c#, 16#a6#, 16#b4#, 16#c6#,
      16#e8#, 16#dd#, 16#74#, 16#1f#, 16#4b#, 16#bd#, 16#8b#, 16#8a#,

      16#70#, 16#3e#, 16#b5#, 16#66#, 16#48#, 16#03#, 16#f6#, 16#0e#,
      16#61#, 16#35#, 16#57#, 16#b9#, 16#86#, 16#c1#, 16#1d#, 16#9e#,

      16#e1#, 16#f8#, 16#98#, 16#11#, 16#69#, 16#d9#, 16#8e#, 16#94#,
      16#9b#, 16#1e#, 16#87#, 16#e9#, 16#ce#, 16#55#, 16#28#, 16#df#,

      16#8c#, 16#a1#, 16#89#, 16#0d#, 16#bf#, 16#e6#, 16#42#, 16#68#,
      16#41#, 16#99#, 16#2d#, 16#0f#, 16#b0#, 16#54#, 16#bb#, 16#16#);

   Inverse_S_Box : constant S_Box_Type := S_Box_Type'
     (16#52#, 16#09#, 16#6a#, 16#d5#, 16#30#, 16#36#, 16#a5#, 16#38#,
      16#bf#, 16#40#, 16#a3#, 16#9e#, 16#81#, 16#f3#, 16#d7#, 16#fb#,

      16#7c#, 16#e3#, 16#39#, 16#82#, 16#9b#, 16#2f#, 16#ff#, 16#87#,
      16#34#, 16#8e#, 16#43#, 16#44#, 16#c4#, 16#de#, 16#e9#, 16#cb#,

      16#54#, 16#7b#, 16#94#, 16#32#, 16#a6#, 16#c2#, 16#23#, 16#3d#,
      16#ee#, 16#4c#, 16#95#, 16#0b#, 16#42#, 16#fa#, 16#c3#, 16#4e#,

      16#08#, 16#2e#, 16#a1#, 16#66#, 16#28#, 16#d9#, 16#24#, 16#b2#,
      16#76#, 16#5b#, 16#a2#, 16#49#, 16#6d#, 16#8b#, 16#d1#, 16#25#,

      16#72#, 16#f8#, 16#f6#, 16#64#, 16#86#, 16#68#, 16#98#, 16#16#,
      16#d4#, 16#a4#, 16#5c#, 16#cc#, 16#5d#, 16#65#, 16#b6#, 16#92#,

      16#6c#, 16#70#, 16#48#, 16#50#, 16#fd#, 16#ed#, 16#b9#, 16#da#,
      16#5e#, 16#15#, 16#46#, 16#57#, 16#a7#, 16#8d#, 16#9d#, 16#84#,

      16#90#, 16#d8#, 16#ab#, 16#00#, 16#8c#, 16#bc#, 16#d3#, 16#0a#,
      16#f7#, 16#e4#, 16#58#, 16#05#, 16#b8#, 16#b3#, 16#45#, 16#06#,

      16#d0#, 16#2c#, 16#1e#, 16#8f#, 16#ca#, 16#3f#, 16#0f#, 16#02#,
      16#c1#, 16#af#, 16#bd#, 16#03#, 16#01#, 16#13#, 16#8a#, 16#6b#,

      16#3a#, 16#91#, 16#11#, 16#41#, 16#4f#, 16#67#, 16#dc#, 16#ea#,
      16#97#, 16#f2#, 16#cf#, 16#ce#, 16#f0#, 16#b4#, 16#e6#, 16#73#,

      16#96#, 16#ac#, 16#74#, 16#22#, 16#e7#, 16#ad#, 16#35#, 16#85#,
      16#e2#, 16#f9#, 16#37#, 16#e8#, 16#1c#, 16#75#, 16#df#, 16#6e#,

      16#47#, 16#f1#, 16#1a#, 16#71#, 16#1d#, 16#29#, 16#c5#, 16#89#,
      16#6f#, 16#b7#, 16#62#, 16#0e#, 16#aa#, 16#18#, 16#be#, 16#1b#,

      16#fc#, 16#56#, 16#3e#, 16#4b#, 16#c6#, 16#d2#, 16#79#, 16#20#,
      16#9a#, 16#db#, 16#c0#, 16#fe#, 16#78#, 16#cd#, 16#5a#, 16#f4#,

      16#1f#, 16#dd#, 16#a8#, 16#33#, 16#88#, 16#07#, 16#c7#, 16#31#,
      16#b1#, 16#12#, 16#10#, 16#59#, 16#27#, 16#80#, 16#ec#, 16#5f#,

      16#60#, 16#51#, 16#7f#, 16#a9#, 16#19#, 16#b5#, 16#4a#, 16#0d#,
      16#2d#, 16#e5#, 16#7a#, 16#9f#, 16#93#, 16#c9#, 16#9c#, 16#ef#,

      16#a0#, 16#e0#, 16#3b#, 16#4d#, 16#ae#, 16#2a#, 16#f5#, 16#b0#,
      16#c8#, 16#eb#, 16#bb#, 16#3c#, 16#83#, 16#53#, 16#99#, 16#61#,

      16#17#, 16#2b#, 16#04#, 16#7e#, 16#ba#, 16#77#, 16#d6#, 16#26#,
      16#e1#, 16#69#, 16#14#, 16#63#, 16#55#, 16#21#, 16#0c#, 16#7d#);

   procedure SubBytes(State : in out State_Type)
     with
       Depends => (State => State)
   is
   begin
      for I in State_Row_Index_Type loop
         for J in State_Column_Index_Type loop
            State(I, J) := S_Box(State(I, J));
         end loop;
      end loop;
   end SubBytes;


   procedure InvSubBytes(State : in out State_Type)
     with
       Depends => (State => State)
   is
   begin
      for I in State_Row_Index_Type loop
         for J in State_Column_Index_Type loop
            State(I, J) := Inverse_S_Box(State(I, J));
         end loop;
      end loop;
   end InvSubBytes;


   procedure ShiftRows(State : in out State_Type)
     with
       Depends => (State => State)
   is

      procedure Shift_One_Row(Row : in State_Row_Index_Type; Amount : in State_Column_Index_Type)
        with
          Global => (In_Out => State),
          Depends => (State =>+ (Row, Amount))
      is
         type Temporary_Array_Type is array(State_Column_Index_Type) of ACO.Octet;
         Temporary_Array : Temporary_Array_Type := Temporary_Array_Type'(others => 0);
      begin
         -- Copy items from State to Temporary_Array, wrapping around if necessary.
         for J in State_Column_Index_Type loop
            Temporary_Array(J - Amount) := State(Row, J);
         end loop;

         -- Copy shifted row back to State.
         for J in State_Column_Index_Type loop
            State(Row, J) := Temporary_Array(J);
         end loop;
      end Shift_One_Row;

   begin
      Shift_One_Row(1, 1);
      Shift_One_Row(2, 2);
      Shift_One_Row(3, 3);
   end ShiftRows;


   procedure InvShiftRows(State : in out State_Type)
     with
       Depends => (State => State)
   is

      procedure Shift_One_Row(Row : in State_Row_Index_Type; Amount : in State_Column_Index_Type)
        with
          Global => (In_Out => State),
          Depends => (State =>+ (Row, Amount))
      is
         type Temporary_Array_Type is array(State_Column_Index_Type) of ACO.Octet;
         Temporary_Array : Temporary_Array_Type := Temporary_Array_Type'(others => 0);
      begin
         -- Copy items from State to Temporary_Array, wrapping around if necessary.
         for J in State_Column_Index_Type loop
            Temporary_Array(J + Amount) := State(Row, J);
         end loop;

         -- Copy shifted row back to State.
         for J in State_Column_Index_Type loop
            State(Row, J) := Temporary_Array(J);
         end loop;
      end Shift_One_Row;

   begin
      Shift_One_Row(1, 1);
      Shift_One_Row(2, 2);
      Shift_One_Row(3, 3);
   end InvShiftRows;


   procedure AddRoundKey(State : in out State_Type; B : in AES_Algorithm; Round : in Round_Index_Type)
     with
       Depends => (State =>+ (B, Round))
   is
   begin
      for I in State_Row_Index_Type loop
         for J in State_Column_Index_Type loop
            State(I, J) := State(I, J) xor B.W(I, Nb*Round + Key_Column_Index_Type(J));
         end loop;
      end loop;
   end AddRoundKey;


   procedure To_State(Block : in ACO.Octet_Array; State : out State_Type)
     with
       Depends => (State => Block),
       Pre => Block'Length = 16
   is
   begin
      for J in State_Column_Index_Type loop
         for I in State_Row_Index_Type loop
            State(I, J) :=
              Block(Block'First + (4*Natural(J - State_Column_Index_Type'First) + (I - State_Row_Index_Type'First)));
         end loop;
      end loop;
   end To_State;


   procedure From_State(State : in State_Type; Block : out ACO.Octet_Array)
     with
       Depends => (Block =>+ State),
       Pre => Block'Length = 16
   is
   begin
      for J in State_Column_Index_Type loop
         for I in State_Row_Index_Type loop
            Block(Block'First + (4*Natural(J - State_Column_Index_Type'First) + (I - State_Row_Index_Type'First))) :=
              State(I, J);
         end loop;
      end loop;
   end From_State;


   ----------------------
   -- Visible Subprograms
   ----------------------

   procedure Encrypt
     (B     : in     AES_Algorithm;
      Block : in out ACO.Octet_Array) is

      State : State_Type;
   begin
      To_State(Block, State);
      AddRoundKey(State, B, 0);
      for I in Round_Index_Type range 1 .. Nr loop
         SubBytes(State);
         ShiftRows(State);
         if I /= Nr then
            -- MixColumns(State);
            null;
         end if;
         AddRoundKey(State, B, I);
      end loop;
      From_State(State, Block);
   end Encrypt;


   procedure Decrypt
     (B     : in     AES_Algorithm;
      Block : in out ACO.Octet_Array) is

      State : State_Type;
   begin
      To_State(Block, State);
      AddRoundKey(State, B, Nr);
      for I in reverse Round_Index_Type range 0 .. Nr - 1 loop
         InvShiftRows(State);
         InvSubBytes(State);
         AddRoundKey(State, B, I);
         if I /= 0 then
            -- InvMixColumns(State);
            null;
         end if;
      end loop;
      From_State(State, Block);
   end Decrypt;


   procedure Initialize
     (B   : out AES_Algorithm;
      Key : in  ACO.Octet_Array) is

      type Word_Type is array(Key_Row_Index_Type) of ACO.Octet;

      Left  : Word_Type;
      Right : Word_Type;

      function G(Word : Word_Type; Round_Index : Round_Index_Type) return Word_Type
        with
          Pre => Round_Index > 0
      is
         type Round_Constant_Lookup_Table is array(Round_Index_Type) of ACO.Octet;
         Round_Constants : constant Round_Constant_Lookup_Table :=
           Round_Constant_Lookup_Table'(0 =>      0, 1 => 16#01#, 2 => 16#02#, 3 => 16#04#,
                                        4 => 16#08#, 5 => 16#10#, 6 => 16#20#, 7 => 16#40#,
                                        8 => 16#80#, 9 => 16#1B#,10 => 16#36#);

         Result : Word_Type;
      begin
         Result := Word_Type'(others => 0);
         Result(0) := Word(1);
         Result(1) := Word(2);
         Result(2) := Word(3);
         Result(3) := Word(0);

         for I in Key_Row_Index_Type loop
            Result(I) := S_Box(Result(I));
         end loop;
         Result(0) := Result(0) xor Round_Constants(Round_Index);
         return Result;
      end G;

      function Get_Column(Column : Key_Column_Index_Type) return Word_Type
        with
          Global => (Input => B)
      is
         Result : Word_Type;
      begin
         Result := Word_Type'(others => 0);
         for I in Key_Row_Index_Type loop
            Result(I) := B.W(I, Column);
         end loop;
         return Result;
      end Get_Column;

   begin  -- Initialize
      B.W := Expanded_Key_Type'(others => (others => 0));

      -- Initialize the first four columns of the expanded key.
      for J in Key_Column_Index_Type range 0 .. Nk - 1 loop
         for I in Key_Row_Index_Type loop
            B.W(I, J) := Key(I + 4*J);
         end loop;
      end loop;

      -- Fill in the other chunks of the expanded key.
      for K in Round_Index_Type range 1 .. Nr loop
         for J in Key_Column_Index_Type range 4*K .. 4*(K + 1) - 1 loop
            Left  := Get_Column(J - 4);
            Right := Get_Column(J - 1);
            if J = 4*K then
               Left := G(Left, K);
            end if;
            for I in Key_Row_Index_Type loop
               B.W(I, J) := Left(I) xor Right(I);
            end loop;
         end loop;
      end loop;
   end Initialize;

end ACO.Crypto.Algorithms.AES;
