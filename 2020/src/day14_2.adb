with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day14_2 is
   -- ### CONSTANTS ### --
   Enable_Debug     : constant Boolean := False;
   Word_Size        : constant Integer := 36;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;
   subtype Mask_String is String (1 .. Word_Size);
   type Unsigned_36 is mod 2**36;
   type Docking_Word is array (1 .. Word_Size) of Unsigned_36;

   package Bit_Vector is new Vectors (Natural, Unsigned_36);
   use     Bit_Vector;

   function Natural_Hash (Id : Unsigned_36) return Hash_Type is
   begin
      -- I don't know how to calculate a Hash that makes more sense.
      return Hash(Unsigned_36'Image(id));
   end Natural_Hash;

   package Memory is new Hashed_Maps (Key_Type        => Unsigned_36,
                                      Element_Type    => Unsigned_36,
                                      Hash            => Natural_Hash,
                                      Equivalent_Keys => "=");
   use Memory;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   procedure Parse_String(Data    : in Unbounded_String;
                          Address : out Unsigned_36;
                          Value   : out Unsigned_36) is
      Curr_Char : Character;
      Buffer : Unbounded_String := To_Unbounded_String("");
   begin
      Parser_Loop:
      for I in 1 .. Length(Data) loop
         Curr_Char := Element(Data, I);
         if Curr_Char = '[' then
            Buffer := To_Unbounded_String("");
         elsif Curr_Char = ']' then
            Address := Unsigned_36'Value(To_String(Buffer));
            Buffer := To_Unbounded_String("");
         elsif Curr_Char = ' ' then
            Buffer := To_Unbounded_String("");
         elsif Curr_Char = '=' then
            Buffer := To_Unbounded_String("");
         else
            Buffer := Buffer & Curr_Char;
         end if;
      end loop Parser_Loop;
      Value := Unsigned_36'Value(To_String(Buffer));
   end Parse_String;

   function Mask_Address (Address : in Unsigned_36;
                          Mask    : in Mask_String) return Bit_Vector.Vector is
      Old_Length : Integer;
      Curr_Mask  : Mask_String;
      Curr_Value : Unsigned_36;
      Result     : Bit_Vector.Vector;
   begin
      Result.Append(Address);
      Log_Debug("First value: " & Unsigned_36'Image(Address));
      Masking_Loop:
      for I in Mask'Range loop
         Log_Debug("Mask iteration " & Integer'Image(I));
         if Mask(I) = '1' then
            Curr_Mask := (others => '0');
            Curr_Mask(I) := '1';
            Update_Data_Loop:
            for J in Result.First_Index .. Result.Last_Index loop
               Result(J) := Result(J) or Unsigned_36'Value("2#" & Curr_Mask & "#");
            end loop Update_Data_Loop;
         elsif Mask(I) = 'X' then
            Old_Length := Integer(Result.Length);
            Log_Debug("Old length" & Integer'Image(Old_Length));
            Duplicate_Values_Loop:
            for J in Result.First_Index .. Result.Last_Index loop
               Curr_Value := Result(J);
               Result.Append(Curr_Value);
               Log_Debug("Appending " & Unsigned_36'Image(Curr_Value));
            end loop Duplicate_Values_Loop;
            Update_Values_Loop:
            for J in Result.First_Index .. Result.Last_Index loop
               if J < Old_Length then
                  -- For the first half, use a '1'
                  Curr_Mask := (others => '0');
                  Curr_Mask(I) := '1';
                  Result(J) := Result(J) or Unsigned_36'Value("2#" & Curr_Mask & "#");
                  Log_Debug("Adding 1, resulting in " & Unsigned_36'Image(Result(J)));
               else
                  -- For the second half, use a '0'
                  Curr_Mask := (others => '1');
                  Curr_Mask(I) := '0';
                  Result(J) := Result(J) and Unsigned_36'Value("2#" & Curr_Mask & "#");
                  Log_Debug("Adding 0, resulting in " & Unsigned_36'Image(Result(J)));
               end if;
            end loop Update_Values_Loop;
         end if;
      end loop Masking_Loop;

      for V of Result loop
         Log_Debug(Unsigned_36'Image(V));
      end loop;

      return Result;
   end Mask_Address;

   procedure Add_To_Map(Data           : in Unbounded_String;
                        Mask           : in Mask_String;
                        Current_Memory : in out Memory.Map) is
      Address          : Unsigned_36;
      Value            : Unsigned_36;
      Masked_Addresses : Bit_Vector.Vector;
   begin
      Parse_String(Data, Address, Value);

      Masked_Addresses := Mask_Address(Address, Mask);

      Update_Memory_Loop:
      for Masked_Address of Masked_Addresses loop
         if Current_Memory.Contains(Masked_Address) then
            Current_Memory.Replace(Masked_Address, Value);
         else
            Current_Memory.Insert(Masked_Address, Value);
         end if;
      end loop Update_Memory_Loop;
   end Add_To_Map;

   procedure Load_File (Input_Memory     : out Memory.Map) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 1;

      Current_Mask : Mask_String := (others => 'X');
   begin
      Open (File => Input, Mode => In_File, Name => "data/day14.input");
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         if Slice(Current_Line, 1, 4) = "mask" then
            Current_Mask := Slice(Current_Line, 8, 43);
         else
            Add_To_Map(Current_Line, Current_Mask, Input_Memory);
         end if;
         Idx := Idx + 1;
      end loop;
      Close (File => Input);
   end Load_File;

   function Sum_Of_Values(Input : in Memory.Map) return Long_Integer is
      Result : Long_Integer := 0;
      Value  : Unsigned_36;
   begin
      Sum_Loop:
      for E in Input.Iterate loop
         Value := Input(E);
         Result := Result + Long_Integer(Value);
      end loop Sum_Loop;
      return Result;
   end Sum_Of_Values;

   Program_Memory : Memory.Map;
   Answer         : Long_Integer;
begin
   Load_File(Program_Memory);

   Dump_Memory_Loop:
   for K in Program_Memory.Iterate loop
      Log_Debug(Unsigned_36'Image(Key(K)) & ": " & Unsigned_36'Image(Program_Memory(K)));
   end loop Dump_Memory_Loop;


   Answer := Sum_Of_Values(Program_Memory);

   Put_line("The sum of all values is " & Long_Integer'Image(Answer));
end Day14_2;
