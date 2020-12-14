with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with interfaces; use Interfaces;

procedure Day14_1 is
   -- ### CONSTANTS ### --
   Enable_Debug     : constant Boolean := False;
   Word_Size        : constant Integer := 36;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;
   subtype Mask_String is String (1 .. Word_Size);
   type Unsigned_36 is mod 2**36;
   type Docking_Word is array (1 .. Word_Size) of Unsigned_36;

   function Natural_Hash (id : Natural) return Hash_Type is
   begin
      return Hash_Type(id);
   end Natural_Hash;

   package Memory is new Hashed_Maps (Key_Type        => Natural,
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
                          Address : out Integer;
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
            Address := Integer'Value(To_String(Buffer));
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

   -- To calculate the masked value, do a bitwise AND with the mask where 'X'
   -- is 1 and a bitwise OR with the mask where 'X' is 0.
   function Mask_Value (Data : in Unsigned_36;
                        Mask : in Mask_String) return Unsigned_36 is
      And_Mask  : Mask_String := Mask;
      Or_Mask   : Mask_String := Mask;
      And_Value : Unsigned_36;
      Or_Value  : Unsigned_36;
   begin
      And_Loop:
      for I in Mask'Range loop
         if Mask(I) = 'X' then
            And_Mask(I) := '1';
         end if;
      end loop And_Loop;
      And_Value := Unsigned_36'Value("2#" & And_Mask & "#");

      Or_Loop:
      for I in Mask'Range loop
         if Mask(I) = 'X' then
            Or_Mask(I) := '0';
         end if;
      end loop Or_Loop;
      Or_Value := Unsigned_36'Value("2#" & Or_Mask & "#");

      return (Data and And_Value) or Or_Value;
   end Mask_Value;

   procedure Add_To_Map(Data           : in Unbounded_String;
                        Mask           : in Mask_String;
                        Current_Memory : in out Memory.Map) is
      Address      : Integer;
      Value        : Unsigned_36;
      Masked_Value : Unsigned_36;
   begin
      Parse_String(Data, Address, Value);

      Masked_Value := Mask_Value(Value, Mask);

      if Current_Memory.Contains(Address) then
         Current_Memory.Replace(Address, Masked_Value);
      else
         Current_Memory.Insert(Address, Masked_Value);
      end if;
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

   Answer := Sum_Of_Values(Program_Memory);

   Put_line("The sum of all values is " & Long_Integer'Image(Answer));
end Day14_1;
