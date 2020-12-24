with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;     use Ada.Containers;
with Ada.Containers.Hashed_Sets; use Ada.Containers;
with Interfaces;                 use Interfaces;

procedure Day24_1 is
   -- ### CONSTANTS ### --
   Enable_Debug     : constant Boolean := False;
   Number_Of_Inputs : constant Integer := 394;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Direction is (East, South_East, South_West, West, North_West, North_East);

   package Directions is new Vectors(Index_Type   => Natural,
                                     Element_Type => Direction);
   use Directions;

   type Input_Data is array (1 .. Number_Of_Inputs) of Directions.Vector;

   subtype Small_Integer is Integer range -2**16 .. 2**16 - 1;

   type Coordinate is record
      X: Small_Integer;
      Y: Small_Integer;
   end record;

   function Coordinate_Hash (Elem : in Coordinate) return Hash_Type is
   begin
      return Hash_Type(Unsigned_32(Elem.X + 2**16) xor Unsigned_32(Elem.Y + 2**16));
   end Coordinate_Hash;

   -- Contains all black tiles
   package Floor is new Hashed_Sets(Element_Type        => Coordinate,
                                    Hash                => Coordinate_Hash,
                                    Equivalent_Elements => "=");
   use Floor;

   function String_To_Directions(Input : in Unbounded_String) return Directions.Vector is
      Curr_Char       : Character;
      Result          : Directions.Vector;
      Is_Intermediate : Boolean := False;
      Previous_Char   : Character := 'x';
   begin
      Parser_Loop:
      for I in 1 .. Length(Input) loop
         Curr_Char := Element(Input, I);
         if Is_Intermediate then
            case Curr_Char is
               when 'e' =>
                  if Previous_Char = 'n' then
                     Result.Append(North_East);
                  elsif Previous_Char = 's' then
                     Result.Append(South_East);
                  else
                     Put_Line("ERROR: Previous char is invalid: " & Previous_Char);
                  end if;
               when 'w' =>
                  if Previous_Char = 'n' then
                     Result.Append(North_West);
                  elsif Previous_Char = 's' then
                     Result.Append(South_West);
                  else
                     Put_Line("ERROR: Previous char is invalid: " & Previous_Char);
                  end if;
               when others => Put_Line("ERROR: Invalid character: " & Curr_Char);
            end case;
            Is_Intermediate := False;
         else
            case Curr_Char is
               when 'e' => Result.Append(East);
               when 'w' => Result.Append(West);
               when others =>
                  Is_Intermediate := True;
                  Previous_Char := Curr_Char;
            end case;
         end if;
      end loop Parser_Loop;

      return Result;
   end String_To_Directions;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   procedure Load_File (Result : out Input_Data) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx : Natural := 1;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day24.input");
      Read_Loop:
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Result(Idx) := String_To_Directions(Current_Line);
         Idx := Idx + 1;
      end loop Read_Loop;
   end Load_File;

   --  /\ /\ /\ /\
   -- |20|21|22|23|
   --  \/ \/ \/ \/\
   --  |10|11|12|13|
   -- / \/ \/ \/ \/
   -- |  |00|01|02|
   --  \/ \/ \/ \/
   -- _
   --|\y
   --  \
   --   \
   --    -----> x
   function Flip(Input : in Input_Data) return Floor.Set is
      Result : Floor.Set;
      Current_Pos : Coordinate := Coordinate'(X => 0,
                                              Y => 0);
   begin
      List_Loop:
      for List of Input loop
         Dirs_Loop:
         for Dir of List loop
            case Dir is
               when East => Current_Pos.X := Current_Pos.X + 1;
               when South_East => Current_Pos.Y := Current_Pos.Y - 1;
               when South_West =>
                  Current_Pos.X := Current_Pos.X - 1;
                  Current_Pos.Y := Current_Pos.Y - 1;
               when West => Current_Pos.X := Current_Pos.X - 1;
               when North_West => Current_Pos.Y := Current_Pos.Y + 1;
               when North_East =>
                  Current_Pos.X := Current_Pos.X + 1;
                  Current_Pos.Y := Current_Pos.Y + 1;
            end case;
         end loop Dirs_Loop;

         if Result.Contains(Current_Pos) then
            Result.Delete(Current_Pos);
         else
            Result.Insert(Current_Pos);
         end if;
         Current_Pos := Coordinate'(0,0);
      end loop List_Loop;
      return Result;
   end Flip;

   Input  : Input_Data;
   Lobby_Floor : Floor.Set;

   Answer : Integer := 0;
begin
   Load_File(Input);

   Lobby_Floor := Flip(Input);

   Answer := Integer(Lobby_Floor.Length);

   Put_line("The number of tiles with the black side up is " & Integer'Image(Answer));
end Day24_1;
