with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;     use Ada.Containers;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Interfaces;                 use Interfaces;

procedure Day24_2 is
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

   package Coordinates is new Vectors(Index_Type   => Natural,
                                      Element_Type => Coordinate);
   use Coordinates;

   function Coordinate_Hash (Elem : in Coordinate) return Hash_Type is
   begin
      return Hash_Type(Unsigned_32(Elem.X + 2**16) xor Unsigned_32(Elem.Y + 2**16));
   end Coordinate_Hash;

   type Color is (Black, White);

   -- Contains all black tiles
   package Floor is new Hashed_Maps(Key_Type        => Coordinate,
                                    Element_Type    => Color,
                                    Hash            => Coordinate_Hash,
                                    Equivalent_Keys => "=");
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
   function Flip(Input : in Input_Data) return Floor.Map is
      Result      : Floor.Map;
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
            Result.Insert(Current_Pos, Black);
         end if;
         Current_Pos := Coordinate'(0,0);
      end loop List_Loop;
      return Result;
   end Flip;

   procedure Add_If_Needed(Current_Floor : in Floor.Map;
                           Coordinates_To_Add : in out Coordinates.Vector;
                           X : Small_Integer;
                           Y : Small_Integer) is
      Pos_To_Check       : Coordinate;
   begin
      Pos_To_Check := Coordinate'(X, Y);
      if not Coordinates_To_Add.Contains(Pos_To_Check)
        and then not Current_Floor.Contains(Pos_To_Check) then
         Coordinates_To_Add.Append(Pos_To_Check);
      end if;
   end Add_If_Needed;


   -- Put a white tile around every black tile.
   procedure Grow (Input : in out Floor.Map) is
      Coordinates_To_Add : Coordinates.Vector;
      Current_Pos        : Coordinate;
   begin
      Tile_Loop:
      for T in Input.Iterate loop
         if Input(T) = Black then
            Current_Pos := Key(T);
            Add_If_Needed(Input, Coordinates_To_Add, Current_Pos.X + 1, Current_Pos.Y);
            Add_If_Needed(Input, Coordinates_To_Add, Current_Pos.X - 1, Current_Pos.Y);
            Add_If_Needed(Input, Coordinates_To_Add, Current_Pos.X, Current_Pos.Y + 1);
            Add_If_Needed(Input, Coordinates_To_Add, Current_Pos.X, Current_Pos.Y - 1);
            Add_If_Needed(Input, Coordinates_To_Add, Current_Pos.X + 1, Current_Pos.Y + 1);
            Add_If_Needed(Input, Coordinates_To_Add, Current_Pos.X - 1, Current_Pos.Y - 1);
         end if;
      end loop Tile_Loop;

      Grow_Loop:
      for Pos of Coordinates_To_Add loop
         Input.Insert(Pos, White);
      end loop Grow_Loop;
   end Grow;

   -- Returns the number of Black tiles around the tile given by coordinate.
   function Count_Around(A_Floor : in Floor.Map;
                         Tile    : in Coordinate) return Integer is
      Result : Integer := 0;
      To_Check : Coordinate;
   begin
      --East
      To_Check := Coordinate'(Tile.X + 1, Tile.Y);
      if A_Floor.Contains(To_Check) and then A_Floor(To_Check) = Black then
         Result := Result + 1;
      end if;
      --West
      To_Check := Coordinate'(Tile.X - 1, Tile.Y);
      if A_Floor.Contains(To_Check) and then A_Floor(To_Check) = Black then
         Result := Result + 1;
      end if;
      --North West
      To_Check := Coordinate'(Tile.X, Tile.Y + 1);
      if A_Floor.Contains(To_Check) and then A_Floor(To_Check) = Black then
         Result := Result + 1;
      end if;
      --South East
      To_Check := Coordinate'(Tile.X, Tile.Y - 1);
      if A_Floor.Contains(To_Check) and then A_Floor(To_Check) = Black then
         Result := Result + 1;
      end if;
      --North East
      To_Check := Coordinate'(Tile.X + 1, Tile.Y + 1);
      if A_Floor.Contains(To_Check) and then A_Floor(To_Check) = Black then
         Result := Result + 1;
      end if;
      --South West
      To_Check := Coordinate'(Tile.X - 1, Tile.Y - 1);
      if A_Floor.Contains(To_Check) and then A_Floor(To_Check) = Black then
         Result := Result + 1;
      end if;

      return Result;
   end Count_Around;

   function Game(Old_Floor : in Floor.Map) return Floor.Map is
      New_Floor : Floor.Map := Old_Floor;
      Neighbours : Integer;
   begin
      Tile_Loop:
      for T in Old_Floor.Iterate loop
         Neighbours := Count_Around(Old_Floor, Key(T));
         case Old_Floor(T) is
            when White =>
               if Neighbours = 2 then
                  New_Floor(Key(T)) := Black;
               end if;
            when Black =>
               if Neighbours = 0 or else Neighbours > 2 then
                  New_Floor(Key(T)) := White;
               end if;
         end case;
      end loop Tile_Loop;
      return New_Floor;
   end Game;

   Input  : Input_Data;
   Lobby_Floor : Floor.Map;

   Answer : Integer := 0;
begin
   Load_File(Input);

   Lobby_Floor := Flip(Input);

   Game_Of_Life_Loop:
   for I in 1 .. 100 loop
      Grow(Lobby_Floor);
      Lobby_Floor := Game(Lobby_Floor);
   end loop Game_Of_Life_Loop;

   Count_Loop:
   for Tile in Lobby_Floor.Iterate loop
      if Lobby_Floor(Tile) = Black then
         Answer := Answer + 1;
      end if;
   end loop Count_Loop;

   Put_line("The number of tiles with the black side up is " & Integer'Image(Answer));
end Day24_2;
