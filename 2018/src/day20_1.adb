with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
procedure Day20_1 is
   String_Length : constant Integer := 14261;
   Maze_Dimension : constant Integer := 100;
   subtype Regex_String is Unbounded_String;
   subtype Coordinate_Idx is Integer range -Maze_Dimension .. Maze_Dimension;

   type Coordinate;
   type Coordinate_Access is access all Coordinate;
   type Coordinate is record
      X : Coordinate_Idx;
      Y : Coordinate_Idx;
      Distance : Integer;
      Root : Coordinate_Access;
   end record;

   type Maze_Row is array (-Maze_Dimension .. Maze_Dimension) of Coordinate_Access;
   type Access_Maze_Row is access all Maze_Row;
   type Maze is array (-Maze_Dimension .. Maze_Dimension) of Access_Maze_Row;

   procedure Load_File (Regex : out Regex_String) is
      Input : File_Type;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day20.input");

      Regex := To_Unbounded_String(Get_Line(File => Input));

      Close (File => Input);
   end Load_File;

   procedure Print_Maze(Input : in Maze) is

   begin
      for R in Input'Range loop
         for C in Input(R)'Range loop
            if R = 0 and C = 0 then
               Put("|" & "(0)");
            else
               if Input(R)(C) /= null then
                  Put("|" & Integer'Image(Input(R)(C).Distance));
               else
                  Put("|" & "  ");
               end if;
            end if;
         end loop;
         Put_Line("|");
         Put_Line("--------------------------------------------------------------");
      end loop;
      Put_Line("==================================================================");
   end Print_Maze;

   procedure Parse_Lineair_String (Input     : in out Regex_String;
                                   Labyrinth : in out Maze;
                                   Next_Root : in Coordinate_Access;
                                   Current_Pos : in out Coordinate_Access) is
      Current_Char : Character;
      Current_Distance : Integer;
      New_Pos : Coordinate_Access;
   begin
      Current_Char := Element(Input, 1);

      while Current_Char = 'N' or Current_Char= 'E'
        or Current_Char = 'S' or Current_Char = 'W' loop

         Current_Distance := Current_Pos.Distance + 1;
         New_Pos := new Coordinate'(X        => Current_Pos.X,
                                    Y        => Current_Pos.Y ,
                                    Distance => Current_Distance,
                                    Root     => Next_Root);
         if Current_Char = 'N' then
            New_Pos.Y := New_Pos.Y - 1;
         elsif Current_Char = 'S' then
            New_Pos.Y := New_Pos.Y + 1;
         elsif Current_Char = 'E' then
            New_Pos.X := New_Pos.X + 1;
         elsif Current_Char = 'W' then
            New_Pos.X := New_Pos.X - 1;
         end if;

         -- If the labyrinth does not yet contain a valid coordinate, fill
         -- in the new one.
         if Labyrinth(New_Pos.Y)(New_Pos.X) = null then
            Labyrinth(New_Pos.Y)(New_Pos.X) := New_Pos;
         end if;

         -- If the new distance is smaller than the old one, or the old one is -1
         -- Update the coordinate with the new data
         if Labyrinth(New_Pos.Y)(New_Pos.X).Distance = -1 or else
           Labyrinth(New_Pos.Y)(New_Pos.X).Distance > Current_Distance then
            Labyrinth(New_Pos.Y)(New_Pos.X).Distance := Current_Distance;

            Labyrinth(New_Pos.Y)(New_Pos.X).Root := New_Pos.Root;
         end if;

         Delete(Input, 1, 1);
         Current_Char := Element(Input, 1);

         Current_Pos := Labyrinth(New_Pos.Y)(New_Pos.X);
      end loop;
   end Parse_Lineair_String;

   procedure Parse_String (Input       : in out Regex_String;
                           Labyrinth   : in out Maze;
                           Next_Root   : in Coordinate_Access;
                           Current_Pos : in out Coordinate_Access) is
      First_Char : Character;
      Working_String : Regex_String;
   begin
      First_Char := Element(Input, 1);
      Working_String := Delete(Input, 1, 1);

      if First_Char = '^' then
         --Put_Line("Starting");
         Parse_String(Input       => Working_String,
                      Labyrinth   => Labyrinth,
                      Next_Root   => Next_Root,
                      Current_Pos => Current_Pos);
      elsif First_Char = '$' then
         --Put_Line("Finished");
         return;
      elsif First_Char = 'N' or First_Char = 'E'
        or First_Char = 'S' or First_Char = 'W' then
         Parse_Lineair_String(Input       => Input,
                              Labyrinth   => Labyrinth,
                              Next_Root   => Next_Root,
                              Current_Pos => Current_Pos);
         Parse_String(Input       => Input,
                      Labyrinth   => Labyrinth,
                      Next_Root   => Next_Root,
                      Current_Pos => Current_Pos);
      elsif First_Char = '(' then
         --Put_Line("Starting branch");
         Parse_String(Input       => Working_String,
                      Labyrinth   => Labyrinth,
                      Next_Root   => Current_Pos,
                      Current_Pos => Current_Pos);
      elsif First_Char = '|' then
         --Put_Line("OR");
         Parse_String(Input       => Working_String,
                      Labyrinth   => Labyrinth,
                      Next_Root   => Next_Root,
                      Current_Pos => Labyrinth(Next_Root.Y)(Next_Root.X));
      elsif First_Char = ')' then
         --Put_Line("Finished branch");
         Parse_String(Input       => Working_String,
                      Labyrinth   => Labyrinth,
                      Next_Root   => Next_Root.Root,
                      Current_Pos => Current_Pos);
      else
         Put_Line("Unsupported character:" & Character'Image(First_Char));
      end if;
   end Parse_String;

   function Create_Maze (Input : in Regex_String) return Maze is
      Result : Maze := (others => new Maze_Row'(others => null));
      Current_Pos : Coordinate_Access := new Coordinate'(X => 0, Y => 0, Distance => 0, Root => null);
      Current_Regex : Regex_String;
   begin
      Current_Regex := Input;

      Result(Current_Pos.X)(Current_Pos.Y) := Current_Pos;

      Parse_String(Input       => Current_Regex,
                   Labyrinth   => Result,
                   Next_Root   => Current_Pos,
                   Current_Pos => Current_Pos);
      return Result;
   end Create_Maze;

   function Find_Largest_Distance(Input : in Maze) return Integer is
      Result : Integer := 0;
   begin
      for R in Input'Range loop
         for C in Input(R)'Range loop
            if Input(R)(C) /= null and then Input(R)(C).Distance > Result then
               Result := Input(R)(C).Distance;
            end if;
         end loop;
      end loop;
      return Result;
   end Find_Largest_Distance;

   Input_Regex : Regex_String;
   Input_Maze : Maze;
   Answer : Integer;
begin
   Load_File (Regex => Input_Regex);

   Input_Maze := Create_Maze(Input => Input_Regex);

   --Print_Maze(Input => Input_Maze);

   Answer := Find_Largest_Distance(Input => Input_Maze);

   Put_Line(Integer'Image(Answer));
end Day20_1;
