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

   procedure Parse_Lineair_String (Input     : in out Regex_String;
                                   Labyrinth : in out Maze;
                                   Current_Pos : in out Coordinate_Access) is
      Current_Char : Character;
      Current_Distance : Integer;
   begin
      Current_Char := Element(Input, 1);
      Current_Distance := Labyrinth(Current_Pos.Y)(Current_Pos.X).Distance;

      while Current_Char = 'N' or Current_Char= 'E'
        or Current_Char = 'S' or Current_Char = 'W' loop
         Current_Distance := Current_Distance + 1;
         if Current_Char = 'N' then
            Current_Pos.Y := Current_Pos.Y - 1;
            if Labyrinth(Current_Pos.Y)(Current_Pos.X) = null then
               Labyrinth(Current_Pos.Y)(Current_Pos.X) := new Coordinate'(X        => Current_Pos.X,
                                                                          Y        => Current_Pos.Y,
                                                                          Distance => -1,
                                                                          Root     => Current_Pos.Root);
            end if;

            if Labyrinth(Current_Pos.Y)(Current_Pos.X).Distance = -1 or else
              Labyrinth(Current_Pos.Y)(Current_Pos.X).Distance > Current_Distance then
               Labyrinth(Current_Pos.Y)(Current_Pos.X).Distance := Current_Distance;
            end if;
         elsif Current_Char = 'S' then
            Current_Pos.Y := Current_Pos.Y + 1;
            if Labyrinth(Current_Pos.Y)(Current_Pos.X) = null then
               Labyrinth(Current_Pos.Y)(Current_Pos.X) := new Coordinate'(X        => Current_Pos.X,
                                                                          Y        => Current_Pos.Y,
                                                                          Distance => -1,
                                                                          Root     => Current_Pos.Root);
            end if;
            if Labyrinth(Current_Pos.Y)(Current_Pos.X).Distance = -1 or else
              Labyrinth(Current_Pos.Y)(Current_Pos.X).Distance > Current_Distance then
               Labyrinth(Current_Pos.Y)(Current_Pos.X).Distance := Current_Distance;
            end if;
         elsif Current_Char = 'E' then
            Current_Pos.X := Current_Pos.X + 1;
            if Labyrinth(Current_Pos.Y)(Current_Pos.X) = null then
               Labyrinth(Current_Pos.Y)(Current_Pos.X) := new Coordinate'(X        => Current_Pos.X,
                                                                          Y        => Current_Pos.Y,
                                                                          Distance => -1,
                                                                          Root     => Current_Pos.Root);
            end if;
            if Labyrinth(Current_Pos.Y)(Current_Pos.X).Distance = -1 or else
              Labyrinth(Current_Pos.Y)(Current_Pos.X).Distance > Current_Distance then
               Labyrinth(Current_Pos.Y)(Current_Pos.X).Distance := Current_Distance;
            end if;
         elsif Current_Char = 'W' then
            Current_Pos.X := Current_Pos.X - 1;
            if Labyrinth(Current_Pos.Y)(Current_Pos.X) = null then
               Labyrinth(Current_Pos.Y)(Current_Pos.X) := new Coordinate'(X        => Current_Pos.X,
                                                                          Y        => Current_Pos.Y,
                                                                          Distance => -1,
                                                                          Root     => Current_Pos.Root);
            end if;
            if Labyrinth(Current_Pos.Y)(Current_Pos.X).Distance = -1 or else
              Labyrinth(Current_Pos.Y)(Current_Pos.X).Distance > Current_Distance then
               Labyrinth(Current_Pos.Y)(Current_Pos.X).Distance := Current_Distance;
            end if;
         end if;
         Delete(Input, 1, 1);
         Current_Char := Element(Input, 1);
      end loop;
   end Parse_Lineair_String;

   procedure Parse_String (Input     : in out Regex_String;
                           Labyrinth : in out Maze;
                           Root      : in Coordinate_Access;
                           Current_Pos : in Coordinate_Access) is
      First_Char : Character;
      Working_String : Regex_String;
      Intermediate_Pos : Coordinate_Access;
      New_Root : Coordinate_Access;
   begin
      First_Char := Element(Input, 1);
      Working_String := Delete(Input, 1, 1);

      if First_Char = '^' then
         --Put_Line("Starting");
         Parse_String(Input     => Working_String,
                      Labyrinth => Labyrinth,
                      Root    => Root,
                      Current_Pos => Current_Pos);
      elsif First_Char = '$' then
         --Put_Line("Finished");
         return;
      elsif First_Char = 'N' or First_Char = 'E'
        or First_Char = 'S' or First_Char = 'W' then
         Intermediate_Pos := Current_Pos;
         Parse_Lineair_String(Input       => Input,
                              Labyrinth   => Labyrinth,
                              Current_Pos => Intermediate_Pos);
         Parse_String(Input       => Input,
                      Labyrinth   => Labyrinth,
                      Root        => Root,
                      Current_Pos => Intermediate_Pos);
      elsif First_Char = '(' then
         --Put_Line("Starting branch");
         New_Root := new Coordinate'(X        => Current_Pos.X,
                                     Y        => Current_Pos.Y,
                                     Distance => -1,
                                     Root     => Root);
         Parse_String(Input       => Working_String,
                      Labyrinth   => Labyrinth,
                      Root        => New_Root,
                      Current_Pos => Current_Pos);
      elsif First_Char = '|' then
         --Put_Line("OR");
         Parse_String(Input       => Working_String,
                      Labyrinth   => Labyrinth,
                      Root        => Root,
                      Current_Pos => Root);
      elsif First_Char = ')' then
         --Put_Line("Finished branch");
         Parse_String(Input       => Working_String,
                      Labyrinth   => Labyrinth,
                      Root        => Root.Root,
                      Current_Pos => Root);
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

      Current_Pos.Root := Current_Pos;
      Result(Current_Pos.X)(Current_Pos.Y) := Current_Pos;

      Parse_String(Input       => Current_Regex,
                   Labyrinth   => Result,
                   Root        => Current_Pos,
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
                  Put("|" & "   ");
               end if;
            end if;
         end loop;
         Put_Line("|");
         Put_Line("--------------------------------------------------------------");
      end loop;
   end Print_Maze;

   Input_Regex : Regex_String;
   Input_Maze : Maze;
   Answer : Integer;
begin
   Load_File (Regex => Input_Regex);

   Input_Maze := Create_Maze(Input => Input_Regex);

   --Print_Maze(Input => Input_Maze);

   Answer := Find_Largest_Distance(Input => Input_Maze);

   Put_Line(Integer'Image(Answer));
   -- 1062 too low
   -- 3575 too high
end Day20_1;
