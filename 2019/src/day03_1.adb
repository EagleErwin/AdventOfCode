with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day03_1 is
   -- ### CONSTANTS ### --
   Wire_Data_Length : constant Integer := 300; -- Number of corners in the wire data (estimation)

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Wire_Direction is (U, D, L, R);
   subtype Wire_Range is Integer;

   type Wire_Data is record
      Direction : Wire_Direction;
      Length    : Wire_Range;
   end record;

   type Wire is array (0 .. Wire_Data_Length) of Wire_Data;

   type Coordinate is record
      X : Integer;
      Y : Integer;
   end record;
   type Coordinate_Array is array (1 .. 100_000) of Coordinate;

   -- ### GLOBAL VARIABLES ## --
   Occupations : Coordinate_Array;
   Nr_Of_Occupations : Integer := 0;
   Intersections : Coordinate_Array;
   Nr_Of_Intersections : Integer := 0;

   function Decode_Wire_String(Encoded_Wire_String : in Unbounded_String) return Wire_Data is
      Result : Wire_Data;
      Direction : Wire_Direction;
      Wire_Length : Wire_Range;
   begin
      Direction := Wire_Direction'Value("" & Element(Encoded_Wire_String, 1));
      Wire_Length := Integer'Value(Slice(Encoded_Wire_String, 2, Length(Encoded_Wire_String)));
      Result := Wire_Data'(Direction => Direction, Length => Wire_Length);
      return Result;
   end Decode_Wire_String;

   procedure Load_File (Wire_1 : out Wire; Wire_2 : out Wire) is
      Input       : File_Type;
      First_Line  : Data_Line;
      Second_Line : Data_Line;
      Idx         : Integer := 0;
      Curr_Char   : Character;
      Buffer      : Unbounded_String;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day03.input");
      First_Line := To_Unbounded_String(Get_Line(Input));
      Second_Line := To_Unbounded_String(Get_Line(Input));
      Close (File => Input);

      for C in 1 .. Length(First_Line) loop
         Curr_Char := Element(First_Line, C);
         if Curr_Char = ',' then
            Wire_1(Idx) := Decode_Wire_String(Buffer);
            -- Reset state
            Buffer := To_Unbounded_String("");
            Idx := Idx + 1;
         else
            Buffer := Buffer & Curr_Char;
         end if;
      end loop;
      Wire_1(Idx) := Decode_Wire_String(Buffer);

      Buffer := To_Unbounded_String("");
      Idx := 0;
      for C in 1 .. Length(Second_Line) loop
         Curr_Char := Element(Second_Line, C);
         if Curr_Char = ',' then
            Wire_2(Idx) := Decode_Wire_String(Buffer);
            -- Reset state
            Buffer := To_Unbounded_String("");
            Idx := Idx + 1;
         else
            Buffer := Buffer & Curr_Char;
         end if;
      end loop;
      Wire_2(Idx) := Decode_Wire_String(Buffer);
   end Load_File;

   procedure Fill_Up(Wire_Nr : in Integer;
                     X : in Integer;
                     Y : in out Integer;
                     Length : in Wire_Range) is
   begin
      for I in Y .. Y + Length loop
         if (Wire_Nr = 1) then -- Fill the Occupations administration
            Nr_Of_Occupations := Nr_Of_Occupations + 1;
            Occupations(Nr_Of_Occupations) := Coordinate'(X => X, Y => I);
         else -- Add to the Intersections administration if needed
            for O in 1 .. Nr_Of_Occupations loop
               if (X = Occupations(O).X and I = Occupations(O).Y) then
                  Nr_Of_Intersections := Nr_Of_Intersections + 1;
                  Intersections(Nr_Of_Intersections) := Occupations(O);
               end if;
            end loop;
         end if;
      end loop;
      Y := Y + Length;
   end Fill_Up;

   procedure Fill_Down(Wire_Nr : in Integer;
                     X : in Integer;
                     Y : in out Integer;
                     Length : in Wire_Range) is
   begin
      for I in Y .. Y - Length loop
         if (Wire_Nr = 1) then -- Fill the Occupations administration
            Nr_Of_Occupations := Nr_Of_Occupations + 1;
            Occupations(Nr_Of_Occupations) := Coordinate'(X => X, Y => I);
         else -- Add to the Intersections administration if needed
            for O in 1 .. Nr_Of_Occupations loop
               if (X = Occupations(O).X and I = Occupations(O).Y) then
                  Nr_Of_Intersections := Nr_Of_Intersections + 1;
                  Intersections(Nr_Of_Intersections) := Occupations(O);
               end if;
            end loop;
         end if;
      end loop;
      Y := Y - Length;
   end Fill_Down;

   procedure Fill_Left(Wire_Nr : in Integer;
                     X : in out Integer;
                     Y : in Integer;
                     Length : in Wire_Range) is
   begin
      for I in X .. X - Length loop
         if (Wire_Nr = 1) then -- Fill the Occupations administration
            Nr_Of_Occupations := Nr_Of_Occupations + 1;
            Occupations(Nr_Of_Occupations) := Coordinate'(X => I, Y => Y);
         else -- Add to the Intersections administration if needed
            for O in 1 .. Nr_Of_Occupations loop
               if (I = Occupations(O).X and Y = Occupations(O).Y) then
                  Nr_Of_Intersections := Nr_Of_Intersections + 1;
                  Intersections(Nr_Of_Intersections) := Occupations(O);
               end if;
            end loop;
         end if;
      end loop;
      X := X - Length;
   end Fill_Left;

   procedure Fill_Right(Wire_Nr : in Integer;
                     X : in out Integer;
                     Y : in Integer;
                     Length : in Wire_Range) is
   begin
      for I in X .. X + Length loop
         if (Wire_Nr = 1) then -- Fill the Occupations administration
            Nr_Of_Occupations := Nr_Of_Occupations + 1;
            Occupations(Nr_Of_Occupations) := Coordinate'(X => I, Y => Y);
         else -- Add to the Intersections administration if needed
            for O in 1 .. Nr_Of_Occupations loop
               if (I = Occupations(O).X and Y = Occupations(O).Y) then
                  Nr_Of_Intersections := Nr_Of_Intersections + 1;
                  Intersections(Nr_Of_Intersections) := Occupations(O);
               end if;
            end loop;
         end if;
      end loop;
      X := X + Length;
   end Fill_Right;

   function Solve(Wire_1 : in Wire; Wire_2 : in Wire) return Integer is
      Curr_X : Integer := 0;
      Curr_Y : Integer := 0;

      Distance : Integer;
      Min_Distance : Integer := 1_000_000; -- Very high initial value
      Intersection : Coordinate;
   begin
      for I in Wire_1'Range loop
         case Wire_1(I).Direction is
            when U => Fill_Up(1, Curr_X, Curr_Y, Wire_1(I).Length);
            when D => Fill_Down(1, Curr_X, Curr_Y, Wire_1(I).Length);
            when L => Fill_Left(1, Curr_X, Curr_Y, Wire_1(I).Length);
            when R => Fill_Right(1, Curr_X, Curr_Y, Wire_1(I).Length);
         end case;
      end loop;

      Curr_X := 0;
      Curr_Y := 0;
      for I in Wire_2'Range loop
         case Wire_2(I).Direction is
            when U => Fill_Up(2, Curr_X, Curr_Y, Wire_2(I).Length);
            when D => Fill_Down(2, Curr_X, Curr_Y, Wire_2(I).Length);
            when L => Fill_Left(2, Curr_X, Curr_Y, Wire_2(I).Length);
            when R => Fill_Right(2, Curr_X, Curr_Y, Wire_2(I).Length);
         end case;
      end loop;

      for I in 1 .. Nr_Of_Intersections loop
         Intersection := Intersections(I);
         --Put_Line("Intersection found at (" & Integer'Image(Intersection.X) & "," & Integer'Image(Intersection.Y) & ")");
         Distance := Intersection.X + Intersection.Y;
         if Distance < Min_Distance then
            Min_Distance := Distance;
         end if;
      end loop;

      return Min_Distance;
   end Solve;

   Red_Wire : Wire;
   Blue_Wire : Wire;
   Answer : Integer;
begin
   Load_File(Wire_1 => Red_Wire, Wire_2 => Blue_Wire);
   Answer := Solve(Wire_1 => Red_Wire, Wire_2 => Blue_Wire);
   Put_Line ("The closest intersection is at Manhattan distance" & Integer'Image(Answer));
end Day03_1;
