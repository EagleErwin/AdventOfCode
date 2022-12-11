with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day09_2 is

   -- ## CONSTANTS ## --
   Number_Of_Instructions : constant Integer := 2_000;
   Grid_Dimension : constant Integer := 1_000; -- Positive and negative
   Number_Of_Knots : constant Integer := 10;

   -- ## TYPES ## --
   type Compass is (Up, Down, Left, Right);

   type Movement is record
      Direction : Compass;
      Amount  : Integer;
   end record;

   type Movement_Array is array (1 .. Number_Of_Instructions) of Movement;

   type Position is record
      Row : Integer;
      Col : Integer;
   end record;

   type Rope is array (1 .. Number_Of_Knots) of Position;

   type Grid_Value is record
      Visited : Boolean;
   end record;

   type Grid_Row is array (-Grid_Dimension .. Grid_Dimension) of Grid_Value;
   type Grid is array (-Grid_Dimension .. Grid_Dimension) of Grid_Row;

   function To_Movement (Input : in Unbounded_String) return Movement is
      Result : Movement;

      Direction_Char : Character;
      Movement_Amount : Integer;
   begin
      Direction_Char := Element(Input, 1);
      case Direction_Char is
         when 'U' => Result.Direction := Up;
         when 'D' => Result.Direction := Down;
         when 'L' => Result.Direction := Left;
         when 'R' => Result.Direction := Right;
         when others => Put_Line("ERROR! Invalid direction character '"
                                 & Direction_Char & "'.");
      end case;

      Movement_Amount := Integer'Value(Slice(Input, 3, Length(Input)));
      Result.Amount := Movement_Amount;

      return Result;
   end To_Movement;

   procedure Load_File (Movements : out Movement_Array) is
      Input          : File_Type;
      Current_Line   : Unbounded_String;

      Movement_Index : Integer := 0;
   begin
      Movements := (others => (Up, 0));
      Open (File => Input, Mode => In_File, Name => "data/day09.input");
      Read_Line_Loop:
      while not End_Of_File (Input) loop
         Movement_Index := Movement_Index + 1;
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Movements(Movement_Index) := To_Movement(Current_Line);
      end loop Read_Line_Loop;
      Close (File => Input);
   end Load_File;

   procedure Update_Knot (Field : in out Grid;
                          Head_Pos : in Position;
                          Tail_Pos : in out Position) is
   begin
      if (Head_Pos.Row - Tail_Pos.Row = 0) then
         -- We are on the same row
         if (Head_Pos.Col - Tail_Pos.Col = 2) then
            -- We move to the right: [T . H] => [. T H]
            Tail_Pos.Col := Tail_Pos.Col + 1;
         elsif (Head_Pos.Col - Tail_Pos.Col = -2) then
            -- We move to the left: [H . T] => [H T .]
            Tail_Pos.Col := Tail_Pos.Col - 1;
         --else
            -- Nothing happens with the tail.
         end if;
      elsif (Head_Pos.Col - Tail_Pos.Col = 0) then
         -- We are on the same col
         if (Head_Pos.Row - Tail_Pos.Row = 2) then
            -- We move up:
            -- |H|    |H|
            -- |.| => |T|
            -- |T|    |.|
            Tail_Pos.Row := Tail_Pos.Row + 1;
         elsif (Head_Pos.Row - Tail_Pos.Row = -2) then
            -- We move down:
            -- |T|    |.|
            -- |.| => |T|
            -- |H|    |H|
            Tail_Pos.Row := Tail_Pos.Row - 1;
         --else
            -- Nothing happens with the tail.
         end if;
      elsif (abs (Head_Pos.Row - Tail_Pos.Row) > 1) then
         -- We are not touching, so move diagonally.
         if (abs (Head_Pos.Col - Tail_Pos.Col) > 1) then
            -- Diagonal in both directions.
            if (Head_Pos.Col > Tail_Pos.Col) then
               Tail_Pos.Col := Tail_Pos.Col + 1;
            else
               Tail_Pos.Col:= Tail_Pos.Col - 1;
            end if;
         else
            -- Same as old diagonal. Take over col from head.
            Tail_Pos.Col := Head_Pos.Col;
         end if;
         if (Head_Pos.Row > Tail_Pos.Row) then
            Tail_Pos.Row := Tail_Pos.Row + 1;
         else
            Tail_Pos.Row:= Tail_pos.Row - 1;
         end if;
      elsif (abs (Head_Pos.Col - Tail_Pos.Col) > 1) then
         -- We are not touching, so move diagonally.
         if (abs (Head_Pos.Row - Tail_Pos.Row) > 1) then
            if (Head_Pos.Row > Tail_Pos.Row) then
               Tail_Pos.Row := Tail_Pos.Row + 1;
            else
               Tail_Pos.Row:= Tail_pos.Row - 1;
            end if;
         else
            -- Same as old diagonal. Take over row from head.
            Tail_Pos.Row := Head_Pos.Row;
         end if;

         if (Head_Pos.Col > Tail_Pos.Col) then
            Tail_Pos.Col := Tail_Pos.Col + 1;
         else
            Tail_Pos.Col:= Tail_Pos.Col - 1;
         end if;
      end if;
   end Update_Knot;

   procedure Update_Tail (Field          : in out Grid;
                          Rope_Positions : in out Rope) is
      Knot_1_Pos : Position;
      Knot_2_Pos : Position;
   begin
      Tail_Loop:
      for J in 1 .. Rope_Positions'Length - 1 loop
         Knot_1_Pos := Rope_Positions(J);
         Knot_2_Pos := Rope_Positions(J + 1);
         Update_Knot(Field, Knot_1_Pos, Knot_2_Pos);
         Rope_Positions(J + 1) := Knot_2_Pos;
      end loop Tail_Loop;
      Field(Rope_Positions(Number_Of_Knots).Row)(Rope_Positions(Number_Of_Knots).Col).Visited := True;
   end Update_Tail;

   procedure Single_Move (Instruction    : in Movement;
                          Field          : in out Grid;
                          Rope_Positions : in out Rope) is
   begin
      case Instruction.Direction is
         when Up    =>
            Move_Up_Loop:
            for I in 1 .. Instruction.Amount loop
               Rope_Positions(1).Row := Rope_Positions(1).Row + 1;
               Update_Tail(Field, Rope_Positions);
            end loop Move_Up_Loop;
         when Down  =>
            Move_Down_Loop:
            for I in 1 .. Instruction.Amount loop
               Rope_Positions(1).Row := Rope_Positions(1).Row - 1;
               Update_Tail(Field, Rope_Positions);
            end loop Move_Down_Loop;
         when Left  =>
            Move_Left_Loop:
            for I in 1 .. Instruction.Amount loop
               Rope_Positions(1).Col := Rope_Positions(1).Col - 1;
               Update_Tail(Field, Rope_Positions);
            end loop Move_Left_Loop;
         when Right =>
            Move_Right_Loop:
            for I in 1 .. Instruction.Amount loop
               Rope_Positions(1).Col := Rope_Positions(1).Col + 1;
               Update_Tail(Field, Rope_Positions);
            end loop Move_Right_Loop;
      end case;
   end Single_Move;

   procedure Move (Movements : in Movement_Array;
                   Field     : in out Grid) is
      Rope_Positions : Rope := (others => (0, 0));

      Current_Move : Movement;
   begin
      Field(Rope_Positions(Number_Of_Knots).Row)(Rope_Positions(Number_Of_Knots).Col).Visited := True;
      Move_Loop:
      for M in Movements'Range loop
         Current_Move := Movements(M);
         Single_Move(Current_Move, Field, Rope_Positions);
      end loop Move_Loop;
   end Move;

   function Count_Tail_Visits (Field : in Grid) return Integer is
      Result : Integer := 0;
   begin
      Row_Loop:
      for R in Grid'Range loop
         Col_Loop:
         for C in Grid_Row'Range loop
            if Field(R)(C).Visited then
               Result := Result + 1;
            end if;
         end loop Col_Loop;
      end loop Row_Loop;

      return Result;
   end Count_Tail_Visits;

   procedure Print_Grid (Input : in Grid) is
   begin
      Row_Loop:
      for R in Grid'Range loop
         Col_Loop:
         for C in Grid_Row'Range loop
            if Input(R)(C).Visited then
               Put("#");
            else
               Put(".");
            end if;
         end loop Col_Loop;
         Put_Line("");
      end loop Row_Loop;
   end Print_Grid;

   Instructions  : Movement_Array;
   Movement_Grid : Grid := (others => (others => (Visited => False)));

   Answer : Integer := 0;
begin
   Load_File(Instructions);

   Move(Instructions, Movement_Grid);

   --Print_Grid(Movement_Grid);
   Answer := Count_Tail_Visits(Movement_Grid);

   Put_Line("The tail of the rope visits a total of" & Integer'Image(Answer)
            & " positions.");
end Day09_2;
