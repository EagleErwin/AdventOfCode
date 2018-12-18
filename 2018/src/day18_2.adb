with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
procedure Day18_2 is
   Field_Dimension : constant Integer := 50;
   Number_Of_Minutes : constant Integer := 1_000_000_000;
   Expected_Maximum_Loop_Period : constant Integer := 100;

   subtype Data_Line is String (1 .. Field_Dimension);

   subtype Coordinate is Integer range 1 .. Field_Dimension;

   type Acre_State is (Open, Tree, Lumberyard);

   type Field_Row is array (1 .. Field_Dimension) of Acre_State;
   type Field is array (1 .. Field_Dimension) of Field_Row;

   type Field_Array is array (1 .. Expected_Maximum_Loop_Period) of Field;

   function Char_To_State (Input_Character : in Character) return Acre_State is
   begin
      if Input_Character = '.' then
         return Open;
      elsif Input_Character = '|' then
         return Tree;
      elsif Input_Character = '#' then
         return Lumberyard;
      else
         Put_Line("[ERROR] Invalid character:"
                  & Character'Image(Input_Character));
         return Open;
      end if;
   end Char_To_State;

   function Acre_To_Char (Input_Acre : in Acre_State) return Character is
   begin
      case Input_Acre is
         when Open => return '.';
         when Tree => return '|';
         when Lumberyard => return '#';
      end case;
   end Acre_To_Char;

   procedure Load_File (Input_Field : out Field) is
      Input         : File_Type;
      Current_Line  : Data_Line;
      Current_Line_Number : Integer := 0;
      Current_Acre_Char : Character;
   begin
      Input_Field := (others => (others => Open));
      Open (File => Input, Mode => In_File, Name => "data/day18.input");

      Row_Loop:
      while not End_Of_File (Input) loop
         Current_Line_Number := Current_Line_Number + 1;
         Current_Line  := Get_Line(File => Input);
         Acre_Loop:
         for I in Current_Line'Range loop
            Current_Acre_Char := Current_Line(I);
            Input_Field(Current_Line_Number)(I) := Char_To_State(Current_Acre_Char);
         end loop Acre_Loop;
      end loop Row_Loop;

      Close (File => Input);
   end Load_File;

   procedure Print_Field(Field_To_Print : in Field) is
   begin
      for R in Field_To_Print'Range loop
         for C in Field_To_Print(R)'Range loop
            Put(Acre_To_Char(Field_To_Print(R)(C)));
         end loop;
         Put_Line("");
      end loop;
      Put_Line("");
   end Print_Field;

   function Get_Neighbours (Desired_State : in Acre_State;
                            X : in Coordinate; Y : in Coordinate;
                            Current_Field : in Field) return Integer is
      Begin_X : Coordinate;
      End_X   : Coordinate;
      Begin_Y : Coordinate;
      End_Y   : Coordinate;

      Current_Acre      : Acre_State;
      Nr_Of_Neighbours : Integer := 0;
   begin
      Begin_X := Integer'Max(X - 1, Coordinate'First);
      End_X := Integer'Min(X + 1, Coordinate'Last);
      Begin_Y := Integer'Max(Y - 1, Coordinate'First);
      End_Y := Integer'Min(Y + 1, Coordinate'Last);
      for I in Begin_X .. End_X loop
         for J in Begin_Y .. End_Y loop
            if I /= X or J /= Y then
               Current_Acre := Current_Field(J)(I);
               if Current_Acre = Desired_State then
                  Nr_Of_Neighbours := Nr_Of_Neighbours + 1;
               end if;
            end if;
         end loop;
      end loop;
      return Nr_Of_Neighbours;
   end Get_Neighbours;

   procedure Evolve (Current_Field : in out Field) is
      Current_Acre          : Acre_State;
      Neighbour_Trees       : Integer;
      Neighbour_Lumberyards : Integer;
      New_Field             : Field;
   begin
      for R in Current_Field'Range loop
         for C in Current_Field(R)'Range loop
            Current_Acre := Current_Field(R)(C);
            Neighbour_Trees := Get_Neighbours(Desired_State => Tree,
                                              X => C, Y => R,
                                              Current_Field => Current_Field);
            Neighbour_Lumberyards := Get_Neighbours(Desired_State => Lumberyard,
                                                    X => C, Y => R,
                                                    Current_Field => Current_Field);

            -- Rule 1
            if Current_Acre = Open then
               if Neighbour_Trees >= 3 then
                  New_Field(R)(C) := Tree;
               else
                  New_Field(R)(C) := Open;
               end if;
            end if;

            -- Rule 2
            if Current_Acre = Tree then
               if Neighbour_Lumberyards >= 3 then
                  New_Field(R)(C) := Lumberyard;
               else
                  New_Field(R)(C) := Tree;
               end if;
            end if;

            -- Rule 3
            if Current_Acre = Lumberyard then
               if Neighbour_Lumberyards = 0 or Neighbour_Trees = 0 then
                  New_Field(R)(C) := Open;
               else
                  New_Field(R)(C) := Lumberyard;
               end if;
            end if;

         end loop;
      end loop;
      Current_Field := New_Field;
   end Evolve;

   function Get_Number_Of(Desired_State : in Acre_State; Current_Field : in Field) return Integer is
      Result : Integer := 0;
   begin
      for R in Current_Field'Range loop
         for C in Current_Field(R)'Range loop
            if Current_Field(R)(C) = Desired_State then
               Result := Result + 1;
            end if;
         end loop;
      end loop;
      return Result;
   end Get_Number_Of;

   function Check_Repetition(Current_Field : in Field;
                             Base : in out Field;
                             Number_Of_Iterations : in out Integer) return Integer is
   begin
      Number_Of_Iterations := Number_Of_Iterations + 1;
         for R in Base'Range loop
            for C in Base(R)'Range loop
               if Base(R)(C) /= Current_Field(R)(C) then
                  return -1; -- No duplicate
               end if;
            end loop;
         end loop;
      return Number_Of_Iterations;
   end Check_Repetition;

   Input_Field           : Field;
   Number_Of_Trees       : Integer;
   Number_Of_Lumberyards : Integer;
   Thousand : Field;
   Iterations : Integer := 0;
   Repetion_Interval : Integer;
   Tick : Integer;
begin
   Load_File (Input_Field => Input_Field);

   Evolution_Loop:
   for I in 1 .. Number_Of_Minutes loop
      Tick := I;
      Evolve(Current_Field => Input_Field);
      if I = 1000 then
         Thousand := Input_Field;
      end if;
      if I > 1000 then
         Repetion_Interval := Check_Repetition(Current_Field => Input_Field,
                                               Base => Thousand,
                                               Number_Of_Iterations => Iterations);
         if Repetion_Interval /= -1 then
            Put_Line("Repetition detected with interval" & Integer'Image(Repetion_Interval));
            exit Evolution_Loop;
         end if;
      end if;
   end loop Evolution_Loop;

   -- Evolve the correct number of iterations to get the correct answer.
   for I in 1 .. (Number_Of_Minutes - Tick) mod Repetion_Interval loop
      Evolve(Current_Field => Input_Field);
   end loop;

   Number_Of_Trees := Get_Number_Of(Desired_State => Tree, Current_Field => Input_Field);
   Number_Of_Lumberyards := Get_Number_Of(Desired_State => Lumberyard, Current_Field => Input_Field);

   --Print_Field(Field_To_Print => Input_Field);

   Put_Line("Answer:" & Integer'Image(Number_Of_Trees * Number_Of_Lumberyards));
end Day18_2;
