with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day09_2 is
   -- ### CONSTANTS ### --

   -- ### TYPE DEFINITIONS ### --
   type Coordinate is record
      Hgt : Integer;
      Size   : Integer;
   end record;

   package Height_Row is new Vectors(Index_Type   => Natural,
                                     Element_Type => Coordinate);
   use Height_Row;

   package Height_Field is new Vectors(Index_Type   => Natural,
                                       Element_Type => Height_Row.Vector);
   use Height_Field;

   procedure Load_File (Caves : out Height_Field.Vector) is
      Input        : File_Type;
      Current_Line : Unbounded_String;
      Current_Row  : Height_Row.Vector;
      Current_Coord : Coordinate;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day09.input");
      while not End_Of_File (Input) loop
         Current_Row  := Height_Row.Empty_Vector;
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Column_Loop:
         for C in 1 .. Length(Current_Line) loop
            Current_Coord := (Integer'Value((1 => Element(Current_Line, C))), 1);
            Current_Row.Append(Current_Coord);
         end loop Column_Loop;
         Caves.Append(Current_Row);
      end loop;
      Close (File => Input);
   end Load_File;

   function Flow(Caves   : in out Height_Field.Vector;
                 Row_Idx : in Natural;
                 Col_Idx : in Natural) return Boolean is
      Current_Height : Integer := Caves(Row_Idx)(Col_Idx).Hgt;
      North          : Integer := 0;
      South          : Integer := 0;
      West           : Integer := 0;
      East           : Integer := 0;
   begin
      if Caves(Row_Idx)(Col_Idx).Hgt = 9 then
         Caves(Row_Idx)(Col_Idx).Size := 0;
      end if;
      if Caves(Row_Idx)(Col_Idx).Size = 0 then
         return False;
      end if;

      if Row_Idx > Caves.First_Index then
         North := Caves(Row_Idx - 1)(Col_Idx).Hgt;
         if North < Current_Height then
            Caves(Row_Idx - 1)(Col_Idx).Size :=
              Caves(Row_Idx - 1)(Col_Idx).Size + Caves(Row_Idx)(Col_Idx).Size;
            Caves(Row_Idx)(Col_Idx).Size := 0;
            return True;
         end if;
      end if;
      if Col_Idx > Caves.First_Element.First_Index then
         West := Caves(Row_Idx)(Col_Idx - 1).Hgt;
         if West < Current_Height then
            Caves(Row_Idx)(Col_Idx - 1).Size :=
              Caves(Row_Idx)(Col_Idx-1).Size + Caves(Row_Idx)(Col_Idx).Size;
            Caves(Row_Idx)(Col_Idx).Size := 0;
            return True;
         end if;
      end if;
      if Row_Idx < Caves.Last_Index then
         South := Caves(Row_Idx + 1)(Col_Idx).Hgt;
         if South < Current_Height then
            Caves(Row_Idx + 1)(Col_Idx).Size :=
              Caves(Row_Idx + 1)(Col_Idx).Size + Caves(Row_Idx)(Col_Idx).Size;
            Caves(Row_Idx)(Col_Idx).Size := 0;
            return True;
         end if;
      end if;
      if Col_Idx < Caves.First_Element.Last_Index then
         East := Caves(Row_Idx)(Col_Idx + 1).Hgt;
         if East < Current_Height then
            Caves(Row_Idx)(Col_Idx + 1).Size :=
              Caves(Row_Idx)(Col_Idx + 1).Size + Caves(Row_Idx)(Col_Idx).Size;
            Caves(Row_Idx)(Col_Idx).Size := 0;
            return True;
         end if;
      end if;

      return False;
   end Flow;

   function Calculate_Answer(Caves : in Height_Field.Vector) return Integer is
      Biggest : Integer := 0;
      Second_Biggest : Integer := 0;
      Third_Biggest : Integer := 0;
   begin
      Row_Loop:
      for Row of Caves loop
         Col_Loop:
         for Col of Row loop
            if Col.Size > Third_Biggest then
               Third_Biggest := Col.Size;
            end if;
            if Col.Size > Second_Biggest then
               Third_Biggest := Second_Biggest;
               Second_Biggest := Col.Size;
            end if;
            if Col.Size > Biggest then
               Second_Biggest := Biggest;
               Biggest := Col.Size;
            end if;
         end loop Col_Loop;
      end loop Row_Loop;

      --  Put_Line(Integer'Image(Biggest)
      --           & Integer'Image(Second_Biggest)
      --           & Integer'Image(Third_Biggest));

      return Biggest * Second_Biggest * Third_Biggest;
   end Calculate_Answer;

   Caves  : Height_Field.Vector;
   Answer : Integer := 0;
   Flown : Boolean := True;
begin
   Load_File(Caves);

   Flow_Loop:
   while Flown loop
      Flown := False;
      Row_Loop:
      for Row_Index in Caves.First_Index .. Caves.Last_Index loop
         Column_Loop:
         for Col_Index in Caves(Row_Index).First_Index .. Caves(Row_Index).Last_Index loop
            Flown := Flown or Flow(Caves, Row_Index, Col_Index);
         end loop Column_Loop;
      end loop Row_Loop;
   end loop Flow_Loop;

   Answer := Calculate_Answer(Caves);

   Put_line("The product of the three largest basins is "
            & Integer'Image(Answer) & ".");
end Day09_2;
