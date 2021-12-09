with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day09_1 is
   -- ### CONSTANTS ### --

   -- ### TYPE DEFINITIONS ### --
   package Height_Row is new Vectors(Index_Type   => Natural,
                                     Element_Type => Natural);
   use Height_Row;

   package Height_Field is new Vectors(Index_Type   => Natural,
                                       Element_Type => Height_Row.Vector);
   use Height_Field;

   procedure Load_File (Caves : out Height_Field.Vector) is
      Input        : File_Type;
      Current_Line : Unbounded_String;
      Current_Row  : Height_Row.Vector;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day09.input");
      while not End_Of_File (Input) loop
         Current_Row  := Height_Row.Empty_Vector;
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Column_Loop:
         for C in 1 .. Length(Current_Line) loop
            Current_Row.Append(Integer'Value((1 => Element(Current_Line, C))));
         end loop Column_Loop;
         Caves.Append(Current_Row);
      end loop;
      Close (File => Input);
   end Load_File;

   function Is_Low_Point(Caves   : in Height_Field.Vector;
                         Row_Idx : in Natural;
                         Col_Idx : in Natural) return Boolean is
      Current_Height : Integer := Caves(Row_Idx)(Col_Idx);
      North          : Integer := 0;
      South          : Integer := 0;
      West           : Integer := 0;
      East           : Integer := 0;

      Result : Boolean := True;
   begin
      if Row_Idx > Caves.First_Index then
         North := Caves(Row_Idx - 1)(Col_Idx);
         Result := Result and (North > Current_Height);
      end if;
      if Col_Idx > Caves.First_Element.First_Index then
         West := Caves(Row_Idx)(Col_Idx - 1);
         Result := Result and (West > Current_Height);
      end if;
      if Row_Idx < Caves.Last_Index then
         South := Caves(Row_Idx + 1)(Col_Idx);
         Result := Result and (South > Current_Height);
      end if;
      if Col_Idx < Caves.First_Element.Last_Index then
         East := Caves(Row_Idx)(Col_Idx + 1);
         Result := Result and (East > Current_Height);
      end if;

      return Result;
   end Is_Low_Point;

   function Calculate_Risk_Level(Caves   : in Height_Field.Vector;
                                 Row_Idx : in Natural;
                                 Col_Idx : in Natural) return Integer is
   begin
      return 1 + Caves(Row_Idx)(Col_Idx);
   end Calculate_Risk_Level;

   Caves  : Height_Field.Vector;
   Answer : Integer := 0;
begin
   Load_File(Caves);

   Row_Loop:
   for Row_Index in Caves.First_Index .. Caves.Last_Index loop
      Column_Loop:
      for Col_Index in Caves(Row_Index).First_Index .. Caves(Row_Index).Last_Index loop
         if (Is_Low_Point(Caves, Row_Index, Col_Index)) then
            Answer := Answer + Calculate_Risk_Level(Caves, Row_Index, Col_Index);
         end if;
      end loop Column_Loop;
   end loop Row_Loop;

   Put_line("The sum of the risk levels of all low points on the heightmap is "
            & Integer'Image(Answer) & ".");
end Day09_1;
