with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day17_1 is
   -- ### CONSTANTS ### --
   Enable_Debug : constant Boolean := False;

   Part_1 : Integer := 0;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Coordinate_Array is array (1 .. 3) of Integer;

   package Append_Vector is new Vectors(Index_Type   => Natural,
                                        Element_Type => Coordinate_Array);
   use Append_Vector;

   function Natural_Hash (id : Integer) return Hash_Type is
   begin
      -- I don't know how to calculate a Hash that makes more sense.
      return Hash(Integer'Image(id));
   end Natural_Hash;

   -- In a row, the key is the X index, the value the active state
   package Cube_Row is new Hashed_Maps(Key_Type        => Integer,
                                       Element_Type    => Boolean,
                                       Hash            => Natural_Hash,
                                       Equivalent_Keys => "=" );
   use Cube_Row;

   -- In a plane, the key is the X index, the value the row
   package Cube_Plane is new Hashed_Maps(Key_Type      => Integer,
                                         Element_Type    => Cube_Row.Map,
                                         Hash            => Natural_Hash,
                                         Equivalent_Keys => "=" );
   use Cube_Plane;

   -- In a space, the key is the Z index, the value the plane
   package Cube_Space is new Hashed_Maps(Key_Type        => Integer,
                                         Element_Type    => Cube_Plane.Map,
                                         Hash            => Natural_Hash,
                                         Equivalent_Keys => "=" );
   use Cube_Space;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   procedure Load_File (Cubes : out Cube_Space.Map) is
      Initial_Z    : constant Integer := 0;
      Input        : File_Type;
      Current_Line : Data_Line;
      Row_Index    : Integer := 0;

      Curr_Char    : Character;
   begin
      Cubes.Insert(Initial_Z, Cube_Plane.Empty_Map);
      Open (File => Input, Mode => In_File, Name => "data/day17.input");
      Read_Loop:
      while not End_Of_File (Input) loop
         Cubes(Initial_Z).Insert(Row_Index, Cube_Row.Empty_Map);
         Current_Line := To_Unbounded_String(Get_Line(Input));

         Parser_Loop:
         for I in 1 .. Length(Current_Line) loop
            Curr_Char := Element(Current_Line, I);
            if Curr_Char = '.' then
               Cubes(Initial_Z)(Row_Index).Insert(I, False);
            else
               Cubes(Initial_Z)(Row_Index).Insert(I, True);
            end if;
         end loop Parser_Loop;
         Row_Index := Row_Index + 1;
      end loop Read_Loop;
      Close (File => Input);
   end Load_File;

   function Count_Cubes(Cubes : in Cube_Space.Map) return Integer is
      Result : Integer := 0;
   begin
      Plane_Loop:
      for P in Cubes.Iterate loop
         Row_Loop:
         for R in Cubes(P).Iterate loop
            Cube_Loop:
            for C in Cubes(P)(R).Iterate loop
               if Cubes(P)(R)(C) then
                  Result := Result + 1;
               end if;
            end loop Cube_Loop;
         end loop Row_Loop;
      end loop Plane_Loop;
      return Result;
   end Count_Cubes;

   -- Add an inactive Cube around the cube provided by the position.
   -- Does not overwrite an existing cube
   procedure Surround(Cubes : in out Cube_Space.Map;
                      Coordinate : in Coordinate_Array) is
      Plane : Integer := Coordinate(1);
      Row   : Integer := Coordinate(2);
      Cube  : Integer := Coordinate(3);
   begin
      -- Append inactive cubes on the same row
      if Cubes(Plane)(Row).Find(Cube - 1) = Cube_Row.No_Element then
         Cubes(Plane)(Row).Insert(Cube - 1, False);
      end if;
      if Cubes(Plane)(Row).Find(Cube + 1) = Cube_Row.No_Element then
         Cubes(Plane)(Row).Insert(Cube + 1, False);
      end if;

      -- Append inactive cubes on the same plane
      -- Row above
      if Cubes(Plane).Find(Row - 1) = Cube_Plane.No_Element then
         -- The full row above it is missing.
         Cubes(Plane).Insert(Row - 1, Cube_Row.Empty_Map);
      end if;
      -- Add missing elements in row above it
      if Cubes(Plane)(Row - 1).Find(Cube - 1) = Cube_Row.No_Element then
         Cubes(Plane)(Row - 1).Insert(Cube - 1, False);
      end if;
      if Cubes(Plane)(Row - 1).Find(Cube) = Cube_Row.No_Element then
         Cubes(Plane)(Row - 1).Insert(Cube, False);
      end if;
      if Cubes(Plane)(Row - 1).Find(Cube + 1) = Cube_Row.No_Element then
         Cubes(Plane)(Row - 1).Insert(Cube + 1, False);
      end if;
      -- Row below
      if Cubes(Plane).Find(Row + 1) = Cube_Plane.No_Element then
         -- The full row below it is missing.
         Cubes(Plane).Insert(Row + 1, Cube_Row.Empty_Map);
      end if;
      -- Add missing elements in row above it
      if Cubes(Plane)(Row + 1).Find(Cube - 1) = Cube_Row.No_Element then
         Cubes(Plane)(Row + 1).Insert(Cube - 1, False);
      end if;
      if Cubes(Plane)(Row + 1).Find(Cube) = Cube_Row.No_Element then
         Cubes(Plane)(Row + 1).Insert(Cube, False);
      end if;
      if Cubes(Plane)(Row + 1).Find(Cube + 1) = Cube_Row.No_Element then
         Cubes(Plane)(Row + 1).Insert(Cube + 1, False);
      end if;

      -- Append inactive cubes on a different plane
      -- Plane above
      if Cubes.Find(Plane-1) = Cube_Space.No_Element then
         -- The full plane above it is missing.
         Cubes.Insert(Plane - 1, Cube_Plane.Empty_Map);
      end if;
      -- Row above
      if Cubes(Plane - 1).Find(Row - 1) = Cube_Plane.No_Element then
         -- The full row above it is missing.
         Cubes(Plane - 1).Insert(Row - 1, Cube_Row.Empty_Map);
      end if;
      -- Add missing elements in row above it
      if Cubes(Plane - 1)(Row - 1).Find(Cube - 1) = Cube_Row.No_Element then
         Cubes(Plane - 1)(Row - 1).Insert(Cube - 1, False);
      end if;
      if Cubes(Plane - 1)(Row - 1).Find(Cube) = Cube_Row.No_Element then
         Cubes(Plane - 1)(Row - 1).Insert(Cube, False);
      end if;
      if Cubes(Plane - 1)(Row - 1).Find(Cube + 1) = Cube_Row.No_Element then
         Cubes(Plane - 1)(Row - 1).Insert(Cube + 1, False);
      end if;
      -- Same row
      if Cubes(Plane - 1).Find(Row) = Cube_Plane.No_Element then
         -- The full row above it is missing.
         Cubes(Plane - 1).Insert(Row, Cube_Row.Empty_Map);
      end if;
      -- Add missing elements in row above it
      if Cubes(Plane - 1)(Row).Find(Cube - 1) = Cube_Row.No_Element then
         Cubes(Plane - 1)(Row).Insert(Cube - 1, False);
      end if;
      if Cubes(Plane - 1)(Row).Find(Cube) = Cube_Row.No_Element then
         Cubes(Plane - 1)(Row).Insert(Cube, False);
      end if;
      if Cubes(Plane - 1)(Row).Find(Cube + 1) = Cube_Row.No_Element then
         Cubes(Plane - 1)(Row).Insert(Cube + 1, False);
      end if;
      -- Row below
      if Cubes(Plane - 1).Find(Row + 1) = Cube_Plane.No_Element then
         -- The full row below it is missing.
         Cubes(Plane - 1).Insert(Row + 1, Cube_Row.Empty_Map);
      end if;
      -- Add missing elements in row above it
      if Cubes(Plane - 1)(Row + 1).Find(Cube - 1) = Cube_Row.No_Element then
         Cubes(Plane - 1)(Row + 1).Insert(Cube - 1, False);
      end if;
      if Cubes(Plane - 1)(Row + 1).Find(Cube) = Cube_Row.No_Element then
         Cubes(Plane - 1)(Row + 1).Insert(Cube, False);
      end if;
      if Cubes(Plane - 1)(Row + 1).Find(Cube + 1) = Cube_Row.No_Element then
         Cubes(Plane - 1)(Row + 1).Insert(Cube + 1, False);
      end if;

      -- Plane below
      if Cubes.Find(Plane + 1) = Cube_Space.No_Element then
         -- The full plane above it is missing.
         Cubes.Insert(Plane + 1, Cube_Plane.Empty_Map);
      end if;
      -- Row above
      if Cubes(Plane + 1).Find(Row - 1) = Cube_Plane.No_Element then
         -- The full row above it is missing.
         Cubes(Plane + 1).Insert(Row - 1, Cube_Row.Empty_Map);
      end if;
      -- Add missing elements in row above it
      if Cubes(Plane + 1)(Row - 1).Find(Cube - 1) = Cube_Row.No_Element then
         Cubes(Plane + 1)(Row - 1).Insert(Cube - 1, False);
      end if;
      if Cubes(Plane + 1)(Row - 1).Find(Cube) = Cube_Row.No_Element then
         Cubes(Plane + 1)(Row - 1).Insert(Cube, False);
      end if;
      if Cubes(Plane + 1)(Row - 1).Find(Cube + 1) = Cube_Row.No_Element then
         Cubes(Plane + 1)(Row - 1).Insert(Cube + 1, False);
      end if;
      -- Same row
      if Cubes(Plane + 1).Find(Row) = Cube_Plane.No_Element then
         -- The full row above it is missing.
         Cubes(Plane + 1).Insert(Row, Cube_Row.Empty_Map);
      end if;
      -- Add missing elements in row above it
      if Cubes(Plane + 1)(Row).Find(Cube - 1) = Cube_Row.No_Element then
         Cubes(Plane + 1)(Row).Insert(Cube - 1, False);
      end if;
      if Cubes(Plane + 1)(Row).Find(Cube) = Cube_Row.No_Element then
         Cubes(Plane + 1)(Row).Insert(Cube, False);
      end if;
      if Cubes(Plane + 1)(Row).Find(Cube + 1) = Cube_Row.No_Element then
         Cubes(Plane + 1)(Row).Insert(Cube + 1, False);
      end if;
      -- Row below
      if Cubes(Plane + 1).Find(Row + 1) = Cube_Plane.No_Element then
         -- The full row below it is missing.
         Cubes(Plane + 1).Insert(Row + 1, Cube_Row.Empty_Map);
      end if;
      -- Add missing elements in row above it
      if Cubes(Plane + 1)(Row + 1).Find(Cube - 1) = Cube_Row.No_Element then
         Cubes(Plane + 1)(Row + 1).Insert(Cube - 1, False);
      end if;
      if Cubes(Plane + 1)(Row + 1).Find(Cube) = Cube_Row.No_Element then
         Cubes(Plane + 1)(Row + 1).Insert(Cube, False);
      end if;
      if Cubes(Plane + 1)(Row + 1).Find(Cube + 1) = Cube_Row.No_Element then
         Cubes(Plane + 1)(Row + 1).Insert(Cube + 1, False);
      end if;
   end Surround;

   procedure Append_Empty_Space(Cubes : in out Cube_Space.Map) is
      To_Append : Append_Vector.Vector;
   begin
      Plane_Loop:
      for P in Cubes.Iterate loop
         Row_Loop:
         for R in Cubes(P).Iterate loop
            Cube_Loop:
            for C in Cubes(P)(R).Iterate loop
               if Cubes(P)(R)(C) then
                  To_Append.Append((Key(P), Key(R), Key(C)));
               end if;
            end loop Cube_Loop;
         end loop Row_Loop;
      end loop Plane_Loop;

      Append_Loop:
      for A of To_Append loop
         Surround(Cubes, A);
      end loop Append_Loop;
   end Append_Empty_Space;

   function Count_Around(Cubes : in Cube_Space.Map;
                         Coordinate : in Coordinate_Array) return Integer is
      Z : constant Integer := 1;
      Y : constant Integer := 2;
      X : constant Integer := 3;

      Result : Integer := 0;
   begin
      Plane_Loop:
      for DZ in Coordinate(Z) - 1 .. Coordinate(Z) + 1 loop
         if Cubes.Find(DZ) /= Cube_Space.No_Element then
            Row_Loop:
            for DY in Coordinate(Y) - 1 .. Coordinate(Y) + 1 loop
               if Cubes(DZ).Find(DY) /= Cube_Plane.No_Element then
                  Cube_Loop:
                  for DX in Coordinate(X) - 1 .. Coordinate(X) + 1 loop
                     if Cubes(DZ)(DY).Find(DX) /= Cube_Row.No_Element then
                        if Cubes(DZ)(DY)(DX) and then not
                          (Coordinate(Z) = DZ and Coordinate(Y) = DY and Coordinate(X) = DX) then
                           Result := Result + 1;
                        end if;
                     end if;
                  end loop Cube_Loop;
               end if;
            end loop Row_Loop;
         end if;
      end loop Plane_Loop;
      return Result;
   end Count_Around;

   procedure Step(Cubes : in out Cube_Space.Map) is
      Old_Cubes : Cube_Space.Map := Cubes;
      Neighbours : Integer;
   begin
      Plane_Loop:
      for P in Old_Cubes.Iterate loop
         Row_Loop:
         for R in Old_Cubes(P).Iterate loop
            Cube_Loop:
            for C in Old_Cubes(P)(R).Iterate loop
               Neighbours := Count_Around(Old_Cubes, (Key(P), Key(R), Key(C)));
               if Old_Cubes(P)(R)(C) then
                  if Neighbours /= 2 and then Neighbours /= 3 then
                     --Log_Debug("This one becomes inactive");
                     Cubes(Key(P))(Key(R))(Key(C)) := False;
                  end if;
               else
                  if Neighbours = 3 then
                     --Log_Debug("This one becomes active");
                     Cubes(Key(P))(Key(R))(Key(C)) := True;
                  end if;
               end if;
            end loop Cube_Loop;
         end loop Row_Loop;
      end loop Plane_Loop;
   end Step;

   procedure Print_Base(Cubes : in Cube_Space.Map) is
      Layer : Integer := -1;
   begin
      Row_Loop:
      for DY in -40 .. 40 loop
         if Cubes(Layer).Find(DY) /= Cube_Plane.No_Element then
            Cube_Loop:
            for DX in -40 .. 40 loop
               if Cubes(Layer)(DY).Find(DX) /= Cube_Row.No_Element then
                  if Cubes(Layer)(DY)(DX) then
                     Put('#');
                  else
                     Put('.');
                  end if;
               else
                  Put('.');
               end if;
            end loop Cube_Loop;
         else
            Put(".................................................................................");
         end if;
         Put_Line("");
      end loop Row_Loop;
      Put_Line("");
      Put_Line("");
   end Print_Base;

   Pocket_Dimension : Cube_Space.Map;
   Answer           : Integer;
begin
   Load_File(Pocket_Dimension);

   Cycle_Loop:
   for I in 1 .. 6 loop
      Append_Empty_Space(Pocket_Dimension);
      Step(Pocket_Dimension);

      if Enable_Debug then
         Print_Base(Pocket_Dimension);
      end if;
      Log_Debug("Number of active cubes after step " & Integer'Image(I)
                & ": " & Integer'Image(Count_Cubes(Pocket_Dimension)));
   end loop Cycle_Loop;

   Answer := Count_Cubes(Pocket_Dimension);

   Put_line("The number of active cubes after the sixth cycle is "
            & Integer'Image(Answer));
end Day17_1;
