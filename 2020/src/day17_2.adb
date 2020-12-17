with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day17_2 is
   -- ### CONSTANTS ### --
   Enable_Debug : constant Boolean := False;

   Part_1 : Integer := 0;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Coordinate_Array is array (0 .. 3) of Integer;

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

   -- In a hyperspace, the key is the w index, the value the space
   package Cube_Hyper is new Hashed_Maps(Key_Type        => Integer,
                                         Element_Type    => Cube_Space.Map,
                                         Hash            => Natural_Hash,
                                         Equivalent_Keys => "=" );
   use Cube_Hyper;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   procedure Load_File (Cubes : out Cube_Hyper.Map) is
      Initial_Z    : constant Integer := 0;
      Initial_W    : constant Integer := 0;
      Input        : File_Type;
      Current_Line : Data_Line;
      Row_Index    : Integer := 0;

      Curr_Char    : Character;
   begin
      Cubes.Insert(Initial_W, Cube_Space.Empty_Map);
      Cubes(Initial_W).Insert(Initial_Z, Cube_Plane.Empty_Map);
      Open (File => Input, Mode => In_File, Name => "data/day17.input");
      Read_Loop:
      while not End_Of_File (Input) loop
         Cubes(Initial_W)(Initial_Z).Insert(Row_Index, Cube_Row.Empty_Map);
         Current_Line := To_Unbounded_String(Get_Line(Input));

         Parser_Loop:
         for I in 1 .. Length(Current_Line) loop
            Curr_Char := Element(Current_Line, I);
            if Curr_Char = '.' then
               Cubes(Initial_W)(Initial_Z)(Row_Index).Insert(I, False);
            else
               Cubes(Initial_W)(Initial_Z)(Row_Index).Insert(I, True);
            end if;
         end loop Parser_Loop;
         Row_Index := Row_Index + 1;
      end loop Read_Loop;
      Close (File => Input);
   end Load_File;

   function Count_Cubes(Cubes : in Cube_Hyper.Map) return Integer is
      Result : Integer := 0;
   begin
      Space_Loop:
      for S in Cubes.Iterate loop
         Plane_Loop:
         for P in Cubes(S).Iterate loop
            Row_Loop:
            for R in Cubes(S)(P).Iterate loop
               Cube_Loop:
               for C in Cubes(S)(P)(R).Iterate loop
                  if Cubes(S)(P)(R)(C) then
                     Result := Result + 1;
                  end if;
               end loop Cube_Loop;
            end loop Row_Loop;
         end loop Plane_Loop;
      end loop Space_Loop;
      return Result;
   end Count_Cubes;

   procedure Surround_Row(Cubes : in out Cube_Hyper.Map;
                          Coordinate : in Coordinate_Array) is
      Space : Integer := Coordinate(0);
      Plane : Integer := Coordinate(1);
      Row   : Integer := Coordinate(2);
      Cube  : Integer := Coordinate(3);
   begin
      if Cubes(Space)(Plane).Find(Row) = Cube_Plane.No_Element then
         -- The full row is missing.
         Cubes(Space)(Plane).Insert(Row, Cube_Row.Empty_Map);
      end if;
      -- Add missing elements in row
      if Cubes(Space)(Plane)(Row).Find(Cube - 1) = Cube_Row.No_Element then
         Cubes(Space)(Plane)(Row).Insert(Cube - 1, False);
      end if;
      if Cubes(Space)(Plane)(Row).Find(Cube) = Cube_Row.No_Element then
         Cubes(Space)(Plane)(Row).Insert(Cube, False);
      end if;
      if Cubes(Space)(Plane)(Row).Find(Cube + 1) = Cube_Row.No_Element then
         Cubes(Space)(Plane)(Row).Insert(Cube + 1, False);
      end if;
   end Surround_Row;

   procedure Surround_Plane(Cubes      : in out Cube_Hyper.Map;
                            Coordinate : in Coordinate_Array) is
      Space : Integer := Coordinate(0);
      Plane : Integer := Coordinate(1);
      Row   : Integer := Coordinate(2);
      Cube  : Integer := Coordinate(3);
   begin
      if Cubes(Space).Find(Plane) = Cube_Space.No_Element then
         -- The full plane is missing.
         Cubes(Space).Insert(Plane, Cube_Plane.Empty_Map);
      end if;
      -- Row above
      Surround_Row(Cubes, (Space, Plane, Row - 1, Cube));
      -- Same row
      Surround_Row(Cubes, (Space, Plane, Row, Cube));
      -- Row below
      Surround_Row(Cubes, (Space, Plane, Row + 1, Cube));
   end Surround_Plane;

   procedure Surround_Space(Cubes : in out Cube_Hyper.Map;
                            Coordinate : in Coordinate_Array) is
      Space : Integer := Coordinate(0);
      Plane : Integer := Coordinate(1);
      Row   : Integer := Coordinate(2);
      Cube  : Integer := Coordinate(3);
   begin
      if Cubes.Find(Space) = Cube_Hyper.No_Element then
         -- The full space is missing.
         Cubes.Insert(Space, Cube_Space.Empty_Map);
      end if;
      -- Plane above
      Surround_Plane(Cubes, (Space, Plane - 1, Row, Cube));
      -- Same plane
      Surround_Plane(Cubes, (Space, Plane, Row, Cube));
      -- Plane below
      Surround_Plane(Cubes, (Space, Plane + 1, Row, Cube));
   end Surround_Space;


   -- Add an inactive Cube around the cube provided by the position.
   -- Does not overwrite an existing cube
   procedure Surround(Cubes : in out Cube_Hyper.Map;
                      Coordinate : in Coordinate_Array) is
      Space : Integer := Coordinate(0);
      Plane : Integer := Coordinate(1);
      Row   : Integer := Coordinate(2);
      Cube  : Integer := Coordinate(3);
   begin
      -- Append inactive cubes on a different space
      -- Space above
      Surround_Space(Cubes, (Space - 1, Plane, Row, Cube));
      -- Current space
      Surround_Space(Cubes, (Space, Plane, Row, Cube));
      -- Space below
      Surround_Space(Cubes, (Space + 1, Plane, Row, Cube));
   end Surround;

   procedure Append_Empty_Space(Cubes : in out Cube_Hyper.Map) is
      To_Append : Append_Vector.Vector;
   begin
      Space_Loop:
      for S in Cubes.Iterate loop
         Plane_Loop:
         for P in Cubes(S).Iterate loop
            Row_Loop:
            for R in Cubes(S)(P).Iterate loop
               Cube_Loop:
               for C in Cubes(S)(P)(R).Iterate loop
                  if Cubes(S)(P)(R)(C) then
                     To_Append.Append((Key(S), Key(P), Key(R), Key(C)));
                  end if;
               end loop Cube_Loop;
            end loop Row_Loop;
         end loop Plane_Loop;
      end loop Space_Loop;

      Append_Loop:
      for A of To_Append loop
         Surround(Cubes, A);
      end loop Append_Loop;
   end Append_Empty_Space;

   function Count_Around(Cubes : in Cube_Hyper.Map;
                         Coordinate : in Coordinate_Array) return Integer is
      W : constant Integer := 0;
      Z : constant Integer := 1;
      Y : constant Integer := 2;
      X : constant Integer := 3;

      Result : Integer := 0;
   begin
      Space_Loop:
      for DW in Coordinate(W) - 1 .. Coordinate(W) + 1 loop
         if Cubes.Find(DW) /= Cube_Hyper.No_Element then
            Plane_Loop:
            for DZ in Coordinate(Z) - 1 .. Coordinate(Z) + 1 loop
               if Cubes(DW).Find(DZ) /= Cube_Space.No_Element then
                  Row_Loop:
                  for DY in Coordinate(Y) - 1 .. Coordinate(Y) + 1 loop
                     if Cubes(DW)(DZ).Find(DY) /= Cube_Plane.No_Element then
                        Cube_Loop:
                        for DX in Coordinate(X) - 1 .. Coordinate(X) + 1 loop
                           if Cubes(DW)(DZ)(DY).Find(DX) /= Cube_Row.No_Element then
                              if Cubes(DW)(DZ)(DY)(DX) and then not
                                (Coordinate(W) = DW and Coordinate(Z) = DZ and
                                     Coordinate(Y) = DY and Coordinate(X) = DX) then
                                 Result := Result + 1;
                              end if;
                           end if;
                        end loop Cube_Loop;
                     end if;
                  end loop Row_Loop;
               end if;
            end loop Plane_Loop;
         end if;
      end loop Space_Loop;

      return Result;
   end Count_Around;

   procedure Step(Cubes : in out Cube_Hyper.Map) is
      Old_Cubes : Cube_Hyper.Map := Cubes;
      Neighbours : Integer;
   begin
      Space_Loop:
      for S in Old_Cubes.Iterate loop
         Plane_Loop:
         for P in Old_Cubes(S).Iterate loop
            Row_Loop:
            for R in Old_Cubes(S)(P).Iterate loop
               Cube_Loop:
               for C in Old_Cubes(S)(P)(R).Iterate loop
                  Neighbours := Count_Around(Old_Cubes, (Key(S), Key(P), Key(R), Key(C)));
                  if Old_Cubes(S)(P)(R)(C) then
                     if Neighbours /= 2 and then Neighbours /= 3 then
                        --Log_Debug("This one becomes inactive");
                        Cubes(Key(S))(Key(P))(Key(R))(Key(C)) := False;
                     end if;
                  else
                     if Neighbours = 3 then
                        --Log_Debug("This one becomes active");
                        Cubes(Key(S))(Key(P))(Key(R))(Key(C)) := True;
                     end if;
                  end if;
               end loop Cube_Loop;
            end loop Row_Loop;
         end loop Plane_Loop;
      end loop Space_Loop;
   end Step;

   procedure Print_Base(Cubes : in Cube_Hyper.Map) is
      Space : INteger := 0;
      Layer : Integer := 0;
   begin
      Row_Loop:
      for DY in -40 .. 40 loop
         if Cubes(Space)(Layer).Find(DY) /= Cube_Plane.No_Element then
            Cube_Loop:
            for DX in -40 .. 40 loop
               if Cubes(Space)(Layer)(DY).Find(DX) /= Cube_Row.No_Element then
                  if Cubes(Space)(Layer)(DY)(DX) then
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

   Pocket_Dimension : Cube_Hyper.Map;
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
end Day17_2;
