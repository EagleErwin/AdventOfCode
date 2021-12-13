with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day13_2 is
   -- ### CONSTANTS ### --

   -- ### TYPE DEFINITIONS ### --

   function Natural_Hash (Input : Natural) return Hash_Type is
   begin
      return Hash_Type(Input);
   end Natural_Hash;

   package Coordinate_Row is new Hashed_Maps(Key_Type        => Natural,
                                             Element_Type    => Boolean,
                                             Hash            => Natural_Hash,
                                             Equivalent_Keys => "=");
   use Coordinate_Row;

   package Coordinate_Plane is new Hashed_Maps(Key_Type        => Natural,
                                               Element_Type    => Coordinate_Row.Map,
                                               Hash            => Natural_Hash,
                                               Equivalent_Keys => "=");
   use Coordinate_Plane;

   type Axis is (X, Y);

   type Instruction is record
      Fold_Axis : Axis;
      Index : Natural;
   end record;

   package Instructions is new Vectors(Index_Type   => Natural,
                                       Element_Type => Instruction);
   use Instructions;

   procedure Process_Coordinate(Input  : in Unbounded_String;
                                Output : in out Coordinate_Plane.Map) is
      Separator_Pos : Natural;
      X : Natural;
      Y : Natural;
   begin
      Separator_Pos := Index(Input, ",");
      X := Natural'Value(Slice(Input, 1, Separator_Pos - 1));
      Y := Natural'Value(Slice(Input, Separator_Pos + 1, Length(Input)));
      if not Output.Contains(Y) then
         Output.Insert(Y, Coordinate_Row.Empty_Map);
      end if;
      if not Output(Y).Contains(X) then
         Output(Y).Insert(X, True);
      end if;
   end Process_Coordinate;

   procedure Process_Instruction(Input  : in Unbounded_String;
                                 Output : in out Instructions.Vector) is
      Equals_Sign_Pos : Natural;
      Fold_Axis       : Axis;
      Fold_Index      : Natural;
   begin
      Equals_Sign_Pos := Index(Input, "=");
      Fold_Axis := Axis'Value((1 => Element(Input, Equals_Sign_Pos - 1)));
      Fold_Index := Integer'Value(Slice(Input, Equals_Sign_Pos + 1, Length(Input)));

      Output.Append((Fold_Axis, Fold_Index));
   end Process_Instruction;

   procedure Load_File (Dots  : out Coordinate_Plane.Map;
                        Folds : out Instructions.Vector) is
      Input              : File_Type;
      Current_Line       : Unbounded_String;
      Parse_Instructions : Boolean := False;
   begin
      Dots  := Coordinate_Plane.Empty_Map;
      Folds := Instructions.Empty_Vector;

      Open (File => Input, Mode => In_File, Name => "data/day13.input");
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         if Current_Line = "" then
            Parse_Instructions := True;
         else
            if not Parse_Instructions then
               -- Coordinates
               Process_Coordinate(Current_Line, Dots);
            else
               -- Instructions
               Process_Instruction(Current_Line, Folds);
            end if;
         end if;
      end loop;
      Close (File => Input);
   end Load_File;

   -- Merge the contents of line B into line A.
   function Merge_Line(A : in Coordinate_Row.Map;
                       B : in Coordinate_Row.Map) return Coordinate_Row.Map is
      Result : Coordinate_Row.Map := Coordinate_Row.Empty_Map;
   begin
      A_Loop:
      for A_Elem in A.Iterate loop
         if not Result.Contains(Key(A_Elem)) then
            Result.Insert(Key(A_Elem), True);
         end if;
      end loop A_Loop;

      B_Loop:
      for B_Elem in B.Iterate loop
         if not Result.Contains(Key(B_Elem)) then
            Result.Insert(Key(B_Elem), True);
         end if;
      end loop B_Loop;
      return Result;
   end Merge_Line;

   -- Fold the plane along the given X value
   procedure Fold_Along_X(Plane : in out Coordinate_Plane.Map;
                         Index : in Natural) is
      New_Plane   : Coordinate_Plane.Map := Coordinate_Plane.Empty_Map;
      New_Col_Idx : Natural;
   begin
      Row_Loop:
      for R in Plane.Iterate loop
         New_Plane.Insert(Key(R), Coordinate_Row.Empty_Map);
         Col_Loop:
         for C in Plane(R).Iterate loop
            if Key(C) < Index then
               -- Left of the folding line. Just take over the value.
               if not New_Plane(Key(R)).Contains(Key(C)) then
                  New_Plane(Key(R)).Insert(Key(C), True);
               end if;
            elsif Key(C) > Index then
               -- Right the folding line. Add it above the line if it is not there yet.
               New_Col_Idx := Index - (Key(C) - Index);
               if not New_Plane(Key(R)).Contains(New_Col_Idx) then
                  New_Plane(Key(R)).Insert(New_Col_Idx, True);
               end if;
            else
               Put_Line("ERROR: Found dot on the fold line!");
            end if;
         end loop Col_Loop;
      end loop Row_Loop;
      Plane := New_Plane;
   end Fold_Along_X;

   -- Fold the plane along the given Y value
   procedure Fold_Along_Y(Plane : in out Coordinate_Plane.Map;
                         Index : in Natural) is
      New_Plane   : Coordinate_Plane.Map := Coordinate_Plane.Empty_Map;
      New_Row     : Coordinate_Row.Map;
      New_Row_Idx : Natural;
   begin
      Row_Loop:
      for R in Plane.Iterate loop
         if Key(R) < Index then
            -- Above from the folding line. Just take over the complete row.
            if Contains(New_Plane, Key(R)) then
               New_Row := Merge_Line(New_Plane(Key(R)), Plane(R));
               New_Plane.Delete(Key(R));
               New_Plane.Insert(Key(R), New_Row);
            else
               New_Plane.Insert(Key(R), Plane(R));
            end if;
         elsif Key(R) > Index then
            -- Below the folding line. Add it above the line.
            New_Row_Idx := Index - (Key(R) - Index);
            if Contains(New_Plane, New_Row_Idx) then
               New_Row := Merge_Line(New_Plane(New_Row_Idx), Plane(R));
               New_Plane.Delete(New_Row_Idx);
               New_Plane.Insert(New_Row_Idx, New_Row);
            else
               New_Plane.Insert(New_Row_Idx, Plane(R));
            end if;
         else
            -- There should be no dots on this row.
            null;
         end if;
      end loop Row_Loop;
      Plane := New_Plane;
   end Fold_Along_Y;

   procedure Fold(Plane : in out Coordinate_Plane.Map;
                  Instr : in Instruction) is
   begin
      case Instr.Fold_Axis is
         when X => Fold_Along_X(Plane, Instr.Index);
         when Y => Fold_Along_Y(Plane, Instr.Index);
      end case;
   end Fold;

   procedure Print_Dots(Dots : in Coordinate_Plane.Map) is
      Max_Row : Natural := 0;
      Max_Col : Natural := 0;
   begin
      Row_Loop:
      for R in Dots.Iterate loop
         if Key(R) > Max_Row then
            Max_Row := Key(R);
         end if;
         Col_Loop:
         for C in Dots(R).Iterate loop
            if Key(C) > Max_Col then
               Max_Col := Key(C);
            end if;
         end loop Col_Loop;
      end loop Row_Loop;

      Print_Loop:
      for I in 0 .. Max_Row loop
         if I < 10 then
            Put(" " & Integer'Image(I) & ": ");
         else
            Put(Integer'Image(I) & ": ");
         end if;
         if Dots.Contains(I) then
            for J in 0 .. Max_Col loop
               if Dots(I).Contains(J) then
                  Put("#");
               else
                  Put(" ");
               end if;
            end loop;
         else
            for N in 0 .. Max_Col loop
               Put(" ");
            end loop;
         end if;
         Put_Line("");
      end loop Print_Loop;
   end Print_Dots;

   Dots   : Coordinate_Plane.Map;
   Folds  : Instructions.Vector;
   Answer : Integer := 0;
begin
   Load_File(Dots, Folds);

   Instruction_Loop:
   for Instr of Folds loop
      Fold(Dots, Instr);
   end loop Instruction_Loop;

   Print_Dots(Dots);

end Day13_2;
