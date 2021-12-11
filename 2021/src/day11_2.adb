with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day11_2 is
   -- ### CONSTANTS ### --
   Number_Of_Octopuses : constant Integer := 100;

   -- ### TYPE DEFINITIONS ### --
   type Energy_Cell is record
      Level : Integer;
      Flashed : Boolean;
   end record;

   package Energy_Row is new Vectors(Index_Type   => Natural,
                                     Element_Type => Energy_Cell);
   use Energy_Row;

   package Energy_Field is new Vectors(Index_Type   => Natural,
                                       Element_Type => Energy_Row.Vector);
   use Energy_Field;

   procedure Load_File (Octopuses : out Energy_Field.Vector) is
      Input              : File_Type;
      Current_Line       : Unbounded_String;
      Current_Energy_Row : Energy_Row.Vector;
      Current_Energy_Cell : Energy_Cell := (Level => 0, Flashed => False);
   begin
      Open (File => Input, Mode => In_File, Name => "data/day11.input");
      while not End_Of_File (Input) loop
         Current_Energy_Row := Energy_Row.Empty_Vector;
         Current_Line := To_Unbounded_String(Get_Line(Input));

         Character_Loop:
         for I in 1 .. Length(Current_Line) loop
            Current_Energy_Cell.Level := Integer'Value((1 => Element(Current_Line, I)));
            Current_Energy_Row.Append(Current_Energy_Cell);
         end loop Character_Loop;

         Octopuses.Append(Current_Energy_Row);
      end loop;
      Close (File => Input);
   end Load_File;

   procedure Print_Octopuses(Octopuses : in Energy_Field.Vector) is
   begin
      Row_Loop:
      for Row of Octopuses loop
         Col_Loop:
         for Cell of Row loop
            Put(Integer'Image(Cell.Level));
         end loop Col_Loop;
         Put_Line("");
      end loop Row_Loop;
   end Print_Octopuses;

   procedure Do_Flash(Octopuses : in out Energy_Field.Vector;
                      Row_Idx : in Integer;
                      Col_Idx : in Integer) is
      Row_Idx_Start : Integer := Row_Idx;
      Row_Idx_Stop  : Integer := Row_Idx;
      Col_Idx_Start : Integer := Col_Idx;
      Col_Idx_Stop  : Integer := Col_Idx;
   begin
      if Row_Idx > Octopuses.First_Index then
         -- We are not at the top row
         Row_Idx_Start := Row_Idx - 1;
      end if;
      if Col_Idx > Octopuses(Row_Idx).First_Index then
         -- We are not in the first column
         Col_Idx_Start := Col_Idx - 1;
      end if;


      if Row_Idx < Octopuses.Last_Index then
         -- We are not at the bottom row
         Row_Idx_Stop := Row_Idx + 1;
      end if;
      if Col_Idx < Octopuses(Row_Idx).Last_Index then
         -- We are not in the last column
         Col_Idx_Stop := Col_Idx + 1;
      end if;

      Row_Loop:
      for R in Row_Idx_Start .. Row_Idx_Stop loop
         Col_Loop:
         for C in Col_Idx_Start .. Col_Idx_Stop loop
            if R /= Row_Idx or else C /= Col_Idx then -- Don't update ourself.
               Octopuses(R)(C).Level := Octopuses(R)(C).Level + 1;
            end if;
         end loop Col_Loop;
      end loop Row_Loop;

      Octopuses(Row_Idx)(Col_Idx).Flashed := True;
   end Do_Flash;

   -- Increase the energy level of all octopuses with 1.
   procedure Increase_Energy (Octopuses : in out Energy_Field.Vector) is
   begin
      Row_Loop:
      for R in Octopuses.First_Index .. Octopuses.Last_Index loop
         Col_Loop:
         for C in Octopuses(R).First_Index .. Octopuses(R).Last_Index loop
            Octopuses(R)(C).Level := Octopuses(R)(C).Level + 1;
         end loop Col_Loop;
      end loop Row_Loop;
   end Increase_Energy;

   function Flash (Octopuses : in out Energy_Field.Vector) return Boolean is
      Has_Flashed : Boolean := True;
      Nr_Of_Flashes : Integer := 0;
   begin
      Increase_Energy(Octopuses);
      Flash_Loop:
      while Has_Flashed loop
         Has_Flashed := False;
         Row_Loop:
         for R in Octopuses.First_Index .. Octopuses.Last_Index loop
            Col_Loop:
            for C in Octopuses(R).First_Index .. Octopuses(R).Last_Index loop
               if not Octopuses(R)(C).Flashed and then
                 Octopuses(R)(C).Level > 9 then
                  Do_Flash(Octopuses, R, C);
                  Has_Flashed := True;
                  Nr_Of_Flashes := Nr_Of_Flashes + 1;
               end if;
            end loop Col_Loop;
         end loop Row_Loop;
      end loop Flash_Loop;

      Reset_Row_Loop:
      for R in Octopuses.First_Index .. Octopuses.Last_Index loop
         Reset_Col_Loop:
         for C in Octopuses(R).First_Index .. Octopuses(R).Last_Index loop
            if Octopuses(R)(C).Level > 9 then
               Octopuses(R)(C).Level := 0;
            end if;
            Octopuses(R)(C).Flashed := False;
         end loop Reset_Col_Loop;
      end loop Reset_Row_Loop;

      return Nr_Of_Flashes = Number_Of_Octopuses;
   end Flash;

   Octopuses   : Energy_Field.Vector;
   Step        : Integer := 0;
   All_Flashed : Boolean := False;
   Answer      : Integer := 0;
begin
   Load_File(Octopuses);

   while not All_Flashed loop
      Step := Step + 1;
      All_Flashed := Flash(Octopuses);
   end loop;

   Answer := Step;

   Put_line("The first step in wich all octopuses flash is "
            & Integer'Image(Answer) & ".");
end Day11_2;
