with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;

procedure Day12_1 is
   -- ### CONSTANTS ### --
   DEBUG     : constant Boolean := False; -- Set this to True to enable debug logging
   Grid_Size : constant Integer := 2 ** 10; -- Max X and negative X value for the grid surface.
   Nr_Of_Moons : constant Integer := 4;
   Total_Nr_Of_Steps : constant Integer := 1_000; -- Number of steps to simulate

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   subtype Coordinate is Integer range -Grid_Size .. Grid_Size - 1;
   subtype Velocity is Integer range -Grid_Size .. Grid_Size - 1;

   type Moon is record
      X     : Coordinate;
      Y     : Coordinate;
      Z     : Coordinate;
      VX    : Velocity;
      VY    : Velocity;
      VZ    : Velocity;
   end record;

   type Space is array (1 .. Nr_Of_Moons) of Moon;

   procedure Log_Debug (Log_Line : in String) is
   begin
      if (DEBUG) then
         Put_Line(Log_Line);
      end if;
   end Log_Debug;

   procedure Print_Moons(Step : in Integer; Moons : in Space) is
   begin
      if DEBUG then
         Log_Debug("After" & Integer'Image(Step) & " steps:");
         for M in Moons'Range loop
            Log_Debug("pos=<x=" & Coordinate'Image(Moons(M).X)
                      & ", y= " & Coordinate'Image(Moons(M).Y)
                      & ", z= " & Coordinate'Image(Moons(M).Z)
                      & ">, vel=<x= " & Velocity'Image(Moons(M).VX)
                      & ", y= " & Velocity'Image(Moons(M).VY)
                      & ", z= " & Velocity'Image(Moons(M).VZ) & ">");
         end loop;
      end if;
   end Print_Moons;

   procedure Load_File (Moons : out Space) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Moon_Idx     : Integer := 0;
      Buffer       : Unbounded_String;

      Curr_X       : Coordinate;
      Curr_Y       : Coordinate;
      Curr_Z       : Coordinate;

      Start_Char   : Integer;
      Stop_Char    : Integer;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day12.input");
      Read_File_Loop:
      while not End_Of_File (Input) loop
         Moon_Idx := Moon_Idx + 1;
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Start_Char := Index(Current_Line, "<x=") + 3;
         Stop_Char  := Index(Current_Line, ",") - 1;
         Curr_X := Coordinate'Value(Slice(Source => Current_Line,
                                                    Low    => Start_Char,
                                                    High   => Stop_Char));
         Start_Char := Index(Current_Line, "y=") + 2;
         Stop_Char  := Index(Current_Line, ",", Index(Current_Line, "y=") + 2) - 1;
         Curr_Y := Coordinate'Value(Slice(Source => Current_Line,
                                                    Low    => Start_Char,
                                                    High   => Stop_Char));
         Start_Char := Index(Current_Line, "z=") + 2;
         Stop_Char  := Index(Current_Line, ">") - 1;
         Curr_Z := Coordinate'Value(Slice(Source => Current_Line,
                                                    Low    => Start_Char,
                                                    High   => Stop_Char));
         Moons(Moon_Idx) := Moon'(X  => Curr_X,
                                  Y  => Curr_Y,
                                  Z  => Curr_Z,
                                  VX => 0,
                                  VY => 0,
                                  VZ => 0);
      end loop Read_File_Loop;
      Close (File => Input);
   end Load_File;

   function Compare (First  : Coordinate;
                     Second : Coordinate) return Velocity is
   begin
      if First < Second then
         return 1;
      elsif First > Second then
         return -1;
      else
         return 0;
      end if;
   end Compare;

   procedure Update_Moon_Velocity(Moon_Idx : in Integer;
                                  Moons    : in out Space) is

   begin
      for M in Moons'Range loop
         if M /= Moon_Idx then
            Moons(Moon_Idx).VX := Moons(Moon_Idx).VX + Compare(Moons(Moon_Idx).X, Moons(M).X);
            Moons(Moon_Idx).VY := Moons(Moon_Idx).VY + Compare(Moons(Moon_Idx).Y, Moons(M).Y);
            Moons(Moon_Idx).VZ := Moons(Moon_Idx).VZ + Compare(Moons(Moon_Idx).Z, Moons(M).Z);
         end if;
      end loop;
   end Update_Moon_Velocity;

   procedure Update_Moon_Positions (Moons : in out Space) is
   begin
      for M in Moons'Range loop
         Moons(M).X := Moons(M).X + Coordinate(Moons(M).VX);
         Moons(M).Y := Moons(M).Y + Coordinate(Moons(M).VY);
         Moons(M).Z := Moons(M).Z + Coordinate(Moons(M).VZ);
      end loop;
   end Update_Moon_Positions;

   procedure Run(Moons : in out Space) is
   begin
      Print_Moons(Step => 0, Moons => Moons);
      Step_Loop:
      for I in 1 .. Total_Nr_Of_Steps loop
         Intermediate_Moon_Update_Loop:
         for M in 1 .. Nr_Of_Moons loop
            Update_Moon_Velocity(Moon_Idx => M,
                                 Moons    => Moons);
         end loop Intermediate_Moon_Update_Loop;
         Update_Moon_Positions(Moons => Moons);
         Print_Moons(Step => I, Moons => Moons);
      end loop Step_Loop;
   end Run;

   function Calculate_Total_Energy(Moons : in Space) return Integer is
      Total_Energy : Integer := 0;

      Curr_E_Pot : Integer;
      Curr_E_Kin : Integer;
   begin
      for I in Moons'Range loop
         Curr_E_Pot := Integer(abs(Moons(I).X) + abs(Moons(I).Y) + abs(Moons(I).Z));
         Curr_E_Kin := Integer(abs(Moons(I).VX) + abs(Moons(I).VY) + abs(Moons(I).VZ));
         Total_Energy := Total_Energy + Curr_E_Pot * Curr_E_Kin;
         Log_Debug("Moon" & Integer'Image(I)
                   & ": Potential energy:" & Integer'Image(Curr_E_Pot)
                   & "; Total kinetic energy:" & Integer'Image(Curr_E_Kin)
                   & "; Total energy:" & Integer'Image(Curr_E_Pot * Curr_E_Kin));
      end loop;
      Log_Debug("Total energy:" & Integer'Image(Total_Energy));
      return Total_Energy;
   end Calculate_Total_Energy;

   Subjects : Space;
   Answer : Integer;
begin
   Load_File (Moons => Subjects);

   Run(Moons => Subjects);
   Answer := Calculate_Total_Energy(Moons => Subjects);

   Put_Line("Total energy after" & Integer'Image(Total_Nr_Of_Steps) & " steps:" & Integer'Image(Answer));
end Day12_1;
