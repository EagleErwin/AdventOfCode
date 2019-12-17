with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;

procedure Day12_2 is
   -- ### CONSTANTS ### --
   DEBUG     : constant Boolean := False; -- Set this to True to enable debug logging
   Grid_Size : constant Integer := 2 ** 20; -- Max X and negative X value for the grid surface.
   Nr_Of_Moons : constant Integer := 4;
   Total_Nr_Of_Steps : constant Integer := 1_000; -- Number of steps to simulate

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   subtype Coordinate is Integer range -Grid_Size .. Grid_Size - 1;
   subtype Velocity is Integer range -Grid_Size .. Grid_Size - 1;

   type Moon is record
      X        : Coordinate;
      Y        : Coordinate;
      Z        : Coordinate;
      VX       : Velocity;
      VY       : Velocity;
      VZ       : Velocity;
      X_Period : Integer;
      Y_Period : Integer;
      Z_Period : Integer;
   end record;

   type Space is array (1 .. Nr_Of_Moons) of Moon;

   -- ### VARIABLES ### --
   Initial_Moon_State : Space;

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
                                  VZ => 0,
                                  X_Period => -1,
                                  Y_Period => -1,
                                  Z_Period => -1);
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

   function Check_Initial_X_State (Moons   : in out Space;
                                   Initial : in Space;
                                   Step    : in Integer) return Boolean is
      Result : Boolean := True;
   begin
      for M in Moons'Range loop
         if Moons(M).X_Period = -1 then -- Only update the period if it is not found yet.
            if Moons(M).X = Initial(M).X and then
              Moons(M).VX = Initial(M).VX then
               Moons(M).X_Period := Step;
               Put_Line("Found X-period for moon" & Integer'Image(M) & ":" & Integer'Image(Step));
            else
               Result := False;
            end if;
         end if;
      end loop;
      return Result;
   end Check_Initial_X_State;

   function Check_Initial_Y_State (Moons   : in out Space;
                                   Initial : in Space;
                                   Step    : in Integer) return Boolean is
      Result : Boolean := True;
   begin
      for M in Moons'Range loop
         if Moons(M).Y_Period = -1 then -- Only update the period if it is not found yet.
            if Moons(M).Y = Initial(M).Y and then
              Moons(M).VY = Initial(M).VY then
               Moons(M).Y_Period := Step;
               Put_Line("Found Y-period for moon" & Integer'Image(M) & ":" & Integer'Image(Step));
            else
               Result := False;
            end if;
         end if;
      end loop;
      return Result;
   end Check_Initial_Y_State;

   function Check_Initial_Z_State (Moons   : in out Space;
                                   Initial : in Space;
                                   Step    : in Integer) return Boolean is
      Result : Boolean := True;
   begin
      for M in Moons'Range loop
         if Moons(M).Y_Period = -1 then -- Only update the period if it is not found yet.
            if Moons(M).Z = Initial(M).Z and then
              Moons(M).VZ = Initial(M).VZ then
               Moons(M).Z_Period := Step;
               Put_Line("Found Z-period for moon" & Integer'Image(M) & ":" & Integer'Image(Step));
            else
               Result := False;
            end if;
         end if;
      end loop;
      return Result;
   end Check_Initial_Z_State;

   function Run(Moons : in out Space) return Integer is
      Result : Integer := 0;
      Found_X : Boolean := False;
      Found_Y : Boolean := False;
      Found_Z : Boolean := False;

      X_Period : Integer;
      Y_Period : Integer;
      Z_Period : Integer;
   begin
      Print_Moons(Step => 0, Moons => Moons);
      Initial_Moon_State := Moons;
      Step_Loop:
      for I in 1 .. Integer'Last loop
         Intermediate_Moon_Update_Loop:
         for M in 1 .. Nr_Of_Moons loop
            Update_Moon_Velocity(Moon_Idx => M,
                                 Moons    => Moons);
         end loop Intermediate_Moon_Update_Loop;
         Update_Moon_Positions(Moons => Moons);
         Print_Moons(Step => I, Moons => Moons);

         if not Found_X and then Check_Initial_X_State(Moons   => Moons,
                                                       Initial => Initial_Moon_State,
                                                       Step    => I) then
            X_Period := I;
            Found_X := True;
            Put_Line("Found all periods of X: " & Integer'Image(X_Period));
         end if;
         if not Found_Y and then Check_Initial_Y_State(Moons   => Moons,
                                                       Initial => Initial_Moon_State,
                                                       Step    => I) then
            Y_Period := I;
            Found_Y := True;
            Put_Line("Found all periods of Y: " & Integer'Image(Y_Period));
         end if;
         if not Found_Z and then Check_Initial_Z_State(Moons   => Moons,
                                                       Initial => Initial_Moon_State,
                                                       Step    => I) then
            Z_Period := I;
            Found_Z := True;
            Put_Line("Found all periods of Z: " & Integer'Image(Z_Period));
         end if;

         if I mod 1_000_000 = 0 then
            Put_Line(Integer'Image(I) & " steps verified");
         end if;
         if Found_X and then Found_Y and then Found_Z then
            exit Step_Loop;
         end if;
      end loop Step_Loop;
      return X_Period * Y_Period * Z_Period;
   end Run;

   Subjects : Space;
   Answer : Integer;
begin
   Load_File (Moons => Subjects);

   Answer := Run(Moons => Subjects);

   Put_Line("Cycle detected after " & Integer'Image(Answer) & " steps");
end Day12_2;
