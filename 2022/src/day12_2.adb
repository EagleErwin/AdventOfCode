with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day12_2 is
   -- ## CONSTANTS ## --
   Map_Width  : constant Integer := 162;
   Map_Height : constant Integer := 41;

   Max_Distance : constant Integer := Map_Width * Map_Height;

   -- ## TYPES ## --
   type Map_Location is record
      Elevation            : Integer;
      Steps_To_Destination : Integer;
   end record;

   type Height_Map_Row is array (1 .. Map_Width) of Map_Location;
   type Height_Map is array (1 .. Map_Height) of Height_Map_Row;

   type Coordinate is record
      Row    : Integer;
      Column : Integer;
   end record;

   type Coordinate_Array is array (1 .. Max_Distance) of Coordinate;

   -- ## GLOBAL VARIABLES ## --
   Nr_Of_Start_Positions : Integer := 0;
   Start_Positions       : Coordinate_Array := (others => (0, 0));

   function Get_Location_Object (Input_Char : in Character;
                                 Position   : in Coordinate)
                                 return Map_Location is
      Result : Map_Location := (-1, -1);
   begin
      case Input_Char is
         when 'S' =>
            Result.Elevation            := Character'Pos('a');
            Result.Steps_To_Destination := Max_Distance;
            Nr_Of_Start_Positions := Nr_Of_Start_Positions + 1;
            Start_Positions(Nr_Of_Start_Positions) := Position;
         when 'E' =>
            Result.Elevation            := Character'Pos('z');
            Result.Steps_To_Destination := 0;
         when others =>
            Result.Elevation            := Character'Pos(Input_Char);
            Result.Steps_To_Destination := Max_Distance;
            if (Input_Char = 'a') then
               Nr_Of_Start_Positions := Nr_Of_Start_Positions + 1;
               Start_Positions(Nr_Of_Start_Positions) := Position;
            end if;
      end case;

      return Result;
   end Get_Location_Object;

   procedure Load_File (Input_Map : out Height_Map) is
      Input               : File_Type;
      Current_Line        : Unbounded_String;
      Current_Char        : Character;
      Row_Index           : Integer := 0;
   begin
      Input_Map := (others => (others => (-1, -1)));
      Open (File => Input, Mode => In_File, Name => "data/day12.input");
      Read_Line_Loop:
      while not End_Of_File (Input) loop
         Row_Index := Row_Index + 1;
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Read_Position_Loop:
         for I in 1 .. Length(Current_Line) loop
            Current_Char := Element(Current_Line, I);
            Input_Map(Row_Index)(I) :=
              Get_Location_Object(Current_Char, (Row_Index, I));
         end loop Read_Position_Loop;
      end loop Read_Line_Loop;
      Close (File => Input);
   end Load_File;

   function Check_Neighbour (Self      : in out Map_Location;
                             Neighbour : in Map_Location) return Boolean is
      Updated : Boolean := False;
   begin
      if (Neighbour.Elevation - Self.Elevation <= 1) then
         -- We can walk to this neighbour.
         if (Neighbour.Steps_To_Destination < Self.Steps_To_Destination - 1) then
            -- Via this neighbour is also closer to the destination.
            Self.Steps_To_Destination := Neighbour.Steps_To_Destination + 1;
            Updated := True;
         end if;
      end if;
      return Updated;
   end Check_Neighbour;

   function Walk_Iteration (Input_Map : in out Height_Map) return Boolean is
      Updated : Boolean := False;
   begin
      Row_Loop:
      for Y in 1 .. Map_Height loop
         Col_Loop:
         for X in 1 .. Map_Width loop
            -- Check above
            if Y > 1 then
               Updated := Check_Neighbour(Input_Map(Y)(X), Input_Map(Y - 1)(X))
                 or Updated;
            end if;
            -- Check below
            if Y < Map_Height then
               Updated := Check_Neighbour(Input_Map(Y)(X), Input_Map(Y + 1)(X))
                 or Updated;
            end if;
            -- Check left
            if X > 1 then
               Updated := Check_Neighbour(Input_Map(Y)(X), Input_Map(Y)(X - 1))
                 or Updated;
            end if;
            -- Check right
            if X < Map_Width then
               Updated := Check_Neighbour(Input_Map(Y)(X), Input_Map(Y)(X + 1))
                 or Updated;
            end if;
         end loop Col_Loop;
      end loop Row_Loop;
      return Updated;
   end Walk_Iteration;

   function Copy_Map (Data_To_Copy : in Height_Map) return Height_Map is
      Result : Height_Map;
   begin
      Row_Loop:
      for Y in 1 .. Map_Height loop
         Col_Loop:
         for X in 1 .. Map_Width loop
            Result(Y)(X) := Data_To_Copy(Y)(X);
         end loop Col_Loop;
      end loop Row_Loop;
      return Result;
   end Copy_Map;

   procedure Print_Map (Data_To_Print : in Height_Map) is
   begin
      Row_Loop:
      for Y in 1 .. Map_Height loop
         Col_Loop:
         for X in 1 .. Map_Width loop
            Put(Integer'Image(Data_To_Print(Y)(X).Steps_To_Destination));
         end loop Col_Loop;
         Put_Line("");
      end loop Row_Loop;
   end Print_Map;

   Original_Hill : Height_Map;
   Hill   : Height_Map;

   Start_Position : Coordinate;
   Current_Distance : Integer;
   Answer : Integer := Max_Distance;
begin
   Load_File(Original_Hill);

   Start_Position_Loop:
   for I in 1 .. Nr_Of_Start_Positions loop
      Start_Position := Start_Positions(I);
      Put("Checking (" & Integer'Image(Start_Position.Row) & ","
               & Integer'Image(Start_Position.Column) & ")... ");
      Hill := Copy_Map(Original_Hill);
      Update_Distances_Loop:
      while Walk_Iteration(Hill) loop
         --Put_Line("Iteration...");
         --Print_Map(Hill);
         null;
      end loop Update_Distances_Loop;
      Current_Distance :=
        Hill(Start_Position.Row)(Start_Position.Column).Steps_To_Destination;
      Put(Integer'Image(Current_Distance));
      if Current_Distance < Answer then
         Put(" -> Best candidate so far!");
         Answer := Current_Distance;
      end if;
      Put_Line("");
   end loop Start_Position_Loop;

   Put_Line("The fewest steps required to move to the best signal location is"
            & Integer'Image(Answer));
end Day12_2;
