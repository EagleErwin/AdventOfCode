with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Containers.Generic_Array_Sort;

procedure Day11_1 is
   -- ### CONSTANTS ### --
   Enable_Debug : constant Boolean := False;
   Number_Of_Seats_In_Row : constant Integer := 91;
   Number_Of_Inputs : constant Integer := 97;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Position is (Floor, Occupied, Empty);

   type Seat_Row is array (1 .. Number_Of_Seats_In_Row) of Position;
   type Seat_Map is array (Natural range <>) of Seat_Row;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   function To_Seat_Row(Input : in Unbounded_String) return Seat_Row is
      Curr_Char : Character;
      Result : Seat_Row;
   begin
      Parser_Loop:
      for I in 1 .. Length(Input) loop
         Curr_Char := Element(Input, I);
         case Curr_Char is
            when '.' => Result(I) := Floor;
            when '#' => Result(I) := Occupied;
            when 'L' => Result(I) := Empty;
            when others =>
               Put_Line("Invalid character found! " & Curr_Char);
               Result(I) := Empty;
         end case;
      end loop Parser_Loop;
      return Result;
   end To_Seat_Row;

   procedure Load_File (Seats : out Seat_Map) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 1;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day11.input");
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Seats(Idx) := To_Seat_Row(Current_Line);
         Idx := Idx + 1;
      end loop;
      Close (File => Input);
   end Load_File;

   function Get_Occupied_Seats(Seats : in Seat_Map) return Integer is
      Result : Integer := 0;
   begin
      Row_Loop:
      for Row of Seats loop
         Column_Loop:
         for Column of Row loop
            if Column = Occupied then
               Result := Result + 1;
            end if;
         end loop Column_Loop;
      end loop Row_Loop;
      return Result;
   end Get_Occupied_Seats;

   function Valid_Coordinate(X : in Integer;
                             Y : in Integer;
                             Matrix : in Seat_Map) return Boolean is
   begin
      return X >= Matrix'First and then
        X <= Matrix'Last and then
        Y >= Matrix(Matrix'First)'First and then
        Y <= Matrix(Matrix'Last)'Last;
   end Valid_Coordinate;

   function Count_Number_Of_Occupied_Neighbours(Seats : in Seat_Map;
                                                X : in Integer;
                                                Y : in Integer) return Integer is
      Result : Integer := 0;
   begin
      X_Loop:
      for XS in (X - 1) .. (X + 1) loop
         Y_Loop:
         for YS in (Y - 1) .. (Y + 1) loop
            if Valid_Coordinate(XS, YS, Seats)
              and then not (XS = X and YS = Y)
              and then Seats(XS)(YS) = Occupied then
               Result := Result + 1;
            end if;
         end loop Y_Loop;
      end loop X_Loop;
      return Result;
   end Count_Number_Of_Occupied_Neighbours;

   function Maps_Equal(Current : in Seat_Map;
                       Previous : in Seat_Map) return Boolean is
      Differences : Integer := 0;
   begin
      X_Loop:
      for X in Current'Range loop
         Y_Loop:
         for Y in Current(X)'Range loop
            if Current(X)(Y) /= Previous(X)(Y) then
               Differences := Differences + 1;
            end if;
         end loop Y_Loop;
      end loop X_Loop;
      return Differences = 0;
   end Maps_Equal;

   -- Returns true if the map was changed in the step
   function Step(Seats : in out Seat_Map) return Boolean is
      Seats_Copy : Seat_Map := Seats;
      Neighbours : Integer;
   begin
      Row_Loop:
      for X in Seats_Copy'Range loop
         Column_Loop:
         for Y in Seats_Copy(X)'Range loop
            Neighbours := Count_Number_Of_Occupied_Neighbours(Seats_Copy, X, Y);
            if Seats_Copy(X)(Y) = Empty and then Neighbours = 0 then
               Seats(X)(Y) := Occupied;
            elsif Seats_Copy(X)(Y) = Occupied and then Neighbours >=4 then
               Seats(X)(Y) := Empty;
            end if;
         end loop Column_Loop;
      end loop Row_Loop;

      return Maps_Equal(Seats, Seats_Copy);
   end Step;

   procedure Print_Map(Seats : in Seat_Map) is
   begin
      if Enable_Debug then
         Put_Line("=======");
         Row_Loop:
         for Row of Seats loop
            Pos_Loop:
            for Pos of Row loop
               case Pos is
                  when Empty => Put('L');
                  when Occupied => Put('#');
                  when Floor => Put('.');
               end case;
            end loop Pos_Loop;
            Put_Line("");
         end loop Row_Loop;
      end if;
   end Print_Map;

   Waiting_Area : Seat_Map(1 .. Number_Of_Inputs);
   Answer : Integer;
begin
   Load_File(Waiting_Area);

   Game_Of_Life_Loop:
   while not step(Waiting_Area) loop
      Print_Map(Waiting_Area);
   end loop Game_Of_Life_Loop;

   Answer := Get_Occupied_Seats(Waiting_Area);

   Put_line("The number of occupied seats when stable is " & Integer'Image(Answer));
end Day11_1;
