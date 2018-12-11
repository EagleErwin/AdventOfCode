with Ada.Text_IO;           use Ada.Text_IO;
procedure Day11_2 is
   X_Offset : constant Integer := 10;

   subtype Data_Line is String (1..4);

   subtype Coordinate is Integer range 1 .. 300;

   type Fuel_Cell is record
      X : Coordinate;
      Y : Coordinate;
      Power_Level : Integer;
   end record;

   type Access_Fuel_Cell is access all Fuel_Cell;

   type Fuel_Cell_Array is array (Coordinate) of Access_Fuel_Cell;
   type Battery is array (Coordinate) of Fuel_Cell_Array;

   procedure Set_Power_Level(Cell : in out Access_Fuel_Cell;
                             Serial_Number : in Integer) is
      Rack_Id : Integer;
      Power_Level_Start : Integer;
      Intermediate_Power_Level_1 : Integer;
      Intermediate_Power_Level_2 : Integer;
      Power_Level : Integer;
      Percent : Integer;
   begin
      Rack_Id := Cell.X + X_Offset;
      Power_Level_Start := Rack_Id * Cell.Y;
      Intermediate_Power_Level_1 := Power_Level_Start + Serial_Number;
      Intermediate_Power_Level_2 := Intermediate_Power_Level_1 * Rack_Id;
      if Intermediate_Power_Level_2 > 100 then
         Percent := Intermediate_Power_Level_2 / 100;
         Power_Level := Integer'Value((1 => Integer'Image(Percent)(Integer'Image(Percent)'Last)));
      else
         Power_Level := 0;
      end if;
      Cell.Power_Level := Power_Level - 5;

   end Set_Power_Level;

   procedure Load_File (Serial_Number : out Integer) is
      Input         : File_Type;
      Current_Line  : Data_Line;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day11.input");
      Current_Line  := Get_Line (Input);
      Serial_Number := Integer'Value(Current_Line);
      Close (File => Input);
   end Load_File;

   function Calculate_Area_Power(Power_Supply : in Battery;
                                 X, Y : in Coordinate;
                                 Grid_Dimension : in Coordinate) return Integer is
      Power : Integer := 0;
      Diff : Integer := Grid_Dimension - 1;
   begin
      if (X + Diff > 300) or (Y + Diff > 300) then
         return -5 * 9; --Minimal power: invalid
      end if;

      for I in Integer range Y .. (Y + Diff) loop
         for J in Integer range X .. (X + Diff) loop
            Power := Power + Power_Supply(I)(J).Power_Level;
         end loop;
      end loop;

      return Power;
   end Calculate_Area_Power;

   function Find_Best_Area (Power_Supply : in Battery) return String is
      Best_Power_Level : Integer := -5 * 9; --If all cells have their minimal value
      Current_Power_Level : Integer;
      X_Coordinate : Coordinate := 1;
      Y_Coordinate : Coordinate := 1;
      Best_X : Coordinate;
      Best_Y : Coordinate;
      Best_N : Coordinate;
   begin
      for N in Integer range 1 .. 300 loop
         Put_Line("Finding best area of size" & Integer'Image(N));
         for I in Integer range 1 .. 300 - (N-1) loop
            for J in Integer range 1 .. 300 - (N-1) loop
               Current_Power_Level := Calculate_Area_Power(Power_Supply, I, J, N);
               if Current_Power_Level > Best_Power_Level then
                  Best_X := I;
                  Best_Y := J;
                  Best_N := N;
                  Best_Power_Level := Current_Power_Level;
               end if;
            end loop;
         end loop;
      end loop;
      return Integer'Image(Best_X) & "," & Integer'Image(Best_Y) & "," & Integer'Image(Best_N);
   end Find_Best_Area;

   Serial_Number : Integer;
   Power_Supply : Battery := (others => (others => null));
   Current_Fuel_Cell : Access_Fuel_Cell;
begin
   Load_File (Serial_Number => Serial_Number);

   Row_Loop:
   for R in Power_Supply'Range loop
      Column_Loop:
      for C in Power_Supply(R)'Range loop
         Current_Fuel_Cell := new Fuel_Cell'(X           => C,
                                             Y           => R,
                                             Power_Level => 0);
         Set_Power_Level(Current_Fuel_Cell, Serial_Number);
         Power_Supply(R)(C) := Current_Fuel_Cell;
      end loop Column_Loop;
   end loop Row_Loop;

   Put_Line(Find_Best_Area(Power_Supply => Power_Supply));
end Day11_2;
