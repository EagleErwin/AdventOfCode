with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day07_1 is
   -- ### CONSTANTS ### --

   -- ### TYPE DEFINITIONS ### --
   package Crab_Row is new Vectors(Index_Type   => Natural,
                                   Element_Type => Integer);
   use Crab_Row;

   procedure Load_File (Crabs : out Crab_Row.Vector) is
      Input         : File_Type;
      Current_Line  : Unbounded_String;
      Current_Value : Integer;

      Current_Char  : Character;
      Buffer : Unbounded_String := To_Unbounded_String("");
   begin
      Open (File => Input, Mode => In_File, Name => "data/day07.input");
      Current_Line := To_Unbounded_String(Get_Line(Input));

      for I in 1 .. Length(Current_Line) loop
         Current_Char := Element(Current_Line, I);
         if Current_Char = ',' then
            Current_Value := Integer'Value(To_String(Buffer));
            Crabs.Append(Current_Value);
            Buffer := To_Unbounded_String("");
         else
            Buffer := Buffer & Current_Char;
         end if;
      end loop;
      -- Don't forget the last value
      Current_Value := Integer'Value(To_String(Buffer));
      Crabs.Append(Current_Value);

      Close (File => Input);
   end Load_File;

   function Calculate_Required_Fuel (Crabs : in Crab_Row.Vector;
                                     Desired_Position : in Integer) return Integer is
      Result : Integer := 0;
   begin
      Calculation_Loop:
      for I in Crabs.Iterate loop
         Result := Result + Abs(Crabs(I)-Desired_Position);
      end loop Calculation_Loop;

      return Result;
   end Calculate_Required_Fuel;

   Crabs        : Crab_Row.Vector;

   Average_Position : Integer := 0;
   Max_Position     : Integer := 0;
   Current_Position : Integer;

   Min_Fuel  : Integer := Integer'Last;
   Fuel_Cost : Integer;

   Answer       : Integer := 0;
begin
   Load_File(Crabs);

   Get_Properties_Loop:
   for I in Crabs.Iterate loop
      Current_Position := Crabs(I);
      if Current_Position > Max_Position then
         Max_Position := Current_Position;
      end if;
      Average_Position := Average_Position + Current_Position;
   end loop Get_Properties_Loop;
   Average_Position := Average_Position / Integer(Crabs.Length);

   for N in 1 .. Max_Position loop
      Fuel_Cost := Calculate_Required_Fuel(Crabs, N);
      if Fuel_Cost < Min_Fuel then
         Min_Fuel := Fuel_Cost;
      end if;
   end loop;

   Answer := Min_Fuel;

   Put_line("A total of " & Integer'Image(Answer)
            & " fuel is required to align to the final position");
end Day07_1;
