with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day01_2 is
   -- ### CONSTANTS ### --
   Data_Line_Amount : constant Integer := 100; -- Number of lines in the dataset

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Booster is record
      Mass       : Integer; -- The mass of this booster module
      Fuel       : Integer; -- The fuel required for the empty module
      Total_Fuel : Integer; -- The total fuel required for the filled module
   end record;

   type Booster_Administration is array (1 .. Data_Line_Amount) of Booster;

   -- Calculate and set the required fuel for an empty booster module
   procedure Set_Required_Fuel(Subject : in out Booster) is
      Mass : Integer;
   begin
      Mass := Subject.Mass;
      Subject.Fuel := (Mass / 3) - 2;
   end Set_Required_Fuel;

   -- Calculate and set the required fuel for the filled booster module
   procedure Set_Total_Fuel(Subject : in out Booster) is
      Mass           : Integer;
      Fuel_Mass      : Integer;
      Fuel_Iteration : Integer;
   begin
      Mass := Subject.Mass;
      Fuel_Mass := 0;
      Fuel_Iteration := (Subject.Fuel / 3) - 2;
      while Fuel_Iteration > 0 loop
         Fuel_Mass := Fuel_Mass + Fuel_Iteration;
         Fuel_Iteration := (Fuel_Iteration / 3) - 2;
      end loop;
      Subject.Total_Fuel := Fuel_Mass + Subject.Fuel;
   end Set_Total_Fuel;

   -- Load the input data and construct the Booster Administration
   procedure Load_File (Boosters : out Booster_Administration) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 1;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day01.input");
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Boosters(Idx).Mass := Integer'Value(To_String(Current_Line));
         Set_Required_Fuel(Boosters(Idx));
         Set_Total_Fuel(Boosters(Idx));
         Idx := Idx + 1;
      end loop;
      Close (File => Input);
   end Load_File;

   -- Calculate the total amount of fuel for the whole set of boosters
   function Calculate_Total (Boosters : in Booster_Administration) return Integer is
      Intermediate : Integer := 0;
   begin
      for I in Boosters'Range loop
         Intermediate := Intermediate + Boosters(I).Total_Fuel;
      end loop;
      return Intermediate;
   end Calculate_Total;

   File_Contents : Data_Line;
   Boosters      : Booster_Administration := (others => (others => 0));
   Answer        : Integer;
begin
   Load_File (Boosters => Boosters);
   Answer := Calculate_Total(Boosters => Boosters);
   Put_Line (Integer'Image(Answer));
end Day01_2;
