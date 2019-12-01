with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day01_1 is
   -- ### CONSTANTS ### --
   Data_Line_Amount : constant Integer := 100; -- Number of lines in the dataset

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Booster is record
      Mass : Integer;
      Fuel : Integer;
   end record;

   type Booster_Administration is array (1 .. Data_Line_Amount) of Booster;

   procedure Set_Required_Fuel(Subject : in out Booster) is
      Mass : Integer;
   begin
      Mass := Subject.Mass;
      Subject.Fuel := (Mass / 3) - 2;
   end Set_Required_Fuel;

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
         Idx := Idx + 1;
      end loop;
      Close (File => Input);
   end Load_File;

   function Calculate_Total (Boosters : in Booster_Administration) return Integer is
      Intermediate : Integer := 0;
   begin
      for I in Boosters'Range loop
         Intermediate := Intermediate + Boosters(I).Fuel;
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
end Day01_1;
