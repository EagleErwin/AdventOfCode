with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day03_2 is
   -- ### CONSTANTS ### --
   Enable_Debug : constant Boolean := False;
   Number_Of_Inputs : constant Integer := 323;
   Input_Line_Length : constant Integer := 31;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is String (1 .. Input_Line_Length);

   type Data_File is array (1 .. Number_Of_Inputs) of Data_line;
   type Answer_Array is array (1 .. 5) of Integer;

   -- ### FUNCTIONS AND PROCEDURES ### --
   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   procedure Load_File (The_Data : out Data_File) is
      Input        : File_Type;
      Idx          : Integer := 1;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day03.input");
      while not End_Of_File (Input) loop
         The_Data(Idx) := Get_Line(Input);
         Idx := Idx + 1;
      end loop;
      Close (File => Input);
   end Load_File;

   function Iterate (Forest : in Data_File;
                     X_Step : in Integer;
                     Y_Step : in Integer) return Integer is
      X : Integer := 1;
      Curr_X : Integer := X;
      Occupation : Character;
      Number_Of_Trees : Integer := 0;
   begin
      Step_Loop:
      for I in 1 .. Number_Of_Inputs loop
         if ((I-1) mod Y_Step) = 0 then -- Skip line if Y_Step says so
            Curr_X := X mod Input_Line_Length;
            if Curr_X = 0 then
               Curr_X := Input_Line_Length;
            end if;
            Occupation := Forest(I)(Curr_X);
            Log_Debug(Integer'Image(Curr_X));
            if Occupation = '#' then
               Log_Debug("Tree on line " & Integer'Image(I)
                         & " at column " & Integer'Image(X));
               Number_Of_Trees := Number_Of_Trees + 1;
            end if;
            X := X + X_Step;
         else
            Log_Debug("Skipping line " & Integer'Image(I));
            Log_Debug("I mod Y_Step = " & Integer'Image(I mod Y_Step));
         end if;
      end loop Step_Loop;
      Log_Debug("Found a total of " & Integer'Image(Number_Of_Trees)
                & " trees on this iteration");
      return Number_Of_Trees;
   end Iterate;


   Forest : Data_File;

   Results : Answer_Array;
   Answer : Long_Integer := 1;
begin
   Load_File(The_Data => Forest);

   Results(1) := Iterate(Forest => Forest, X_Step => 1, Y_Step => 1);
   Results(2) := Iterate(Forest => Forest, X_Step => 3, Y_Step => 1);
   Results(3) := Iterate(Forest => Forest, X_Step => 5, Y_Step => 1);
   Results(4) := Iterate(Forest => Forest, X_Step => 7, Y_Step => 1);
   Results(5) := Iterate(Forest => Forest, X_Step => 1, Y_Step => 2);

   for I in 1 .. 5 loop
      Answer := Answer * Long_Integer(Results(I));
   end loop;

   Put_line("The multiplication of the encountered trees is "
            & Long_Integer'Image(Answer));
end Day03_2;
