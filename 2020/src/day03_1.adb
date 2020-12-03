with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day03_1 is
   -- ### CONSTANTS ### --
   Enable_Debug : constant Boolean := False;
   Number_Of_Inputs : constant Integer := 323;
   Input_Line_Length : constant Integer := 31;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is String (1 .. Input_Line_Length);

   type Data_File is array (1 .. Number_Of_Inputs) of Data_line;

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

   Forest : Data_File;

   X : Integer := 1;
   Curr_X : Integer := X;
   Occupation : Character;

   Answer : Integer := 0;
begin
   Load_File(The_Data => Forest);

   Step_Loop:
   for I in 1 .. Number_Of_Inputs loop
      Curr_X := X mod Input_Line_Length;
      if Curr_X = 0 then
         Curr_X := Input_Line_Length;
      end if;
      Occupation := Forest(I)(Curr_X);
      Log_Debug(Integer'Image(Curr_X));
      if Occupation = '#' then
         Log_Debug("Tree on line " & Integer'Image(I)
                   & " at column " & Integer'Image(X));
         Answer := Answer + 1;
      end if;
      X := X + 3;
   end loop Step_Loop;

   Put_line("The number of encountered trees is " & Integer'Image(Answer));
end Day03_1;
