with Ada.Text_IO; use Ada.Text_IO;
procedure Day02_2 is
   Data_Line_Length : constant Integer :=
     26; -- Number of characters on one line
   Data_Line_Amount : constant Integer :=
     250; -- Number of lines in the dataset
   subtype Data_Line is String (1 .. Data_Line_Length);
   type Data_Array is array (1 .. Data_Line_Amount) of Data_Line;

   procedure Load_File (Data : out Data_Array) is
      Current_Line : Data_Line;
      Input        : File_Type;
      Row_Idx      : Integer := 0;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day02.input");
      while not End_Of_File (Input) loop
         Current_Line   := Get_Line (Input);
         Row_Idx        := Row_Idx + 1;
         Data (Row_Idx) := Current_Line;
      end loop;
      Close (File => Input);
   end Load_File;

   function Find_Correct_Box (Data   : in  Data_Array;
                              Id     : in  Data_Line;
                              Result : out Data_Line) return Boolean
   is
      Number_Of_Matches : Integer;
   begin
      --Put_Line("Comparing all against " & Id);
      for L in Data'Range loop
         Number_Of_Matches := 0;
         for C in Id'Range loop
            if Id(C) = Data(L)(C) then
               Number_Of_Matches := Number_Of_Matches + 1;
            end if;
         end loop;
         if Number_Of_Matches = Data_Line_Length - 1 then
            Result := Data(L);
            return True;
         end if;
      end loop;

      return False;
   end Find_Correct_Box;

   Input_Data   : Data_Array := (others => (others => ' '));
   Current_Line : Data_Line;
   Correct_Box  : Data_Line;

   Answer_Idx : Integer := 0;
   Answer : String (1 .. Data_Line_Length - 1) := (others => ' ');
begin
   Load_File (Data => Input_Data);

   for L in Input_Data'Range loop
      Current_Line := Input_Data(L);
      exit when Find_Correct_Box(Data => Input_Data,
                          Id => Current_Line,
                          Result => Correct_Box);
   end loop;
   Put_Line("Found the strings: " & Current_Line & " and " & Correct_Box);

   for C in Current_Line'Range loop
      if Current_Line(C) = Correct_Box(C) then
         Answer_Idx := Answer_Idx + 1;
         Answer(Answer_Idx) := Current_Line(C);
      end if;
   end loop;

   Put_Line("The commonality between the strings is: " & Answer);
end Day02_2;
