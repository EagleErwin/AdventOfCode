with Ada.Text_IO;              use Ada.Text_IO;
procedure Day2_1 is
   Data_Line_Length : constant Integer := 26; -- Number of characters on one line
   Data_Line_Amount : constant Integer := 250; -- Number of lines in the dataset
   subtype Data_Line is String(1 .. Data_Line_Length);
   type Data_Array is array(1 .. Data_Line_Amount) of Data_Line;
   type Character_Administration_Array is array(Character range 'a' .. 'z') of Integer;
   
   procedure Load_File(Data : out Data_Array) is
      Current_Line : Data_Line;
      Input  : File_Type;
      Row_Idx : Integer := 0;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day2.input");
      while not End_Of_File (Input) loop
         Current_Line := Get_Line(Input);
         Row_Idx := Row_Idx + 1;
         Data(Row_Idx) := Current_Line;
   end loop;
   Close (File => Input);
   end Load_File;
   
   Input_Data : Data_Array := (others => (others => ' '));
   Administration : Character_Administration_Array;
   Current_Character : Character;
   Box_Two : Boolean;
   Box_Three : Boolean;

   Total_Twos : Integer := 0;
   Total_Threes : Integer := 0;
   
   Answer : Integer;
begin
   Load_File(Data => Input_Data);
   
   for L in Input_Data'Range loop
      Administration := (others => 0);
      Box_Two := False;
      Box_Three := False;
      for C in Input_Data(L)'Range loop
         Current_Character := Input_Data(L)(C);
         Administration(Current_Character) := Administration(Current_Character) + 1;
      end loop;
      for A in Administration'Range loop
         if Administration(A) = 2 then
            Box_Two := True;
            --Put_Line(A & " found two times");
         end if;
         if Administration(A) = 3 then
            Box_Three := True;
            --Put_Line(A & " found three times");
         end if;
      end loop;
      if Box_Two then
         Total_Twos := Total_Twos + 1;
      end if;
      if Box_Three then
         Total_Threes := Total_Threes + 1;
      end if;
   end loop;
   Put_Line("Twos: " & Integer'Image(Total_Twos) & "; Threes: " & Integer'Image(Total_Threes));
   Answer := Total_Twos * Total_Threes;
   Put_Line("Checksum: " & Integer'Image(Answer));
end Day2_1;
