with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day01_2 is 
   Data_Line_Amount : constant Integer := 1040; -- Number of lines in the dataset
   Min_Intermediate : constant Integer := -1000000;
   Max_Intermediate : constant Integer := 1000000;

   subtype Data_Line is Unbounded_String;

   type Data_Array is array (1 .. Data_Line_Amount) of Integer;
   type Administration_Array is array (Min_Intermediate .. Max_Intermediate) of Boolean;
   
   procedure Load_File (Data : out Data_Array) is
      Current_Line      : Data_Line;
      Current_Frequency : Integer;
      Input             : File_Type;
      Row_Idx           : Integer := 0;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day01.input");
      while not End_Of_File (Input) loop
         Current_Line      := To_Unbounded_String (Get_Line (Input));
         Current_Frequency := Integer'Value (To_String (Current_Line));
         Row_Idx           := Row_Idx + 1;
         Data (Row_Idx)    := Current_Frequency;
      end loop;
      Close (File => Input);
   end Load_File;

   function Do_Loop (Intermediates : in out Administration_Array;
                     Data : in Data_Array;
                     Current_Frequency : in out Integer) return Boolean is
   begin
      for L in Data'Range loop
         Current_Frequency := Current_Frequency + Data(L);
         if Intermediates(Current_Frequency) then
            Put_Line("Finished!" & Integer'Image(Current_Frequency));
            return True;
         end if;
         
         Intermediates(Current_Frequency) := True;
      end loop;
      return False;
   end Do_Loop;
   
   Frequency : Integer := 0;
   Input_Data   : Data_Array;
   Administration : Administration_Array := (others => False);
begin
   Load_File (Data => Input_Data);

   while not Do_Loop(Administration, Input_Data, Frequency) loop
      Put_Line("Not finished, looping around. Starting with " & Integer'Image(Frequency));
   end loop;
end Day01_2;
