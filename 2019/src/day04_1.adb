with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;

procedure Day04_1 is
   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is String (1 .. 13);
   subtype Value_String is String (1 .. 6);

   procedure Load_File (Lower_Bound : out Integer; Upper_Bound : out Integer) is
      Input        : File_Type;
      Input_String : Data_Line;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day04.input");
      Input_String := Get_Line(Input);
      Close (File => Input);

      Lower_Bound := Integer'Value(Head(Input_String, 6));
      Upper_Bound := Integer'Value(Tail(Input_String, 6));
   end Load_File;

   -- Checks if two adjacent digits are the same (like 22 in 122345).
   function Check_1 (Input : in Integer) return Boolean is
      Input_String : Value_String := Trim(Integer'Image(Input), Ada.Strings.Left);
   begin
      for I in 1 .. 5 loop
         if Input_String(I) = Input_String(I + 1) then
            return True;
         end if;
      end loop;

      return False;
   end Check_1;

   -- Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
   function Check_2 (Input : in Integer) return Boolean is
      Input_String : Value_String := Trim(Integer'Image(Input), Ada.Strings.Left);
   begin
      for I in 1 .. 5 loop
         if Integer'Value((1 => Input_String(I))) > Integer'Value((1 => Input_String(I + 1))) then
            return False;
         end if;
      end loop;

      return True;
   end Check_2;

   function Solve (Lower_Bound : in Integer;
                   Upper_Bound : in Integer) return Integer is
      Answer : Integer := 0;
   begin
      for Sample in Lower_Bound .. Upper_Bound loop
         if Check_1(Sample) and Check_2(Sample) then
            Answer := Answer + 1;
         end if;
      end loop;

      return Answer;
   end Solve;

   Lower_Bound : Integer;
   Upper_Bound : Integer;

   Answer   : Integer := 0;
begin
   Load_File (Lower_Bound, Upper_Bound);
   Answer := Solve (Lower_Bound, Upper_Bound);
   Put_Line("Answer:" & Integer'Image(Answer));
end Day04_1;
