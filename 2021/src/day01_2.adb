with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day01_2 is
   Input          : File_Type;
   Current_Line   : Unbounded_String;
   -- Large value, so the first value is never larger
   Previous_Value : Integer := Integer'Last;
   Current_Value  : Integer;
   Element_1      : Integer;
   Element_2      : Integer;
   Element_3      : Integer;
   Answer         : Integer := 0;
begin
   Open (File => Input, Mode => In_File, Name => "data/day01.input");
   Current_Line := To_Unbounded_String(Get_Line(Input));
   Element_1 := Integer'Value(To_String(Current_Line));
   Current_Line := To_Unbounded_String(Get_Line(Input));
   Element_2 := Integer'Value(To_String(Current_Line));
   while not End_Of_File (Input) loop
      Current_Line := To_Unbounded_String(Get_Line(Input));
      Element_3 := Integer'Value(To_String(Current_Line));

      Current_Value := Element_1 + Element_2 + Element_3;
      if (Current_Value > Previous_Value) then
         Answer := Answer + 1;
      end if;
      Previous_Value := Current_Value;
      Element_1 := Element_2;
      Element_2 := Element_3;
   end loop;
   Close (File => Input);

   Put_line("A total of " & Integer'Image(Answer)
            & " measurements in the three-measurement sliding window is"
            & " larger than the previous value.");
end Day01_2;
