with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day01_1 is
   Input          : File_Type;
   Current_Line   : Unbounded_String;
   -- Large value, so the first value is never larger
   Previous_Value : Integer := Integer'Last;
   Current_Value  : Integer;
   Answer         : Integer := 0;
begin
   Open (File => Input, Mode => In_File, Name => "data/day01.input");
   while not End_Of_File (Input) loop
      Current_Line := To_Unbounded_String(Get_Line(Input));
      Current_Value := Integer'Value(To_String(Current_Line));
      if (Current_Value > Previous_Value) then
         Answer := Answer + 1;
      end if;
      Previous_Value := Current_Value;
   end loop;
   Close (File => Input);

   Put_line("A total of " & Integer'Image(Answer)
            & " measurements is larger than the previous value.");
end Day01_1;
