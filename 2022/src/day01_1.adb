with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day01_1 is
   Input          : File_Type;
   Current_Line   : Unbounded_String;
   Current_Value  : Integer := 0;
   Maximum_Value  : Integer := 0;
   Answer         : Integer := 0;
begin
   Open (File => Input, Mode => In_File, Name => "data/day01.input");
   while not End_Of_File (Input) loop
      Current_Line := To_Unbounded_String(Get_Line(Input));
      if (Current_Line = "") then
         if (Maximum_Value < Current_Value) then
            Maximum_Value := Current_Value;
         end if;
         Current_Value := 0;
      else
         Current_Value := Current_Value + Integer'Value(To_String(Current_Line));
      end if;
   end loop;
   Close (File => Input);

   Answer := Maximum_Value;

   Put_line("The Elf carrying the most callories carries "
            & Integer'Image(Answer) & " calories.");
end Day01_1;
