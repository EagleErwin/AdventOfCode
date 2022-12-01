with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day01_2 is
   type Values_Array is array (1..3) of Integer;
   Input          : File_Type;
   Current_Line   : Unbounded_String;
   Current_Value  : Integer := 0;
   Maximum_Values : Values_Array := (others => 0);
   Answer         : Integer := 0;
begin
   Open (File => Input, Mode => In_File, Name => "data/day01.input");
   while not End_Of_File (Input) loop
      Current_Line := To_Unbounded_String(Get_Line(Input));
      if (Current_Line = "") then
         if (Maximum_Values(1) < Current_Value) then
            Maximum_Values(3) := Maximum_Values(2);
            Maximum_Values(2) := Maximum_Values(1);
            Maximum_Values(1) := Current_Value;
         elsif (Maximum_Values(2) < Current_Value) then
            Maximum_Values(3) := Maximum_Values(2);
            Maximum_Values(2) := Current_Value;
         elsif (Maximum_Values(3) < Current_Value) then
            Maximum_Values(3) := Current_Value;
         end if;
         Current_Value := 0;
      else
         Current_Value := Current_Value + Integer'Value(To_String(Current_Line));
      end if;
   end loop;
   Close (File => Input);

   Answer := Maximum_Values(1) + Maximum_Values(2) + Maximum_Values(3);

   Put_line("The three Elves carrying the most callories carry "
            & Integer'Image(Answer) & " calories in total.");
end Day01_2;
