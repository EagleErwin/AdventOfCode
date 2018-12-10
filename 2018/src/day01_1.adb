with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day01_1 is
   Input  : File_Type;
   Answer : Integer := 0;
begin
   Open (File => Input, Mode => In_File, Name => "data/day01.input");
   loop
      declare
         Frequency : Integer;
         Line : Unbounded_String := To_Unbounded_String (Get_Line (Input));
      begin
         Frequency := Integer'Value (To_String (Line));
         Answer    := Answer + Frequency;
      end;
   end loop;

exception
   when End_Error =>
      if Is_Open (Input) then
         Close (Input);
      end if;

      Put_Line (Integer'Image (Answer));
end Day01_1;
