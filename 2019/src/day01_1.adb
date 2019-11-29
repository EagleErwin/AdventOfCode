with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day01_1 is
   subtype Data_Line is Unbounded_String;

   procedure Load_File (Contents : out Data_Line) is
      Input         : File_Type;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day01.input");
      Contents := To_Unbounded_String(Get_Line(File => Input));
      Close (File => Input);
   end Load_File;

   File_Contents : Data_Line;
begin
   Load_File (Contents => File_Contents);
   Put_Line (To_String(File_Contents));
end Day01_1;
