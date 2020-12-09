with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day09_1 is
   -- ### CONSTANTS ### --
   Enable_Debug : constant Boolean := False;
   Number_Of_Inputs : constant Integer := 1_000;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Input_Numbers is array (1 .. Number_Of_Inputs) of Long_Integer;
   type Window is array ( 1 .. 25 ) of Long_Integer;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   procedure Load_File (Stream : out Input_Numbers) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 1;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day09.input");
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Stream(Idx) := Long_Integer'Value(To_String(Current_Line));
         Idx := Idx + 1;
      end loop;
      Close (File => Input);
   end Load_File;

   function Verify_Value(Sequence : in Window;
                         Value : in Long_Integer) return Boolean is
   begin
      First_Value_Loop:
      for I of Sequence loop
         Second_Value_Loop:
         for J of Sequence loop
            if I /= J and then (I + J) = Value then
               return True;
            end if;
         end loop Second_Value_Loop;
      end loop First_Value_Loop;
      return False;
   end Verify_Value;

   Stream : Input_Numbers;
   Sliding_Window : Window;
   Idx : Integer := 1;
   Answer : Long_Integer := 0;
begin
   Load_File(Stream);

   Verify_Windows_Loop:
   while Idx < Number_Of_Inputs loop
      Sliding_Window := Window(Stream(Idx .. (Idx + 24)));
      if not Verify_Value(Sliding_Window, Stream(25 + Idx)) then
         Answer := Stream(25 + Idx);
         exit Verify_Windows_Loop;
      end if;
      Idx := Idx + 1;
   end loop Verify_Windows_Loop;

   Put_line("The first number that is not valid is " & Long_Integer'Image(Answer));
end Day09_1;
