with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day09_2 is
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

   procedure Find_Values(Sequence : in Input_Numbers;
                         Sum : in Long_Integer;
                         Min : out Long_Integer;
                         Max : out Long_Integer) is
      Intermediate_Sum : Long_Integer;
      Offset : Integer := 0;
      Min_Idx : Integer;
      Max_Idx : Integer;
   begin
      Begin_Loop:
      for I in Sequence'Range loop
         Intermediate_Sum := 0;
         Increment_Loop:
         for J in I .. Sequence'Last loop
            Intermediate_Sum := Intermediate_Sum + Sequence(J);
            if Intermediate_Sum = Sum then
               Log_Debug("Found indices "
                         & Integer'Image(I) & " .. " & Integer'Image(J));
               Min_Idx := I;
               Max_Idx := J;
               exit Begin_Loop;
            elsif Intermediate_Sum > Sum then
               exit Increment_Loop;
            end if;
         end loop Increment_Loop;
      end loop Begin_Loop;

      Max := 0;
      Min := Sum;
      Find_Values_Loop:
      for Val in Min_Idx .. Max_Idx loop
         if Sequence(Val) > Max then
            Max := Sequence(Val);
         elsif Sequence(Val) < Min then
            Min := Sequence(Val);
         end if;
      end loop Find_Values_Loop;
   end Find_Values;

   Stream : Input_Numbers;
   Sliding_Window : Window;
   Idx : Integer := 1;

   Invalid_Number : Long_Integer;
   Sequence_Min : Long_Integer;
   Sequence_Max : Long_Integer;
   Answer : Long_Integer := 0;
begin
   Load_File(Stream);

   Verify_Windows_Loop:
   for Idx in 1 .. Number_Of_Inputs - 1 loop
      Sliding_Window := Window(Stream(Idx .. (Idx + 24)));
      if not Verify_Value(Sliding_Window, Stream(25 + Idx)) then
         Invalid_Number := Stream(25 + Idx);
         exit Verify_Windows_Loop;
      end if;
   end loop Verify_Windows_Loop;

   Find_Values(Stream, Invalid_Number, Sequence_Min, Sequence_Max);

   Answer := Sequence_Min + Sequence_Max;

   Put_line("The sum of the lowest (" & Long_Integer'Image(Sequence_Min)
            & ") and highest (" & Long_Integer'Image(Sequence_Max)
            & ") number in the sequence is " & Long_Integer'Image(Answer));
   -- Too low 8427069
end Day09_2;
