with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day06_1 is
   -- ### CONSTANTS ### --
   Enable_Debug : constant Boolean := False;
   Max_Group_Size : constant Integer := 5; -- Should be enough

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;
   type Answer_Array is array (Character range 'a' .. 'z') of Boolean;
   type Group_Response is array (1 .. Max_Group_Size) of Answer_Array;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   function Count_Answers(Responses : in Group_Response) return Integer is
      Summary : Answer_Array := (others => False);
      Count : Integer := 0;
   begin
      Summarize_Loop:
      for I in Responses'Range loop
         Summary := Summary or Responses(I);
      end loop Summarize_Loop;

      Count_Loop:
      for J in Summary'Range loop
         if Summary(J) then
            Count := Count + 1;
         end if;
      end loop Count_Loop;

      Log_Debug("The number of questions answered by 'yes' is "
                & Integer'Image(Count));
      return Count;
   end Count_Answers;

   Answer : Integer := 0;

   Input        : File_Type;
   Current_Line : Data_Line;
   Idx          : Integer := 1;

   Current_Response : Group_Response := (others => (others => False));
begin
   Open (File => Input, Mode => In_File, Name => "data/day06.input");
   while not End_Of_File (Input) loop
      Current_Line := To_Unbounded_String(Get_Line(Input));
      -- New record
      if Current_Line = To_Unbounded_String("") then
         Answer := Answer + Count_Answers(Current_Response);
         Current_Response := (others => (others => False));
         Idx := 1;
      else
         Parser_Loop:
         for I in 1 .. Length(Current_Line) loop
            Current_Response(Idx)(Element(Current_Line, I)) := True;
         end loop Parser_Loop;
         Idx := Idx + 1;
      end if;
   end loop;
   -- Don't forget the last one
   Answer := Answer + Count_Answers(Current_Response);
   Close (File => Input);

   Put_line("The answer is " & Integer'Image(Answer));
end Day06_1;
