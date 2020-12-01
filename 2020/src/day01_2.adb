with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day01_2 is
   -- ### CONSTANTS ### --
   Number_Of_Inputs : constant Integer := 200;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;
   type Expense is range 0 .. 2020;
   type Expense_Report is array (1 .. Number_Of_Inputs) of Expense;

procedure Load_File (Expenses : out Expense_Report) is
   Input        : File_Type;
   Current_Line : Data_Line;
   Idx          : Integer := 1;
begin
   Open (File => Input, Mode => In_File, Name => "data/day01.input");
   while not End_Of_File (Input) loop
      Current_Line := To_Unbounded_String(Get_Line(Input));
      Expenses(Idx) := Expense'Value(To_String(Current_Line));
      Idx := Idx + 1;
   end loop;
   Close (File => Input);
end Load_File;

   Expenses : Expense_Report;
   Expected_Sum : constant Integer := 2020;
   Subject : Integer;
   Object : Integer;
   Abject : Integer; -- You need to call it something if you already have Subject and Object...

   Answer : Integer;
begin
   Load_File(Expenses);

   Subject_Loop:
   for I in Expenses'Range loop
      Subject := Integer(Expenses(I));
      Object_Loop:
      for J in Expenses'Range loop
         Object := Integer(Expenses(J));
         Abject_Loop:
         for K in Expenses'Range loop
            Abject := Integer(Expenses(K));
            if Subject /= Object and then
              Subject /= Abject and then
              Object /= Abject and then
              Subject + Object + Abject = Expected_Sum then
               exit Subject_Loop;
            end if;
         end loop Abject_Loop;
      end loop Object_Loop;
   end loop Subject_Loop;

   Answer := Subject * Object * Abject;
   Put_line("The answer is " & Integer'Image(Answer)
            & " based on " & Integer'Image(Subject)
            & " and " & Integer'Image(Object)
            & " and " & Integer'Image(Abject) & ".");
end Day01_2;
