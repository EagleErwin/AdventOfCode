with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
procedure Day14_1 is
   Rounds_After_Input : constant Integer := 10;

   subtype Data_Line is Unbounded_String;
   subtype Recipe_Score is Integer range 0 .. 9;

   type Recipe;
   type Recipe_Access is access all Recipe;
   type Recipe is record
      Score : Recipe_Score;
      Prev : Recipe_Access;
      Next : Recipe_Access;
   end record;

   type Score_Array is array (1 .. 2) of Recipe_Score;

   procedure Load_File (Input_Number : out Integer) is
      Input         : File_Type;
      Current_Line  : Data_Line;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day14.input");
      Current_Line  := To_Unbounded_String (Get_Line (Input));
      Input_Number := Integer'Value(To_String(Current_Line));
      Close (File => Input);
   end Load_File;

   procedure Update_Recipe (Subject : in out Recipe_Access) is
      Current_Score : Recipe_Score;
      Number_Of_Steps : Integer;
   begin
      Current_Score := Subject.Score;
      Number_Of_Steps := Integer(Current_Score + 1);
      for I in 1 .. Number_Of_Steps loop
         Subject := Subject.Next;
      end loop;
   end Update_Recipe;

   function Add_Recipe (First_Recipe   : in out Recipe_Access;
                         Second_Recipe : in out Recipe_Access;
                         Last_Recipe   : in out Recipe_Access;
                         Print_Scores  : in Boolean := False) return Integer is
      New_First_Recipe : Recipe_Access;
      New_Second_Recipe : Recipe_Access;

      Current_First_Score : Recipe_Score;
      Current_Second_Score : Recipe_Score;
      Score_Sum : Integer;

      Number_Of_Created_Recipes : Integer;
   begin
      Current_First_Score := First_Recipe.Score;
      Current_Second_Score := Second_Recipe.Score;
      Score_Sum := Integer(Current_First_Score + Current_Second_Score);
      if Score_Sum > Recipe_Score'Last then
         Current_First_Score := Recipe_Score(Score_Sum / 10);
         Current_Second_Score := Recipe_Score(Score_Sum - 10);
         New_First_Recipe := new Recipe'(Score => Current_First_Score,
                                         Prev  => Last_Recipe,
                                         Next  => null);
         New_Second_Recipe := new Recipe'(Score => Current_Second_Score,
                                          Prev  => New_First_Recipe,
                                          Next  => Last_Recipe.Next);
         -- Reconstruct the chain with the new elements.
         New_First_Recipe.Next := New_Second_Recipe;
         Last_Recipe.Next.Prev := New_Second_Recipe;
         Last_Recipe.Next := New_First_Recipe;
         Last_Recipe := New_Second_Recipe;
         if Print_Scores then
            Put(Recipe_Score'Image(New_First_Recipe.Score));
            Put(Recipe_Score'Image(New_Second_Recipe.Score));
         end if;
         Number_Of_Created_Recipes := 2;
      else
         Current_First_Score := Recipe_Score(Score_Sum);
         New_First_Recipe := new Recipe'(Score => Current_First_Score,
                                         Prev  => Last_Recipe,
                                         Next  => Last_Recipe.Next);
         -- Reconstruct the chain with the new element.
         Last_Recipe.Next.Prev := New_First_Recipe;
         Last_Recipe.Next := New_First_Recipe;
         Last_Recipe := New_First_Recipe;
         if Print_Scores then
            Put(Recipe_Score'Image(New_First_Recipe.Score));
         end if;
         Number_Of_Created_Recipes := 1;
      end if;
      Update_Recipe(First_Recipe);
      Update_Recipe(Second_Recipe);
      return Number_Of_Created_Recipes;
   end Add_Recipe;

   Input_Number : Integer;
   First_Elf_Recipe : Recipe_Access;
   Second_Elf_Recipe : Recipe_Access;
   Last_Recipe : Recipe_Access;
   Created_Recipes : Integer;
begin
   Load_File (Input_Number => Input_Number);

   -- Initialize the chain
   First_Elf_Recipe := new Recipe'(Score => 3,
                               Prev  => null,
                               Next  => null);
   Second_Elf_Recipe := new Recipe'(Score => 7,
                                Prev  => First_Elf_Recipe,
                                Next  => First_Elf_Recipe);
   First_Elf_Recipe.Next := Second_Elf_Recipe;
   First_Elf_Recipe.Prev := Second_Elf_Recipe;

   Last_Recipe := Second_Elf_Recipe;

   Created_Recipes := 2;

   -- First, calculate the first steps until we are at the input value.
   while Created_Recipes < Input_Number loop
      Created_Recipes := Created_Recipes + Add_Recipe(First_Recipe  => First_Elf_Recipe,
                                                      Second_Recipe => Second_Elf_Recipe,
                                                      Last_Recipe => Last_Recipe);
      if Created_Recipes > Input_Number then
         Put(Recipe_Score'Image(Last_Recipe.Score));
      end if;
   end loop;

   -- Now, calculate the next 10
   while Created_Recipes < Input_Number + Rounds_After_Input loop
      Created_Recipes := Created_Recipes + Add_Recipe(First_Recipe  => First_Elf_Recipe,
                                                      Second_Recipe => Second_Elf_Recipe,
                                                      Last_Recipe => Last_Recipe,
                                                      Print_Scores => True);
      if Created_Recipes > Input_Number + Rounds_After_Input then
         Put_Line("");
         Put_Line("I calculated one recipe too much. Ignore the last number!");
      end if;
   end loop;
   Put_Line("");
end Day14_1;
