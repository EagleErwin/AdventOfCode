with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
procedure Day14_2 is
   Rounds_After_Input : constant Integer := 10;

   subtype Recipe_Score is Integer range 0 .. 9;

   type Recipe;
   type Recipe_Access is access all Recipe;
   type Recipe is record
      Score : Recipe_Score;
      Prev : Recipe_Access;
      Next : Recipe_Access;
   end record;

   type Score_Array is array (1 .. 2) of Recipe_Score;

   procedure Load_File (Input_Number : out String) is
      Input         : File_Type;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day14.input");
      Input_Number  := Get_Line (Input);
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

   function Check_Pattern (Last_Recipe : in Recipe_Access;
                           Search_Pattern : in String) return Boolean is
      Buffer : Unbounded_String;
      Current_Recipe : Recipe_Access;
   begin
      Current_Recipe := Last_Recipe;
      for I in 1 .. Search_Pattern'Length loop
         Buffer := Trim (Recipe_Score'Image(Current_Recipe.Score), Both) & Buffer;
         Current_Recipe := Current_Recipe.Prev;
      end loop;

      if Search_Pattern = To_String(Buffer) then
         return True;
      end if;
      return False;
   end Check_Pattern;

   function Add_Recipe (First_Recipe   : in out Recipe_Access;
                        Second_Recipe : in out Recipe_Access;
                        Last_Recipe   : in out Recipe_Access;
                        Finished : out Boolean;
                        Search_Pattern : in String;
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
         -- Check if the first insetion already matches the pattern.
         Finished := Check_Pattern(Last_Recipe    => Last_Recipe.Prev,
                                   Search_Pattern => Search_Pattern);
         if Finished then
            Number_Of_Created_Recipes := 1;
         else
            Finished := Check_Pattern(Last_Recipe => Last_Recipe.Prev,
                                      Search_Pattern => Search_Pattern);
            Number_Of_Created_Recipes := 2;
         end if;
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
         Finished := Check_Pattern(Last_Recipe    => Last_Recipe,
                                   Search_Pattern => Search_Pattern);
         Number_Of_Created_Recipes := 1;
      end if;
      Update_Recipe(First_Recipe);
      Update_Recipe(Second_Recipe);

      return Number_Of_Created_Recipes;
   end Add_Recipe;

   Input_Number : String (1..6);
   First_Elf_Recipe : Recipe_Access;
   Second_Elf_Recipe : Recipe_Access;
   Last_Recipe : Recipe_Access;
   Created_Recipes : Integer;
   Finished : Boolean := False;
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

   while not Finished and Created_Recipes < 1_000_000_000 loop
      Created_Recipes := Created_Recipes + Add_Recipe(First_Recipe  => First_Elf_Recipe,
                             Second_Recipe => Second_Elf_Recipe,
                             Last_Recipe => Last_Recipe,
                             Finished => Finished,
                             Search_Pattern => Input_Number);
   end loop;

   if not Finished then
      Put_Line("ERROR! Pattern not found after " & Integer'Image(Created_Recipes) & " created recipes.");
   else
      Put_Line("The pattern " & Input_Number & " occurs first after " & Integer'Image(Created_Recipes - Input_Number'Length) & " recipes.");
   end if;

end Day14_2;
