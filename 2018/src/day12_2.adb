with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
procedure Day12_2 is
   Input_File_Name : constant String := "data/day12.input";
   Number_Of_Pots : constant Integer := 100; -- Number of pots in the input data
   Number_Of_Transformations : constant Integer := 32; -- The number of transformations in the input data
   Number_Of_Generations : constant Integer := 500; -- The number of generations to calculate

   type Pot is (Empty, Plant);
   type Pot_Pattern is array (1..5) of Pot;

   -- Every generation, an extra pot might be needed.
   -- In the end, two extra pots are required to match against the patterns.
   type Pot_Array is array(-(Number_Of_Generations + 2) .. Number_Of_Pots + Number_Of_Generations + 2) of Pot;

   type Transformation is record
      Pattern : Pot_Pattern;
      New_Pot : Pot;
   end record;

   type Transformation_Array is array(1 .. Number_Of_Transformations) of Transformation;

   function Create_Pot_Array (Input_String : in Unbounded_String) return Pot_Array is
      Result : Pot_Array := (others => Empty);
      Pots_Start_Idx : Integer := 16; -- The string starts with "initial state: "
      Pot_Char : Character;
   begin
      for P in Pots_Start_Idx .. Length (Input_String) loop
         Pot_Char := Element(Input_String, P);
         if Pot_Char = '.' then
            Result(P - Pots_Start_Idx) := Empty;
         elsif Pot_Char = '#' then
            Result(P - Pots_Start_Idx) := Plant;
         else
            Put_Line("Incorrect pot character " & Character'Image(Pot_Char)
                     & " in input data " & To_String(Input_String));
         end if;
      end loop;
      return Result;
   end Create_Pot_Array;

   function Create_Transformation (Input_String : in Unbounded_String)
                                   return Transformation is
      Result : Transformation;
      Pattern : Pot_Pattern := (others => Empty);
      New_State_Pos : constant Integer := 10;
      New_State : Pot;
      Pot_Char : Character;
   begin
      for C in 1 .. 5 loop
         Pot_Char := Element(Input_String, C);
         if Pot_Char = '.' then
            Pattern(C) := Empty;
         elsif Pot_Char = '#' then
            Pattern(C) := Plant;
         else
            Put_Line("Incorrect pot character " & Character'Image(Pot_Char)
                     & " in constraint " & To_String(Input_String));
         end if;
      end loop;

      Pot_Char := Element(Input_String, New_State_Pos);
      if Pot_Char = '.' then
         New_State := Empty;
      elsif Pot_Char= '#' then
         New_State := Plant;
      else
         Put_Line("Incorrect pot character " & Character'Image(Pot_Char)
                  & " in new state " & Character'Image(Element(Input_String, New_State_Pos)));
      end if;

      Result := Transformation'(Pattern => Pattern, New_Pot => New_State);

      return Result;
   end Create_Transformation;

   procedure Load_File (Garden : out Pot_Array;
                        Transformations : out Transformation_Array) is
      Input          : File_Type;
      Current_Line   : Unbounded_String;
      Transformation_Idx : Integer := 1;
   begin
      Open (File => Input, Mode => In_File, Name => Input_File_Name);
      Current_Line  := To_Unbounded_String(Get_Line (Input));
      Garden := Create_Pot_Array(Input_String => Current_Line);

      Current_Line := To_Unbounded_String(Get_Line(Input)); -- Empty line

      while not End_Of_File (Input) loop
         Current_Line  := To_Unbounded_String(Get_Line (Input));
         Transformations(Transformation_Idx) := Create_Transformation(Input_String => Current_Line);
         Transformation_Idx := Transformation_Idx + 1;
      end loop;

      Close (File => Input);
   end Load_File;

   procedure Print_Pattern(Pattern : in Pot_Pattern) is
   begin
      for P in Pattern'Range loop
         if Pattern(P) = Empty then
            Put(".");
         else
            Put("#");
         end if;
      end loop;
      Put_Line("");
   end Print_Pattern;

   function Verify_Pattern(Pattern : in Pot_Pattern;
                           Current_Pots : in Pot_Pattern) return Boolean is
   begin
      for I in Pot_Pattern'Range loop
         if Pattern(I) /= Current_Pots(I) then
            return False;
         end if;
      end loop;
      return True;
   end Verify_Pattern;

   procedure Calculate_Next_Generation(Pots : in out Pot_Array;
                                       Transformations : in Transformation_Array) is
      Next_Gen : Pot_Array := Pots;
      Subset : Pot_Pattern := (others => Empty);
   begin
      Pot_Loop:
      for Pot_Id in -Number_Of_Generations .. Number_Of_Pots + Number_Of_Generations loop
         if (Pot_Id > Pots'Last - Pot_Pattern'Last) then
            exit Pot_Loop;
         end if;
         Constraints_Loop:
         for C in Transformations'Range loop
            Subset(1) := Pots(Pot_Id - 2);
            Subset(2) := Pots(Pot_Id - 1);
            Subset(3) := Pots(Pot_Id);
            Subset(4) := Pots(Pot_Id + 1);
            Subset(5) := Pots(Pot_Id + 2);
            if Verify_Pattern(Pattern => Transformations(C).Pattern,
                              Current_Pots => Subset) then
               Next_Gen(Pot_Id) := Transformations(C).New_Pot;
               exit Constraints_Loop;
            end if;
         end loop Constraints_Loop;

      end loop Pot_Loop;

      Pots := Next_Gen;
   end Calculate_Next_Generation;

   Input_Pots  : Pot_Array;
   Transformations : Transformation_Array;
   Current_Generation : Integer := 0;
   Result : Long_Integer := 0;

   Number_Of_Plants : Integer := 0;
   Generation_For_Answer : Long_Integer := 50_000_000_000;
begin
   Load_File (Garden          => Input_Pots,
              Transformations => Transformations);

   while Current_Generation < Number_Of_Generations loop
      Current_Generation := Current_Generation + 1;
      Calculate_Next_Generation(Pots => Input_Pots,
                                Transformations => Transformations);
   end loop;

   -- After 500 days, the plants only shift to the right (probably earlier).
   -- So from this point, every generation, every plant gets one index number higher.
   -- The total answer becomes #plants higher every generation.

   for P in Input_Pots'Range loop
      if Input_Pots(P) = Empty then
         Put(".");
      else
         Put("#");
         Result := Result + Long_Integer(P);
         Number_Of_Plants := Number_Of_Plants + 1;
      end if;
   end loop;
   Put_Line("");
   Put_Line("After" & Integer'Image(Current_Generation)
            & " generations, the number of pots is" & Integer'Image(Number_Of_Plants)
            & " with a score of" & Long_Integer'Image(Result));
   Put_Line("Adding a score of " & Integer'Image(Number_Of_Plants) & " for the remaining generations");

   Result := Result + ((Generation_For_Answer - Long_Integer(Current_Generation)) * Long_Integer(Number_Of_Plants));

   Put_Line("Answer:" & Long_Integer'Image(Result));
end Day12_2;
