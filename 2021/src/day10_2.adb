with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day10_2 is
   -- ### CONSTANTS ### --

   -- ### TYPE DEFINITIONS ### --
   package Code_Line is new Vectors(Index_Type   => Natural,
                                    Element_Type => Character);
   use Code_Line;

   package Program is new Vectors(Index_Type   => Natural,
                                  Element_Type => Code_Line.Vector);
   use Program;

   package Scores is new Vectors(Index_Type => Natural,
                                 Element_Type => Long_Integer);
   use Scores;

   package Score_Sorter is new Scores.Generic_Sorting;
   use Score_Sorter;

   procedure Load_File (Navigation_Subsystem : out Program.Vector) is
      Input             : File_Type;
      Current_Line      : Unbounded_String;
      Current_Code_Line : Code_Line.Vector;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day10.input");
      while not End_Of_File (Input) loop
         Current_Code_Line := Code_Line.Empty_Vector;
         Current_Line := To_Unbounded_String(Get_Line(Input));

         Character_Loop:
         for I in 1 .. Length(Current_Line) loop
            Current_Code_Line.Append(Element(Current_Line, I));
         end loop Character_Loop;

         Navigation_Subsystem.Append(Current_Code_Line);
      end loop;
      Close (File => Input);
   end Load_File;

   procedure Print_Line (Line_Of_Code : in Code_Line.Vector) is
   begin
      Print_Loop:
      for C of Line_Of_Code loop
         Put(C);
      end loop Print_Loop;
      Put_Line("");
   end Print_Line;

   function Reduce_Line(Line_Of_Code : in Code_Line.Vector) return Code_Line.Vector is
      Result  : Code_Line.Vector := Line_Of_Code;
      Bracket : Character;
      Reduced : Boolean := False;
   begin
      Element_Loop:
      for I in Line_Of_Code.First_Index .. Line_Of_Code.Last_Index loop
         Bracket := Line_Of_Code(I);
         case Bracket is
            when ')' =>
               if Line_Of_Code(I-1) = '(' then
                  Result.Delete(I);
                  Result.Delete(I-1);
                  Reduced := True;
                  exit Element_Loop;
               end if;
            when ']' =>
               if Line_Of_Code(I-1) = '[' then
                  Result.Delete(I);
                  Result.Delete(I-1);
                  Reduced := True;
                  exit Element_Loop;
               end if;
            when '}' =>
               if Line_Of_Code(I-1) = '{' then
                  Result.Delete(I);
                  Result.Delete(I-1);
                  Reduced := True;
                  exit Element_Loop;
               end if;
            when '>' =>
               if Line_Of_Code(I-1) = '<' then
                  Result.Delete(I);
                  Result.Delete(I-1);
                  Reduced := True;
                  exit Element_Loop;
               end if;
            when others => null;
         end case;
      end loop Element_Loop;

      if Reduced then
         Result := Reduce_Line(Result);
      end if;
      return Result;
   end Reduce_Line;

   -- Returns whether the line of code is corrupted. If it is not corrupted,
   -- it is incomplete and Reduced_Line_Of_Code will be the reduced line.
   function Is_Corrupted(Line_Of_Code : in Code_Line.Vector;
                         Reduced_Line : out Code_Line.Vector) return Boolean is
      Corrupted : Boolean := False;
   begin
      Reduced_Line := Reduce_Line(Line_Of_Code);

      Find_Illegal_Char_Loop:
      for C of Reduced_Line loop
         case C is
            when ')' | ']' | '}' | '>' =>
               Corrupted := True;
               exit Find_Illegal_Char_Loop;
            when others => null;
         end case;
      end loop Find_Illegal_Char_Loop;

      return Corrupted;
   end Is_Corrupted;

   function Finish (Incomplete_Line : in Code_Line.Vector) return Long_Integer is
      Complete_Line : Code_Line.Vector := Incomplete_Line;
      Score : Long_Integer := 0;
   begin
      Complete_Loop:
      for C of reverse Incomplete_Line loop
         case C is
            when '(' => Score := Score * 5 + 1;
            when '[' => Score := Score * 5 + 2;
            when '{' => Score := Score * 5 + 3;
            when '<' => Score := Score * 5 + 4;
            when others => null;
         end case;
      end loop Complete_Loop;
      return Score;
   end Finish;

   Navigation_Subsystem : Program.Vector;
   Reduced_Line         : Code_Line.Vector;
   Current_Score        : Long_Integer;
   Score_List           : Scores.Vector;

   Number_Of_Scores : Integer;
   Answer           : Long_Integer;
begin
   Load_File(Navigation_Subsystem);

   Line_Loop:
   for Line_Of_Code of Navigation_Subsystem loop
      if not Is_Corrupted(Line_Of_Code, Reduced_Line) then
         Current_Score := Finish(Reduced_Line);
         Score_List.Append(Current_Score);
      end if;
   end loop Line_Loop;

   Sort(Score_List);

   Number_Of_Scores := Integer(Score_List.Length);
   Answer := Score_List(Number_Of_Scores / 2);

   Put_line("The middle score of the incomplete lines "
            & Long_Integer'Image(Answer) & ".");
end Day10_2;
