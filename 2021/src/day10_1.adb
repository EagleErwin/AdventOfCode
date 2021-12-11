with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day10_1 is
   -- ### CONSTANTS ### --

   -- ### TYPE DEFINITIONS ### --
   package Code_Line is new Vectors(Index_Type   => Natural,
                                    Element_Type => Character);
   use Code_Line;

   package Program is new Vectors(Index_Type   => Natural,
                                  Element_Type => Code_Line.Vector);
   use Program;

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

   -- Returns the syntax error score of an invalid line of code. Only corrupted
   -- lines are considered, others will return a score of 0.
   function Get_Score(Line_Of_Code : in Code_Line.Vector) return Integer is
      Reduced_Line : Code_Line.Vector;
   begin
      Reduced_Line := Reduce_Line(Line_Of_Code);

      Find_Illegal_Char_Loop:
      for C of Reduced_Line loop
         case C is
            when ')' => return 3;
            when ']' => return 57;
            when '}' => return 1197;
            when '>' => return 25137;
            when others => null;
         end case;
      end loop Find_Illegal_Char_Loop;
      return 0;
   end Get_Score;

   Navigation_Subsystem : Program.Vector;
   Answer  : Integer := 0;
begin
   Load_File(Navigation_Subsystem);

   Line_Loop:
   for Line_Of_Code of Navigation_Subsystem loop
      Answer := Answer + Get_Score(Line_Of_Code);
   end loop Line_Loop;

   Put_line("The total syntax error score for the corrupted lines is "
            & Integer'Image(Answer) & ".");
end Day10_1;
