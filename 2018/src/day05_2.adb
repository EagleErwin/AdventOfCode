with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
procedure Day05_2 is
   Initial_String_Length : constant Integer := 50000;
   ASCII_Distance : constant Integer := 32;
   Letters_In_Alphabet : constant Integer := 26;
   
   type Administration is array(Character range 'a'..'z') of Integer;
         
   procedure Load_File (Data : out Unbounded_String) is
      Input : File_Type;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day05.input");
      Data  := To_Unbounded_String (Get_Line (Input));
      Close (File => Input);
   end Load_File;
   
   function Is_Match (A, B : in Character) return Boolean is
   begin
      if Character'Pos(A) + ASCII_Distance = Character'Pos(B) or
        Character'Pos(A) - ASCII_Distance = Character'Pos(B) then
         return True;
      end if;
      return False;
   end Is_Match;
   
   function Clash (Polymer : in Unbounded_String) return Integer is
      Match_Index : Integer := 1;
      Pair_A : Character;
      Pair_B : Character;
      Result : Unbounded_String;
   begin
      Result := Polymer;
      while Match_Index < Length(Result) loop
         Pair_A := Element(Result, Match_Index);
         Pair_B := Element(Result, Match_Index + 1);
         if Is_Match(A => Pair_A, B => Pair_B) then
            Result := Delete(Source => Result,
                              From => Match_Index, Through => Match_Index+1);
            if Match_Index > 1 then
               Match_Index := Match_Index - 1;
            end if;
         else
            Match_Index := Match_Index + 1;
         end if;      
      end loop;
      return Length(Result);
   end Clash;
   
   function Strip (Polymer : in Unbounded_String;
                   Unit : in Character) return Unbounded_String is
      Lower_Case_Char : Character;
      Upper_Case_Char : Character;
      Current_Char : Character;
      Intermediate : Unbounded_String;
   begin
      Lower_Case_Char := Unit;
      Upper_Case_Char := Character'Val(Character'Pos(Unit) - ASCII_Distance);
      for I in 1 .. Length(Polymer) loop
         Current_Char := Element(Polymer, I);
         if Current_Char /= Lower_Case_Char
           and then Current_Char /= Upper_Case_Char then
            Intermediate := Intermediate & Current_Char;
         end if;
      end loop;
      return Intermediate;
   end Strip;
   
   Alphabet : Administration;
   Input_String : Unbounded_String;
   
   Clashed_Length : Integer;
   Shortest : Integer := Initial_String_Length;
begin
   Load_File(Data => Input_String);
   for C in Character range 'a'..'z' loop
      Clashed_Length := Clash(Polymer => Strip(Polymer => Input_String, Unit => C));
      Alphabet(C) := Clashed_Length;
   end loop;
   
   for I in Alphabet'Range loop
      if Alphabet(I) < Shortest then
         Shortest := Alphabet(I);
      end if;
   end loop;
   
   Put_line(Integer'Image(Shortest));
end Day05_2;
