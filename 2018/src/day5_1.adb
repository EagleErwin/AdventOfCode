with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
procedure Day5_1 is
   Initial_String_Length : constant Integer := 50000;
   ASCII_Distance : constant Integer := 32;
         
   procedure Load_File (Data : out Unbounded_String) is
      Input : File_Type;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day5.input");
      Data  := To_Unbounded_String (Get_Line (Input));
      Close (File => Input);
   end Load_File;
   
   function Is_Match (A, B : in Character) return Boolean is
   begin
--      Put_Line("Checking characters " & A & " and " & B);
      if Character'Pos(A) + ASCII_Distance = Character'Pos(B) or
        Character'Pos(A) - ASCII_Distance = Character'Pos(B) then
         return True;
      end if;
      return False;
   end Is_Match;
   
   procedure Clash (Polymer : in out Unbounded_String) is
      Match_Index : Integer := 1;
      Pair_A : Character;
      Pair_B : Character;
   begin
      while Match_Index < Length(Polymer) loop
         Pair_A := Element(Polymer, Match_Index);
         Pair_B := Element(Polymer, Match_Index + 1);
         if Is_Match(A => Pair_A, B => Pair_B) then
            Polymer := Delete(Source => Polymer,
                              From => Match_Index, Through => Match_Index+1);
            if Match_Index > 1 then
               Match_Index := Match_Index - 1;
            end if;
         else
            Match_Index := Match_Index + 1;
         end if;      
      end loop;
      
   end Clash;
   
   
   Input_String : Unbounded_String;
begin
   Load_File(Data => Input_String);
   Clash(Input_String);
   Put_Line(Integer'Image(Length(Input_String)));

end Day5_1;
