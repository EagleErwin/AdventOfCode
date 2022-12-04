with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day04_1 is
   Input_Size : constant Integer := 1_000;

   type Section_Range is record
      Start_Value : Integer;
      Stop_Value : Integer;
   end record;

   type Pair is record
      Elf_1 : Section_Range;
      Elf_2 : Section_Range;
   end record;

   type Assignments is array (1 .. Input_Size) of Pair;

   function Get_Pair (Input : in Unbounded_String) return Pair is
      Result  : Pair;
      Range_1 : Section_Range := (others => 0);
      Range_2 : Section_Range := (others => 0);

      Parse_State  : Integer := 1; --Indicates if we are filling Range_1 or Range_2
      Current_Char : Character;
      Buffer       : Unbounded_String := To_Unbounded_String("");
   begin
      Parse_Loop:
      for I in 1 .. Length(Input) loop
         Current_Char := Element(Input, I);
         if (Current_Char = '-') then
            if (Parse_State = 1) then
               Range_1.Start_Value := Integer'Value(To_String(Buffer));
            else
               Range_2.Start_Value := Integer'Value(To_String(Buffer));
            end if;
            Buffer := To_Unbounded_String("");
         elsif (Current_Char = ',') then
            Range_1.Stop_Value := Integer'Value(To_String(Buffer));
            Buffer := To_Unbounded_String("");
            Parse_State := 2;
         else
            -- Number
            Buffer := Buffer & Current_Char;
         end if;
      end loop Parse_Loop;

      Range_2.Stop_Value := Integer'Value(To_String(Buffer));

      Result.Elf_1 := Range_1;
      Result.Elf_2 := Range_2;

      return Result;
   end Get_Pair;

   procedure Load_File (Work : out Assignments) is
      Input            : File_Type;
      Current_Line     : Unbounded_String;
      Current_Pair     : Pair;
      Assignment_Index : Integer := 0;

   begin
      Open (File => Input, Mode => In_File, Name => "data/day04.input");
      while not End_Of_File (Input) loop
         Assignment_Index := Assignment_Index + 1;
         Current_Line := To_Unbounded_String(Get_Line(Input));

         Current_Pair := Get_Pair(Current_Line);
         Work(Assignment_Index) := Current_Pair;
      end loop;
      Close (File => Input);
   end Load_File;

   function Overlap_Check (Subject : in Pair) return Boolean is
      E_1    : Section_Range := Subject.Elf_1;
      E_2    : Section_Range := Subject.Elf_2;
      Result : Boolean := False;
   begin
      return ((E_1.Start_Value <= E_2.Start_Value and E_1.Stop_Value >= E_2.Stop_Value) or else
            (E_2.Start_Value <= E_1.Start_Value and E_2.Stop_Value >= E_1.Stop_Value));
   end Overlap_Check;

   Work   : Assignments;
   Answer : Integer := 0;
begin
   Load_File(Work);

   Overlap_Check_Loop:
   for I in Work'Range loop
      if (Overlap_Check(Work(I))) then
         Answer := Answer + 1;
      end if;
   end loop Overlap_Check_Loop;

   Put_Line("The number of assignment pairs in which one range fully contains "
              & "the other, is " & Integer'Image(Answer));
end Day04_1;
