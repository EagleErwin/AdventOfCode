with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day02_1 is
   -- ### TYPE DEFINITIONS ### --
   type Direction is (Forward, Down, Up);
   type Instruction is record
      Dir      : Direction;
      Distance : Integer;
   end record;


   function To_Instruction(Input: in Unbounded_String) return Instruction is
      Separator_Index : Integer;
      Result : Instruction;
   begin
      Separator_Index := Index(Input, " ");

      Result.Dir      := Direction'Value(Slice(Input, 1, Separator_Index - 1));
      Result.Distance := Integer'Value(Slice(Input,
                                             Separator_Index + 1,
                                             Length(Input)));
      return Result;
   end To_Instruction;

   Input          : File_Type;
   Current_Line   : Unbounded_String;
   Current_Instr  : Instruction;
   Current_Dist   : Integer;
   Horizontal_Pos : Integer := 0;
   Depth          : Integer := 0;
   Answer         : Integer := 0;
begin
   Open (File => Input, Mode => In_File, Name => "data/day02.input");
   while not End_Of_File (Input) loop
      Current_Line := To_Unbounded_String(Get_Line(Input));
      Current_Instr := To_Instruction(Current_Line);
      Current_Dist := Current_Instr.Distance;

      case Current_Instr.Dir is
         when Forward => Horizontal_Pos := Horizontal_Pos + Current_Dist;
         when Down    => Depth := Depth + Current_Dist;
         when Up      => Depth := Depth - Current_Dist;
      end case;

   end loop;
   Close (File => Input);

   Answer := Horizontal_Pos * Depth;

   Put_line("The answer is " & Integer'Image(Answer)
            & ", calculated by multiplying horizontal position "
            & Horizontal_Pos'Image & " with depth " & Depth'Image & ".");
end Day02_1;
