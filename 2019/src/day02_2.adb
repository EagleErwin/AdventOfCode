with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day02_2 is
   -- ### CONSTANTS ### --
   Memory_Size : constant Integer := 999; -- Index of last memory location (sufficient capacity)
   Required_Value : constant Integer := 19_690_720;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Application is array (0 .. Memory_Size) of Integer;

   procedure Load_File (Intcode : out Application) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 0;
      Curr_Char    : Character;
      Buffer       : Unbounded_String;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day02.input");
      Current_Line := To_Unbounded_String(Get_Line(Input));
      Close (File => Input);

      for C in 1 .. Length(Current_Line) loop
         Curr_Char := Element(Current_Line, C);
         if Curr_Char = ',' then
            Intcode(Idx) := Integer'Value(To_String(Buffer));
            Buffer := To_Unbounded_String("");
            Idx := Idx + 1;
         else
            Buffer := Buffer & Curr_Char;
         end if;
      end loop;
      Intcode(Idx) := Integer'Value(To_String(Buffer));
   end Load_File;

   function Reset (Intcode : in Application) return Application is
      Result : Application;
   begin
      Result := Intcode;
      return Result;
   end Reset;

   procedure Init (Intcode : in out Application;
                   Noun : in Integer;
                   Verb : in Integer) is
   begin
      Intcode(1) := Noun;
      Intcode(2) := Verb;
   end Init;

   -- Add the values at position 1 and 2 and store the result at position Target_Pos
   procedure Add (Position_1 : in Integer;
                  Position_2 : in Integer;
                  Target_Pos : in Integer;
                  Intcode    : in out Application;
                  IP         : in out Integer) is
   begin
      Intcode(Target_Pos) := Intcode(Position_1) + Intcode(Position_2);
      IP := IP + 4;
   end Add;


   -- Multiplies the values at position 1 and 2 and store the result at position Target_Pos
   procedure Mul (Position_1 : in Integer;
                  Position_2 : in Integer;
                  Target_Pos : in Integer;
                  Intcode    : in out Application;
                  IP         : in out Integer) is
   begin
      Intcode(Target_Pos) := Intcode(Position_1) * Intcode(Position_2);
      IP := IP + 4;
   end Mul;

   procedure Run (Intcode : in out Application) is
      Instruction_Pointer : Integer := 0;
      Current_Instruction : Integer;
   begin
      Program_Loop :
      loop
         Current_Instruction := Intcode(Instruction_Pointer);
         case Current_Instruction is
            when 1 => Add(Position_1 => Intcode(Instruction_Pointer + 1),
                          Position_2 => Intcode(Instruction_Pointer + 2),
                          Target_Pos => Intcode(Instruction_Pointer + 3),
                          Intcode => Intcode,
                          IP => Instruction_Pointer);
            when 2 => Mul(Position_1 => Intcode(Instruction_Pointer + 1),
                          Position_2 => Intcode(Instruction_Pointer + 2),
                          Target_Pos => Intcode(Instruction_Pointer + 3),
                          Intcode => Intcode,
                          IP => Instruction_Pointer);
            when 99 => exit Program_Loop;
            when others => Put_Line("Invalid instruction found at location " & Integer'Image(Instruction_Pointer) & ": " & Integer'Image(Current_Instruction));
         end case;
      end loop Program_Loop;
   end Run;

   function Verify (Intcode : in Application) return Boolean is
   begin
      return Intcode(0) = Required_Value;
   end Verify;

   function Calculate_Answer (Noun : in Integer;
                              Verb : in Integer) return Integer is
   begin
      return 100 * Noun + Verb;
   end Calculate_Answer;

   Intcode_App : Application;
   Intcode_Try : Application;
   Answer      : Integer;
begin
   Load_File (Intcode => Intcode_App);
   First_Value :
   for A in 0 .. 99 loop
      Second_Value :
      for B in 0 .. 99 loop
         Intcode_Try := Reset (Intcode => Intcode_App);
         Init (Intcode => Intcode_Try,
               Noun => A,
               Verb => B);
         Run (Intcode => Intcode_Try);
         if Verify (Intcode => Intcode_Try) then
               Answer := Calculate_Answer(Noun => A, Verb => B);
            exit First_Value;
         end if;
      end loop Second_Value;
   end loop First_Value;

   Put_Line (Integer'Image(Answer));
end Day02_2;
