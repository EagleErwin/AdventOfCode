with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day02_1 is
   -- ### CONSTANTS ### --
   Memory_Size : constant Integer := 999; -- Index of last memory location (sufficient capacity)

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

   procedure Init (Intcode : in out Application) is
   begin
      Intcode(1) := 12;
      Intcode(2) := 2;
   end Init;

   -- Add the values at position 1 and 2 and store the result at position Target_Pos
   procedure Add (Position_1 : in Integer;
                  Position_2 : in Integer;
                  Target_Pos : in Integer;
                  Intcode    : in out Application) is
   begin
      Intcode(Target_Pos) := Intcode(Position_1) + Intcode(Position_2);
   end Add;


   -- Multiplies the values at position 1 and 2 and store the result at position Target_Pos
   procedure Mul (Position_1 : in Integer;
                  Position_2 : in Integer;
                  Target_Pos : in Integer;
                  Intcode    : in out Application) is
   begin
      Intcode(Target_Pos) := Intcode(Position_1) * Intcode(Position_2);
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
                          Intcode => Intcode);
            when 2 => Mul(Position_1 => Intcode(Instruction_Pointer + 1),
                          Position_2 => Intcode(Instruction_Pointer + 2),
                          Target_Pos => Intcode(Instruction_Pointer + 3),
                          Intcode => Intcode);
            when 99 => exit Program_Loop;
            when others => Put_Line("Invalid instruction found at location " & Integer'Image(Instruction_Pointer) & ": " & Integer'Image(Current_Instruction));
         end case;
         Instruction_Pointer := Instruction_Pointer + 4;
      end loop Program_Loop;
   end Run;

   Intcode_App    : Application;
begin
   Load_File (Intcode => Intcode_App);
   Init (Intcode => Intcode_App);
   Run (Intcode => Intcode_App);
   Put_Line (Integer'Image(Intcode_App(0)));
end Day02_1;
