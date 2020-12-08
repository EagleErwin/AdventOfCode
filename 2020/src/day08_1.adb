with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day08_1 is
   -- ### CONSTANTS ### --
   Enable_Debug : constant Boolean := False;
   Number_Of_Inputs : constant Integer := 649;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Operation is (Acc, Jmp, Nop);

   type Instruction is record
      Op : Operation;
      Val : Integer;
      Executed : Boolean;
   end record;

   type Program is array (1 .. Number_Of_Inputs) of Instruction;

   type Execution is record
      App : Program;
      IP : Integer;
      Accumulator : Integer;
   end record;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   procedure Load_File (Application : out Program) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 1;

      Curr_Op : Operation;
      Curr_Val : Integer;
      Curr_Instr : Instruction;
   begin
      Application := (others => Instruction'(Op       => Nop,
                                             Val      => 0,
                                             Executed => False));

      Open (File => Input, Mode => In_File, Name => "data/day08.input");
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Curr_Op := Operation'Value(Slice(Current_Line, 1, 3));
         Curr_Val := Integer'Value(Slice(Current_Line, 5, Length(Current_Line)));
         Curr_Instr := Instruction'(Op       => Curr_Op,
                                    Val      => Curr_Val,
                                    Executed => False);
         Application(Idx) := Curr_Instr;
         Idx := Idx + 1;
      end loop;
      Close (File => Input);
   end Load_File;

   procedure Run (Memory : in out Execution) is
      Curr_Instr : Instruction := Memory.App(Memory.IP);
   begin
      Loop_Detection_Loop:
      while not Curr_Instr.Executed loop
         Memory.App(Memory.IP).Executed := True;
         case Curr_Instr.Op is
            when Acc =>
               Memory.Accumulator := Memory.Accumulator + Curr_Instr.Val;
               Memory.IP := Memory.IP + 1;
            when Jmp =>
               Memory.IP := Memory.IP + Curr_Instr.Val;
            when Nop =>
               Memory.IP := Memory.IP + 1;
         end case;
         Curr_Instr := Memory.App(Memory.IP);
      end loop Loop_Detection_Loop;
   end Run;

   Handheld_Game : Program;
   Game_Execution : Execution;
   Answer : Integer := 0;
begin
   Load_File(Handheld_Game);

   Game_Execution := Execution'(App         => Handheld_Game,
                                IP          => 1,
                                Accumulator => 0);
   Run(Game_Execution);

   Answer := Game_Execution.Accumulator;

   Put_line("The value of the accumulator is " & Integer'Image(Answer)
            & " when the loop was detected.");
end Day08_1;
