with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day08_2 is
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

   function Run (Memory : in out Execution) return Boolean is
      Curr_Instr : Instruction := Memory.App(Memory.IP);
   begin
      Loop_Detection_Loop:
      while not Curr_Instr.Executed loop
         Memory.App(Memory.IP).Executed := True;
         Log_Debug("Executing " & Operation'Image(Curr_Instr.Op)
                   & " at IP " & Integer'Image(Memory.IP));
         case Curr_Instr.Op is
            when Acc =>
               Memory.Accumulator := Memory.Accumulator + Curr_Instr.Val;
               Memory.IP := Memory.IP + 1;
            when Jmp =>
               Memory.IP := Memory.IP + Curr_Instr.Val;
            when Nop =>
               Memory.IP := Memory.IP + 1;
         end case;
         if Memory.IP >= Memory.App'Length then
            return True;
         end if;
         Curr_Instr := Memory.App(Memory.IP);
      end loop Loop_Detection_Loop;
      return False;
   end Run;

   function Patch_Program (Bugged : in Program;
                           Op_To_Change : in Operation;
                           Occurrence_To_Change : in Integer) return Program is
      Result : Program := Bugged;
      Occurrence : Integer := 0;
      Curr_Instr : Instruction;
   begin
      Patch_Loop:
      for I in Result'Range loop
         Curr_Instr := Result(I);
         if Curr_Instr.Op = Op_To_Change then
            Occurrence := Occurrence + 1;
            if Occurrence = Occurrence_To_Change then
               case Op_To_Change is
                  when Jmp => Result(I).Op := Nop;
                     Log_Debug("Patched Jmp #" & Integer'Image(Occurrence) & " to Nop");
                  when Nop =>
                     Result(I).Op := Jmp;
                     Log_Debug("Patched Nop #" & Integer'Image(Occurrence) & " to Jmp");
                  when Acc => Result(I).Op := Acc;
               end case;
               exit Patch_Loop;
            end if;
         end if;
      end loop Patch_Loop;

      return Result;
   end Patch_Program;

   Handheld_Game : Program;
   Game_Execution : Execution;

   Patched_Game : Program;
   Finished : Boolean;
   Op_To_Change : Operation := Jmp;

   Answer : Integer := 0;
begin
   Load_File(Handheld_Game);

   Jmp_Loop:
   for J in 1 .. Number_Of_Inputs loop
      Patched_Game := Patch_Program(Handheld_Game, Op_To_Change, J);
      Game_Execution := Execution'(App         => Patched_Game,
                                   IP          => 1,
                                   Accumulator => 0);
      Finished := Run(Game_Execution);
      if Finished then
         Put_Line("The program finished after changing the "
                  & Integer'Image(J) & "th occurrence of Jmp to Nop.");
         exit Jmp_Loop;
      end if;
   end loop Jmp_Loop;

   if not Finished then
      Op_To_Change := Nop;
      Nop_Loop:
      for N in 1 .. Number_Of_Inputs loop
         Patched_Game := Patch_Program(Handheld_Game, Op_To_Change, N);
         Game_Execution := Execution'(App         => Patched_Game,
                                      IP          => 1,
                                      Accumulator => 0);
         Finished := Run(Game_Execution);
         if Finished then
            Put_Line("The program finished after changing the "
                     & Integer'Image(N) & "th occurrence of Nop to Jmp.");
            exit Nop_Loop;
         end if;
      end loop Nop_Loop;
   end if;

   Answer := Game_Execution.Accumulator;

   if Finished then
      Put_Line("The program was finished successfully");
   else
      Put_Line("The program was forced to stop due to a loop");
   end if;

   Put_line("The value of the accumulator is " & Integer'Image(Answer) & ".");
end Day08_2;
