with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;

procedure Day07_2 is
   -- ### CONSTANTS ### --
   DEBUG : constant Boolean := False; -- Set this to True to enable debug logging
   Memory_Size : constant Long_Integer := 100_000; -- Index of last memory location (sufficient capacity)

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Application is array (0 .. Memory_Size) of Long_Integer;

   type Operation is (Add, Mul, Input, Output, JumpTrue, JumpFalse, LessThan, Equals, Finished);
   type Parameter_Mode is (Position, Immediate);

   type Instruction is record
      Op     : Operation;
      Mode_1 : Parameter_Mode;
      Mode_2 : Parameter_Mode;
      Mode_3 : Parameter_Mode;
   end record;

   type Phase_Setting_Sequence is array (0..4) of Integer range 5 .. 9;
   type Permutations_Db is array (0 .. 119) of Phase_Setting_Sequence;

   type Long_Access is access all Long_Integer;
   type Execution is record
      Memory             : Application;
      IP                 : Long_Integer;
      Input_Value        : Long_Access;
      Phase_Setting      : Long_Integer;
      Phase_Setting_Read : Boolean;
      Last_Output        : Long_Access;
      Finished           : Boolean;
   end record;

   type Execution_Access is access all Execution;
   type System is array (0 .. 4) of Execution_Access;

   -- ### VARIABLES ### --
   Permutations_Ptr : Integer := 0; -- Indicates which permutation should be written

   procedure Log_Debug (Log_Line : in String) is
   begin
      if (DEBUG) then
         Put_Line(Log_Line);
      end if;
   end Log_Debug;

   procedure Generate_Amplifier_System (Amplifiers : out System;
                                        Initial_Memory : in Application) is
   begin
      Amplifiers := (others => new Execution'(Memory             => Initial_Memory,
                                              IP                 => 0,
                                              Input_Value        => null,
                                              Phase_Setting      => 0,
                                              Phase_Setting_Read => False,
                                              Last_Output        => null,
                                              Finished           => False));
   end Generate_Amplifier_System;

   -- Thank you https://www.topcoder.com/generating-permutations/
   procedure Generate_Permutations(Output   : in out Permutations_Db;
                                   Sequence : in out Phase_Setting_Sequence;
                                   Elements : in Integer) is
      Tmp_Value : Integer;
   begin
      if Elements = 1 then
         Output(Permutations_Ptr) := Sequence;
         Permutations_Ptr := Permutations_Ptr + 1;
      else
         for I in 0 .. (Elements - 1) loop
            Tmp_Value := Sequence(I);
            Sequence(I) := Sequence(Elements - 1);
            Sequence(Elements - 1) := Tmp_Value;

            Generate_Permutations(Output, Sequence, (Elements - 1));

            Tmp_Value := Sequence(I);
            Sequence(I) := Sequence(Elements - 1);
            Sequence(Elements - 1) := Tmp_Value;
         end loop;
      end if;
   end Generate_Permutations;

   procedure Load_File (Intcode : out Application) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 0;
      Curr_Char    : Character;
      Buffer       : Unbounded_String;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day07.input");
      Current_Line := To_Unbounded_String(Get_Line(Input));
      Close (File => Input);

      for C in 1 .. Length(Current_Line) loop
         Curr_Char := Element(Current_Line, C);
         if Curr_Char = ',' then
            Intcode(Long_Integer(Idx)) := Long_Integer'Value(To_String(Buffer));
            Buffer := To_Unbounded_String("");
            Idx := Idx + 1;
         else
            Buffer := Buffer & Curr_Char;
         end if;
      end loop;
      Intcode(Long_Integer(Idx)) := Long_Integer'Value(To_String(Buffer));
   end Load_File;

   procedure Init_Phase_Settings (Amplifiers : in out System;
                                  Sequence   : in Phase_Setting_Sequence) is
   begin
      for I in Amplifiers'Range loop
         Amplifiers(I).Phase_Setting := Long_Integer(Sequence(I));
      end loop;
   end Init_Phase_Settings;

   function Decode_Operation (Operation_String : in String) return Operation is
   begin
      if Operation_String = "01" then
         return Add;
      elsif Operation_String = "02" then
         return Mul;
      elsif Operation_String = "03" then
         return Input;
      elsif Operation_String = "04" then
         return Output;
      elsif Operation_String = "05" then
         return JumpTrue;
      elsif Operation_String = "06" then
         return JumpFalse;
      elsif Operation_String = "07" then
         return LessThan;
      elsif Operation_String = "08" then
         return Equals;
      elsif Operation_String = "99" then
         return Finished;
      end if;
      Put_Line("Illegal operation: " & Operation_String);
      return Finished;
   end Decode_Operation;

   function Decode_Mode (Mode_String : in Character) return Parameter_Mode is
   begin
      if Mode_String = '0' then
         return Position;
      elsif Mode_String = '1' then
         return Immediate;
      end if;
      Put_Line("Illegal mode: " & Mode_String);
      return Position;
   end Decode_Mode;

   function Decode_Instruction (InstructionInt : in Long_Integer)
                                return Instruction is
      InstructionString    : String(1..5) := (others => '0');
      InstructionIntString : String := Trim(Source => Long_Integer'Image(InstructionInt),
                                            Side   => Ada.Strings.Left);
      The_Operation        : Operation;
      Result               : Instruction;
   begin
      Log_Debug("Decoding " & Long_Integer'Image(InstructionInt));
      InstructionString := Overwrite(Source   => InstructionString,
                                     Position => (1 + InstructionString'Length - InstructionIntString'Length),
                                     New_Item => InstructionIntString);
      Log_Debug(InstructionString);
      The_Operation := Decode_Operation(InstructionString(4..5));

      Log_Debug(InstructionString & " -> " & Operation'Image(The_Operation));
      Result := Instruction'(Op => The_Operation,
                             Mode_1 => Decode_Mode(InstructionString(3)),
                             Mode_2 => Decode_Mode(InstructionString(2)),
                             Mode_3 => Decode_Mode(InstructionString(1)));
      return Result;
   end Decode_Instruction;

   -- Add instruction
   function Add (Add_Instruction : in Instruction;
                 Exec            : in out Execution) return Boolean is
      First_Value  : Long_Integer;
      Second_Value : Long_Integer;
      Target_Pos   : Long_Integer;
   begin
      case Add_Instruction.Mode_1 is
         when Position  => First_Value := Exec.Memory(Exec.Memory(Exec.IP + 1));
         when Immediate => First_Value := Exec.Memory(Exec.IP + 1);
      end case;

      case Add_Instruction.Mode_2 is
         when Position  => Second_Value := Exec.Memory(Exec.Memory(Exec.IP + 2));
         when Immediate => Second_Value := Exec.Memory(Exec.IP + 2);
      end case;

      case Add_Instruction.Mode_3 is
         when Position  => Target_Pos := Exec.Memory(Exec.IP + 3);
         when Immediate => Target_Pos := Exec.IP + 3;
      end case;
      Exec.Memory(Target_Pos) := First_Value + Second_Value;
      Log_Debug("Adding " & Long_Integer'Image(First_Value)
                & " + " & Long_Integer'Image(Second_Value)
                & " to " & Long_Integer'Image(Target_Pos));
      Exec.IP := Exec.IP + 4;

      return False;
   end Add;

   -- Multiply instruction
   function Mul (Mul_Instruction : in Instruction;
                  Exec           : in out Execution) return Boolean is
      First_Value  : Long_Integer;
      Second_Value : Long_Integer;
      Target_Pos   : Long_Integer := Exec.Memory(Exec.IP + 3);
   begin
      case Mul_Instruction.Mode_1 is
         when Position  => First_Value := Exec.Memory(Exec.Memory(Exec.IP + 1));
         when Immediate => First_Value := Exec.Memory(Exec.IP + 1);
      end case;

      case Mul_Instruction.Mode_2 is
         when Position  => Second_Value := Exec.Memory(Exec.Memory(Exec.IP + 2));
         when Immediate => Second_Value := Exec.Memory(Exec.IP + 2);
      end case;

      case Mul_Instruction.Mode_3 is
         when Position  => Target_Pos := Exec.Memory(Exec.IP + 3);
         when Immediate => Target_Pos := Exec.IP + 3;
      end case;
      Log_Debug("Adding " & Long_Integer'Image(First_Value)
                & " * " & Long_Integer'Image(Second_Value)
                & " to " & Long_Integer'Image(Target_Pos));
      Exec.Memory(Target_Pos) := First_Value * Second_Value;
      Exec.IP := Exec.IP + 4;

      return False;
   end Mul;

   function Input (Input_Instruction : in Instruction;
                   Exec              : in out Execution) return Boolean is
      Target_Pos : Long_Integer;
   begin
      case Input_Instruction.Mode_1 is
         when Position  => Target_Pos := Exec.Memory(Exec.IP + 1);
         when Immediate => Target_Pos := Exec.IP + 1;
      end case;

      if Exec.Phase_Setting_Read then
         if (Exec.Input_Value = null) then
            Log_Debug("Interrupted because no Input is available");
            return True;
         else
            Exec.Memory(Target_Pos) := Exec.Input_Value.all;
            Log_Debug("Adding " & Long_Integer'Image(Exec.Input_Value.all)
                      & " to " & Long_Integer'Image(Target_Pos));
            Exec.Input_Value := null;
         end if;
      else
         Exec.Memory(Target_Pos) := Exec.Phase_Setting;
         Exec.Phase_Setting_Read := True;
         Log_Debug("Reading Phase Setting "
                   & Long_Integer'Image(Exec.Phase_Setting)
                   & " to " & Long_Integer'Image(Target_Pos));
      end if;


      Exec.IP := Exec.IP + 2;

      return False;
   end Input;

   function Output (Output_Instruction : in Instruction;
                    Exec               : in out Execution) return Boolean is
      Output_Value : Long_Integer;
   begin
      case Output_Instruction.Mode_1 is
         when Position  => Output_Value := Exec.Memory(Exec.Memory(Exec.IP + 1));
         when Immediate => Output_Value := Exec.Memory(Exec.IP + 1);
      end case;
      Exec.Last_Output := new Long_Integer'(Output_Value);
      Log_Debug("Output: " & Long_Integer'Image(Exec.Last_Output.all));
      Exec.IP := Exec.IP + 2;

      return False;
   end Output;

   -- JumpTrue instruction
   function JumpTrue (JT_Instruction : in Instruction;
                      Exec           : in out Execution) return Boolean is
      First_Value  : Long_Integer;
      Second_Value : Long_Integer;
   begin
      case JT_Instruction.Mode_1 is
         when Position  => First_Value := Exec.Memory(Exec.Memory(Exec.IP + 1));
         when Immediate => First_Value := Exec.Memory(Exec.IP + 1);
      end case;

      case JT_Instruction.Mode_2 is
         when Position  => Second_Value := Exec.Memory(Exec.Memory(Exec.IP + 2));
         when Immediate => Second_Value := Exec.Memory(Exec.IP + 2);
      end case;

      if First_Value /= 0 then
         Exec.IP := Second_Value;
         Log_Debug("JumpingTrue to " & Long_Integer'Image(Second_Value)
                   & " based on " & Long_Integer'Image(First_Value));
      else
         Exec.IP := Exec.IP + 3;
         Log_Debug("No JumpTrue based on " & Long_Integer'Image(First_Value));
      end if;

      return False;
   end JumpTrue;

   -- JumpFalse instruction
   function JumpFalse (JF_Instruction : in Instruction;
                       Exec           : in out Execution) return Boolean is
      First_Value  : Long_Integer;
      Second_Value : Long_Integer;
   begin
      case JF_Instruction.Mode_1 is
         when Position  => First_Value := Exec.Memory(Exec.Memory(Exec.IP + 1));
         when Immediate => First_Value := Exec.Memory(Exec.IP + 1);
      end case;

      case JF_Instruction.Mode_2 is
         when Position  => Second_Value := Exec.Memory(Exec.Memory(Exec.IP + 2));
         when Immediate => Second_Value := Exec.Memory(Exec.IP + 2);
      end case;

      if First_Value = 0 then
         Exec.IP := Second_Value;
         Log_Debug("JumpingFalse to " & Long_Integer'Image(Second_Value)
                   & " based on " & Long_Integer'Image(First_Value));
      else
         Exec.IP := Exec.IP + 3;
         Log_Debug("No JumpFalse based on " & Long_Integer'Image(First_Value));
      end if;

      return False;
   end JumpFalse;

   -- LessThan instruction
   function LessThan (LT_Instruction : in Instruction;
                      Exec           : in out Execution) return Boolean is
      First_Value  : Long_Integer;
      Second_Value : Long_Integer;
      Target_Pos   : Long_Integer;
   begin
      case LT_Instruction.Mode_1 is
         when Position  => First_Value := Exec.Memory(Exec.Memory(Exec.IP + 1));
         when Immediate => First_Value := Exec.Memory(Exec.IP + 1);
      end case;

      case LT_Instruction.Mode_2 is
         when Position => Second_Value := Exec.Memory(Exec.Memory(Exec.IP + 2));
         when Immediate => Second_Value := Exec.Memory(Exec.IP + 2);
      end case;

      case LT_Instruction.Mode_3 is
         when Position  => Target_Pos := Exec.Memory(Exec.IP + 3);
         when Immediate => Target_Pos := Exec.IP + 3;
      end case;

      if First_Value < Second_Value then
         Exec.Memory(Target_Pos) := 1;
      else
         Exec.Memory(Target_Pos) := 0;
      end if;

      Log_Debug("Adding " & Long_Integer'Image(Exec.Memory(Target_Pos))
                & " to " & Long_Integer'Image(Target_Pos)
                & " after checking " & Long_Integer'Image(First_Value)
                & "<" & Long_Integer'Image(Second_Value));
      Exec.IP := Exec.IP + 4;

      return False;
   end LessThan;

   -- Equals instruction
   function Equals (Equals_Instruction : in Instruction;
                    Exec               : in out Execution) return Boolean is
      First_Value  : Long_Integer;
      Second_Value : Long_Integer;
      Target_Pos   : Long_Integer;
   begin
      case Equals_Instruction.Mode_1 is
         when Position => First_Value := Exec.Memory(Exec.Memory(Exec.IP + 1));
         when Immediate => First_Value := Exec.Memory(Exec.IP + 1);
      end case;

      case Equals_Instruction.Mode_2 is
         when Position => Second_Value := Exec.Memory(Exec.Memory(Exec.IP + 2));
         when Immediate => Second_Value := Exec.Memory(Exec.IP + 2);
      end case;

      case Equals_Instruction.Mode_3 is
         when Position  => Target_Pos := Exec.Memory(Exec.IP + 3);
         when Immediate => Target_Pos := Exec.IP + 3;
      end case;

      if First_Value = Second_Value then
         Exec.Memory(Target_Pos) := 1;
      else
         Exec.Memory(Target_Pos) := 0;
      end if;

      Log_Debug("Adding " & Long_Integer'Image(Exec.Memory(Target_Pos))
                & " to " & Long_Integer'Image(Target_Pos)
                & " after checking " & Long_Integer'Image(First_Value)
                & "=" & Long_Integer'Image(Second_Value));
      Exec.IP := Exec.IP + 4;

      return False;
   end Equals;

   procedure Run (Exec   : in out Execution;
                  Result : out Long_Integer) is
      Current_Instruction : Instruction;
      Interrupted : Boolean := False;
   begin
      Result := -1;
      Program_Loop :
      while (not Interrupted and not Exec.Finished) loop
         Log_Debug("IP: " & Long_Integer'Image(Exec.IP));
         Current_Instruction := Decode_Instruction(InstructionInt => Exec.Memory(Exec.IP));
         case Current_Instruction.Op is
            when Add =>
               Interrupted := Add(Add_Instruction => Current_Instruction,
                                  Exec => Exec);
            when Mul =>
               Interrupted := Mul(Mul_Instruction => Current_Instruction,
                                  Exec => Exec);
            when Input =>
               Interrupted := Input(Input_Instruction => Current_Instruction,
                                    Exec => Exec);
            when Output =>
               Interrupted := Output(Output_Instruction => Current_Instruction,
                                     Exec => Exec);
            when JumpTrue =>
               Interrupted := JumpTrue(JT_Instruction => Current_Instruction,
                                       Exec           => Exec);
            when JumpFalse =>
               Interrupted := JumpFalse(JF_Instruction => Current_Instruction,
                                        Exec => Exec);
            when LessThan =>
               Interrupted := LessThan(LT_Instruction => Current_Instruction,
                                       Exec => Exec);
            when Equals =>
               Interrupted := Equals(Equals_Instruction => Current_Instruction,
                                     Exec => Exec);
            when Finished =>
               Log_Debug("Execution of an amplifier finished!");
               Exec.Finished := True;
               Result := Exec.Last_Output.all;
               exit Program_Loop;
         end case;
      end loop Program_Loop;
   end Run;

   Initial_App       : Application;
   Running_Execution : Execution_Access;
   Intcode_System    : System;

   Sequence_Permutations : Permutations_Db;
   Start_Sequence : Phase_Setting_Sequence := (5, 6, 7, 8, 9);
   Next_Executor_Nr   : Integer := 0;

   Executor_Finished : Boolean := False;
   System_Finished   : Boolean := False;

   Thruster_Strength_Tmp : Long_Integer;
   Thruster_Strength_Max : Long_Integer := 0;
begin
   Load_File (Intcode => Initial_App);

   Generate_Permutations(Output   => Sequence_Permutations,
                         Sequence => Start_Sequence,
                         Elements => Start_Sequence'Length);



   Permutation_Loop:
   for I in Sequence_Permutations'Range loop
      Generate_Amplifier_System(Amplifiers => Intcode_System,
                                Initial_Memory => Initial_App);
      -- Set the input of the first system to 0.
      Intcode_System(0).Input_Value := new Long_Integer'(0);

      Init_Phase_Settings(Amplifiers => Intcode_System,
                          Sequence   => Sequence_Permutations(I));

      All_Boosters_Loop:
      while not System_Finished loop
         System_Finished := True;
         Running_Execution := Intcode_System(Next_Executor_Nr);
         if not Running_Execution.Finished then
            Run (Exec => Running_Execution.all,
                 Result => Thruster_Strength_Tmp);
            Next_Executor_Nr := Next_Executor_Nr + 1;
            if Next_Executor_Nr > Intcode_System'Last then
               Next_Executor_Nr := 0;
            end if;
            Intcode_System(Next_Executor_Nr).Input_Value := Running_Execution.Last_Output;

            if Running_Execution.Finished
              and then Thruster_Strength_Tmp > Thruster_Strength_Max then
               Log_Debug("Result: " & Long_Integer'Image(Thruster_Strength_Tmp));
               Thruster_Strength_Max := Thruster_Strength_Tmp;
            end if;
         end if;

         for S in Intcode_System'Range loop
            System_Finished := System_Finished and Intcode_System(S).all.Finished;
         end loop;
      end loop All_Boosters_Loop;

      System_Finished := False;
   end loop Permutation_Loop;

   Put_Line("Maxium strenght found: " & Long_Integer'Image(Thruster_Strength_Max));
end Day07_2;
