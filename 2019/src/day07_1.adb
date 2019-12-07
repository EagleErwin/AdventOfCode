with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;

procedure Day07_1 is
   -- ### CONSTANTS ### --
   Memory_Size : constant Integer := 999; -- Index of last memory location (sufficient capacity)

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Application is array (0 .. Memory_Size) of Integer;

   type Operation is (Add, Mul, Input, Output, JumpTrue, JumpFalse, LessThan, Equals, Finished);
   type Parameter_Mode is (Position, Immediate);

   type Instruction is record
      Op     : Operation;
      Mode_1 : Parameter_Mode;
      Mode_2 : Parameter_Mode;
      Mode_3 : Parameter_Mode;
   end record;

   type Input_Array is array (0 .. 9) of Integer;
   type Phase_Setting_Sequence is array (0..4) of Integer range 0 .. 4;
   type Permutations_Db is array (0 .. 119) of Phase_Setting_Sequence;

   -- ### GLOBAL VARIABLES ### --
   Highest_Signal : Integer := 0;
   Latest_Output : Integer;
   -- Provides the input to the program based on the Inputs_Ptr
   Inputs : Input_Array := (others => 0);
   Inputs_Ptr : Integer := 0; -- Indicates which input to use
   Output_Ptr : Integer := 3; -- Indicates where the next output should be written
   Permutations_Ptr : Integer := 0; -- Indicates which permutation should be written

   -- Thank you https://www.topcoder.com/generating-permutations/
   procedure Generate_Permutations(Output : in out Permutations_Db;
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
            Intcode(Idx) := Integer'Value(To_String(Buffer));
            Buffer := To_Unbounded_String("");
            Idx := Idx + 1;
         else
            Buffer := Buffer & Curr_Char;
         end if;
      end loop;
      Intcode(Idx) := Integer'Value(To_String(Buffer));
   end Load_File;

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

   function Decode_Instruction (InstructionInt : in Integer) return Instruction is
      InstructionString : String(1..5) := (others => '0');
      InstructionIntString : String := Trim(Integer'Image(InstructionInt), Ada.Strings.Left);
      The_Operation : Operation;
      Result : Instruction;
   begin
      --Put_Line("Decoding " & Integer'Image(InstructionInt));
      InstructionString := Overwrite(Source   => InstructionString,
                                     Position => (1 + InstructionString'Length - InstructionIntString'Length),
                                     New_Item => InstructionIntString);
      --Put_Line(InstructionString);
      The_Operation := Decode_Operation(InstructionString(4..5));

      Result := Instruction'(Op => The_Operation,
                             Mode_1 => Decode_Mode(InstructionString(3)),
                             Mode_2 => Decode_Mode(InstructionString(2)),
                             Mode_3 => Decode_Mode(InstructionString(1)));
      return Result;
   end Decode_Instruction;

   function Reset (Intcode : in Application) return Application is
      Result : Application;
   begin
      Result := Intcode;
      return Result;
   end Reset;

   procedure Init_Inputs (Sequence : in Phase_Setting_Sequence) is
   begin
      Put("Phase setting sequence:");
      Put(Integer'Image(Sequence(0)));
      Put(Integer'Image(Sequence(1)));
      Put(Integer'Image(Sequence(2)));
      Put(Integer'Image(Sequence(3)));
      Put(Integer'Image(Sequence(4)));

      Output_Ptr := 3;
      Inputs_Ptr := 0;
      Inputs := (others => 0);
      Inputs(0) := Sequence(0); -- Phase setting A
      Inputs(1) := 0;           -- Input for A
      Inputs(2) := Sequence(1); -- Phase setting B
                                -- Output from A
      Inputs(4) := Sequence(2); -- Phase setting C
                                -- Output from B
      Inputs(6) := Sequence(3); -- Phase setting D
                                -- Output from C
      Inputs(8) := Sequence(4); -- Phase setting E
                                -- Output from D
   end Init_Inputs;

   -- Add instruction
   procedure Add (Add_Instruction : in Instruction;
                  Intcode         : in out Application;
                  IP              : in out Integer) is
      First_Value  : Integer;
      Second_Value : Integer;
      Target_Pos   : Integer := Intcode(IP + 3);
   begin
      case Add_Instruction.Mode_1 is
         when Position => First_Value := Intcode(Intcode(IP + 1));
         when Immediate => First_Value := Intcode(IP + 1);
      end case;

      case Add_Instruction.Mode_2 is
         when Position => Second_Value := Intcode(Intcode(IP + 2));
         when Immediate => Second_Value := Intcode(IP + 2);
      end case;

      Intcode(Target_Pos) := First_Value + Second_Value;
      --Put_Line("Adding " & Integer'Image(First_Value) & " + " & Integer'Image(Second_Value) & " to " & Integer'Image(Target_Pos));
      IP := IP + 4;
   end Add;


   -- Multiply instruction
   procedure Mul (Mul_Instruction : in Instruction;
                  Intcode         : in out Application;
                  IP              : in out Integer) is
      First_Value  : Integer;
      Second_Value : Integer;
      Target_Pos   : Integer := Intcode(IP + 3);
   begin
      case Mul_Instruction.Mode_1 is
         when Position => First_Value := Intcode(Intcode(IP + 1));
         when Immediate => First_Value := Intcode(IP + 1);
      end case;

      case Mul_Instruction.Mode_2 is
         when Position => Second_Value := Intcode(Intcode(IP + 2));
         when Immediate => Second_Value := Intcode(IP + 2);
      end case;

      Intcode(Target_Pos) := First_Value * Second_Value;
      --Put_Line("Adding " & Integer'Image(First_Value) & " * " & Integer'Image(Second_Value) & " to " & Integer'Image(Target_Pos));
      IP := IP + 4;
   end Mul;

   procedure Input (Target_Pos : in Integer;
                    Intcode    : in out Application;
                    IP         : in out Integer) is
      Input_Value : Integer := 5;
   begin
      Intcode(Target_Pos) := Inputs(Inputs_Ptr);
      Inputs_Ptr := Inputs_Ptr + 1;
      --Put_Line("Adding " & Integer'Image(Input_Value) & " to " & Integer'Image(Target_Pos));
      IP := IP + 2;
   end Input;

   procedure Output (Position : in Integer;
                     Intcode  : in out Application;
                     IP       : in out Integer) is
   begin
      Latest_Output := Intcode(Position);
      if Output_Ptr < Inputs'Length then
         Inputs(Output_Ptr) := Latest_Output;
         Output_Ptr := Output_Ptr + 2;
      else
         --Put_Line("Output can not be stored in Inputs array. Expecting this is the last one.");
         Put_Line(" Output: " & Integer'Image(Latest_Output));
      end if;
      --Put_Line("Output: " & Integer'Image(Latest_Output));
      IP := IP + 2;
   end Output;

   -- JumpTrue instruction
   procedure JumpTrue (JT_Instruction : in Instruction;
                       Intcode        : in out Application;
                       IP             : in out Integer) is
      First_Value  : Integer;
      Second_Value : Integer;
   begin
      case JT_Instruction.Mode_1 is
         when Position => First_Value := Intcode(Intcode(IP + 1));
         when Immediate => First_Value := Intcode(IP + 1);
      end case;

      case JT_Instruction.Mode_2 is
         when Position => Second_Value := Intcode(Intcode(IP + 2));
         when Immediate => Second_Value := Intcode(IP + 2);
      end case;

      if First_Value /= 0 then
         IP := Second_Value;
         --Put_Line("JumpingTrue to " & Integer'Image(Second_Value) & " based on " & Integer'Image(First_Value));
      else
         IP := IP + 3;
         --Put_Line("No JumpTrue based on " & Integer'Image(First_Value));
      end if;
   end JumpTrue;

   -- JumpFalse instruction
   procedure JumpFalse (JF_Instruction : in Instruction;
                        Intcode        : in out Application;
                        IP             : in out Integer) is
      First_Value  : Integer;
      Second_Value : Integer;
   begin
      case JF_Instruction.Mode_1 is
         when Position => First_Value := Intcode(Intcode(IP + 1));
         when Immediate => First_Value := Intcode(IP + 1);
      end case;

      case JF_Instruction.Mode_2 is
         when Position => Second_Value := Intcode(Intcode(IP + 2));
         when Immediate => Second_Value := Intcode(IP + 2);
      end case;

      if First_Value = 0 then
         IP := Second_Value;
         --Put_Line("JumpingFalse to " & Integer'Image(Second_Value) & " based on " & Integer'Image(First_Value));
      else
         IP := IP + 3;
         --Put_Line("No JumpFalse based on " & Integer'Image(First_Value));
      end if;
   end JumpFalse;

   -- LessThan instruction
   procedure LessThan (LT_Instruction : in Instruction;
                       Intcode        : in out Application;
                       IP             : in out Integer) is
      First_Value  : Integer;
      Second_Value : Integer;
      Target_Pos   : Integer := Intcode(IP + 3);
   begin
      case LT_Instruction.Mode_1 is
         when Position => First_Value := Intcode(Intcode(IP + 1));
         when Immediate => First_Value := Intcode(IP + 1);
      end case;

      case LT_Instruction.Mode_2 is
         when Position => Second_Value := Intcode(Intcode(IP + 2));
         when Immediate => Second_Value := Intcode(IP + 2);
      end case;

      if First_Value < Second_Value then
         Intcode(Target_Pos) := 1;
      else
         Intcode(Target_Pos) := 0;
      end if;

      --Put_Line("Adding " & Integer'Image(Intcode(Target_Pos)) & " to " & Integer'Image(Target_Pos) & " after checking " & Integer'Image(First_Value) & "<" & Integer'Image(Second_Value));
      IP := IP + 4;
   end LessThan;

   -- Equals instruction
   procedure Equals (Equals_Instruction : in Instruction;
                       Intcode        : in out Application;
                       IP             : in out Integer) is
      First_Value  : Integer;
      Second_Value : Integer;
      Target_Pos   : Integer := Intcode(IP + 3);
   begin
      case Equals_Instruction.Mode_1 is
         when Position => First_Value := Intcode(Intcode(IP + 1));
         when Immediate => First_Value := Intcode(IP + 1);
      end case;

      case Equals_Instruction.Mode_2 is
         when Position => Second_Value := Intcode(Intcode(IP + 2));
         when Immediate => Second_Value := Intcode(IP + 2);
      end case;

      if First_Value = Second_Value then
         Intcode(Target_Pos) := 1;
      else
         Intcode(Target_Pos) := 0;
      end if;

      --Put_Line("Adding " & Integer'Image(Intcode(Target_Pos)) & " to " & Integer'Image(Target_Pos) & " after checking " & Integer'Image(First_Value) & "=" & Integer'Image(Second_Value));
      IP := IP + 4;
   end Equals;

   function Run (Intcode : in out Application) return Integer is
      Instruction_Pointer : Integer := 0;
      Current_Instruction : Instruction;
   begin
      Program_Loop :
      loop
         --Put_Line("IP: " & Integer'Image(Instruction_Pointer));
         Current_Instruction := Decode_Instruction(InstructionInt => Intcode(Instruction_Pointer));
         case Current_Instruction.Op is
            when Add => Add(Add_Instruction => Current_Instruction,
                            Intcode => Intcode,
                            IP => Instruction_Pointer);
            when Mul => Mul(Mul_Instruction => Current_Instruction,
                            Intcode => Intcode,
                            IP => Instruction_Pointer);
            when Input => Input(Target_Pos => Intcode(Instruction_Pointer + 1),
                                Intcode => Intcode,
                                IP => Instruction_Pointer);
            when Output => Output(Position => Intcode(Instruction_Pointer + 1),
                             Intcode => Intcode,
                             IP => Instruction_Pointer);
            when JumpTrue => JumpTrue(JT_Instruction => Current_Instruction,
                                      Intcode => Intcode,
                                      IP => Instruction_Pointer);
            when JumpFalse => JumpFalse(JF_Instruction => Current_Instruction,
                                        Intcode => Intcode,
                                        IP => Instruction_Pointer);
            when LessThan => LessThan(LT_Instruction => Current_Instruction,
                                      Intcode => Intcode,
                                      IP => Instruction_Pointer);
            when Equals => Equals(Equals_Instruction => Current_Instruction,
                                  Intcode => Intcode,
                                  IP => Instruction_Pointer);
            when Finished => exit Program_Loop;
            when others => exit Program_Loop;
         end case;
      end loop Program_Loop;
      if Current_Instruction.Op /= Finished then
         Put_Line("ABORTED: Invalid instruction found at location " & Integer'Image(Instruction_Pointer) & ": " & Operation'Image(Current_Instruction.Op));
      end if;
      return Latest_Output;
   end Run;

   Intcode_App : Application;
   Intcode_Try : Application;
   Thruster_Strenght : Integer;
   Sequence_Permutations : Permutations_Db;

   Start_Sequence : Phase_Setting_Sequence := (0, 1, 2, 3, 4);
begin
   Load_File (Intcode => Intcode_App);

   Generate_Permutations(Output   => Sequence_Permutations,
                         Sequence => Start_Sequence,
                         Elements => 5);

   Permutation_Loop:
   for I in Sequence_Permutations'Range loop
      Init_Inputs(Sequence_Permutations(I));

      All_Boosters_Loop:
      for N in 0 .. 4 loop
         Intcode_Try := Reset (Intcode => Intcode_App);

         Thruster_Strenght := Run (Intcode => Intcode_Try);

         if Thruster_Strenght > Highest_Signal then
            Highest_Signal := Thruster_Strenght;
         end if;
      end loop All_Boosters_Loop;

   end loop Permutation_Loop;

   Put_Line("Maxium strenght found: " & Integer'Image(Highest_Signal));
end Day07_1;
