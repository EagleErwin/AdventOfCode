with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;

procedure Day19_1 is
   -- ### CONSTANTS ### --
   DEBUG : constant Boolean := False; -- Set this to True to enable debug logging
   Memory_Size : constant Long_Integer := 100_000; -- Index of last memory location (sufficient capacity)
   Grid_Size   : constant Integer := 49; -- Max X and negative X value for the grid surface.

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Application is array (0 .. Memory_Size) of Long_Integer;

   type Operation is (Add, Mul, Input, Output, JumpTrue, JumpFalse, LessThan, Equals, Adjust, Finished);
   type Parameter_Mode is (Position, Immediate, Relative);

   type Instruction is record
      Op     : Operation;
      Mode_1 : Parameter_Mode;
      Mode_2 : Parameter_Mode;
      Mode_3 : Parameter_Mode;
   end record;

   subtype Coordinate is Integer range 0 .. Grid_Size;
   type Coordinate_Access is access all Coordinate;
   type Force is (Stationary, Pulled, Unknown);
   type Force_Access is access Force;

   type Grid_Row is array (Coordinate'Range) of Force_Access;
   type Grid_Row_Access is access all Grid_Row;
   type Grid is array (Coordinate'Range) of Grid_Row_Access;

   type Execution is record
      Memory          : Application;
      IP              : Long_Integer;
      Input_X         : Coordinate_Access;
      Input_Y         : Coordinate_Access;
      Relative_Offset : Long_Integer;
      Output_Force    : Force_Access;
   end record;
   type Execution_Access is access all Execution;

   procedure Log_Debug (Log_Line : in String) is
   begin
      if (DEBUG) then
         Put_Line(Log_Line);
      end if;
   end Log_Debug;

   procedure Load_File (Intcode : out Application) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 0;
      Curr_Char    : Character;
      Buffer       : Unbounded_String;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day19.input");
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
      elsif Operation_String = "09" then
         return Adjust;
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
      elsif Mode_String = '2' then
         return Relative;
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

   function Reset (Intcode : in Application) return Execution is
      Result : Execution;
   begin
      Result := Execution'(Memory          => Intcode,
                           IP              => 0,
                           Input_X         => null, -- Will be overwritten directly
                           Input_Y         => null, -- Will be overwritten directly
                           Relative_Offset => 0,
                           Output_Force    => null);
      return Result;
   end Reset;

   -- Add instruction
   procedure Add (Add_Instruction : in Instruction;
                  Exec            : in Execution_Access) is
      First_Value  : Long_Integer;
      Second_Value : Long_Integer;
      Target_Pos   : Long_Integer;
   begin
      case Add_Instruction.Mode_1 is
         when Position  => First_Value := Exec.Memory(Exec.Memory(Exec.IP + 1));
         when Immediate => First_Value := Exec.Memory(Exec.IP + 1);
         when Relative  => First_Value := Exec.Memory(Exec.Memory(Exec.IP + 1) + Exec.Relative_Offset);
      end case;

      case Add_Instruction.Mode_2 is
         when Position  => Second_Value := Exec.Memory(Exec.Memory(Exec.IP + 2));
         when Immediate => Second_Value := Exec.Memory(Exec.IP + 2);
         when Relative  => Second_Value := Exec.Memory(Exec.Memory(Exec.IP + 2) + Exec.Relative_Offset);
      end case;

      case Add_Instruction.Mode_3 is
         when Position  => Target_Pos := Exec.Memory(Exec.IP + 3);
         when Immediate => Target_Pos := Exec.IP + 3;
         when Relative  => Target_Pos := Exec.Memory(Exec.IP + 3) + Exec.Relative_Offset;
      end case;
      Exec.Memory(Target_Pos) := First_Value + Second_Value;
      Log_Debug("Adding " & Long_Integer'Image(First_Value)
                & " + " & Long_Integer'Image(Second_Value)
                & " to " & Long_Integer'Image(Target_Pos));
      Exec.IP := Exec.IP + 4;
   end Add;


   -- Multiply instruction
   procedure Mul (Mul_Instruction : in Instruction;
                  Exec            : in Execution_Access) is
      First_Value  : Long_Integer;
      Second_Value : Long_Integer;
      Target_Pos   : Long_Integer := Exec.Memory(Exec.IP + 3);
   begin
      case Mul_Instruction.Mode_1 is
         when Position  => First_Value := Exec.Memory(Exec.Memory(Exec.IP + 1));
         when Immediate => First_Value := Exec.Memory(Exec.IP + 1);
         when Relative  => First_Value := Exec.Memory(Exec.Memory(Exec.IP + 1) + Exec.Relative_Offset);
      end case;

      case Mul_Instruction.Mode_2 is
         when Position  => Second_Value := Exec.Memory(Exec.Memory(Exec.IP + 2));
         when Immediate => Second_Value := Exec.Memory(Exec.IP + 2);
         when Relative  => Second_Value := Exec.Memory(Exec.Memory(Exec.IP + 2) + Exec.Relative_Offset);
      end case;

      case Mul_Instruction.Mode_3 is
         when Position  => Target_Pos := Exec.Memory(Exec.IP + 3);
         when Immediate => Target_Pos := Exec.IP + 3;
         when Relative  => Target_Pos := Exec.Memory(Exec.IP + 3) + Exec.Relative_Offset;
      end case;
      Log_Debug("Adding " & Long_Integer'Image(First_Value)
                & " * " & Long_Integer'Image(Second_Value)
                & " to " & Long_Integer'Image(Target_Pos));
      Exec.Memory(Target_Pos) := First_Value * Second_Value;
      Exec.IP := Exec.IP + 4;
   end Mul;

   procedure Input (Input_Instruction : in Instruction;
                    Exec              : in Execution_Access) is
      Target_Pos : Long_Integer;
   begin
      case Input_Instruction.Mode_1 is
         when Position  => Target_Pos := Exec.Memory(Exec.IP + 1);
         when Immediate => Target_Pos := Exec.IP + 1;
         when Relative  => Target_Pos := Exec.Memory(Exec.IP + 1) + Exec.Relative_Offset;
      end case;

      if Exec.Input_X /= null then
         Exec.Memory(Target_Pos) := Long_Integer(Exec.Input_X.all);
         Log_Debug("Providing " & Coordinate'Image(Exec.Input_X.all)
                   & " to " & Long_Integer'Image(Target_Pos));
         Exec.Input_X := null;
      elsif Exec.Input_Y /= null then
         Exec.Memory(Target_Pos) := Long_Integer(Exec.Input_Y.all);
         Log_Debug("Providing " & Coordinate'Image(Exec.Input_Y.all)
                   & " to " & Long_Integer'Image(Target_Pos));
         Exec.Input_Y := null;
      else
         Put_Line("ERROR: Unable to provide input value");
      end if;

      Exec.IP := Exec.IP + 2;
   end Input;

   procedure Output (Output_Instruction : in Instruction;
                     Exec               : in Execution_Access) is
      Output_Value : Long_Integer;
      Output_Force : Force;
   begin
      case Output_Instruction.Mode_1 is
         when Position  => Output_Value := Exec.Memory(Exec.Memory(Exec.IP + 1));
         when Immediate => Output_Value := Exec.Memory(Exec.IP + 1);
         when Relative  => Output_Value := Exec.Memory(Exec.Memory(Exec.IP + 1) + Exec.Relative_Offset);
      end case;

      Output_Force := Force'Val(Output_Value);
      Exec.Output_Force := new Force'(Output_Force);

      Exec.IP := Exec.IP + 2;
   end Output;

   -- JumpTrue instruction
   procedure JumpTrue (JT_Instruction : in Instruction;
                       Exec           : in Execution_Access) is
      First_Value  : Long_Integer;
      Second_Value : Long_Integer;
   begin
      case JT_Instruction.Mode_1 is
         when Position  => First_Value := Exec.Memory(Exec.Memory(Exec.IP + 1));
         when Immediate => First_Value := Exec.Memory(Exec.IP + 1);
         when Relative  => First_Value := Exec.Memory(Exec.Memory(Exec.IP + 1) + Exec.Relative_Offset);
      end case;

      case JT_Instruction.Mode_2 is
         when Position  => Second_Value := Exec.Memory(Exec.Memory(Exec.IP + 2));
         when Immediate => Second_Value := Exec.Memory(Exec.IP + 2);
         when Relative  => Second_Value := Exec.Memory(Exec.Memory(Exec.IP + 2) + Exec.Relative_Offset);
      end case;

      if First_Value /= 0 then
         Exec.IP := Second_Value;
         Log_Debug("JumpingTrue to " & Long_Integer'Image(Second_Value)
                   & " based on " & Long_Integer'Image(First_Value));
      else
         Exec.IP := Exec.IP + 3;
         Log_Debug("No JumpTrue based on " & Long_Integer'Image(First_Value));
      end if;
   end JumpTrue;

   -- JumpFalse instruction
   procedure JumpFalse (JF_Instruction : in Instruction;
                        Exec           : in Execution_Access) is
      First_Value  : Long_Integer;
      Second_Value : Long_Integer;
   begin
      case JF_Instruction.Mode_1 is
         when Position  => First_Value := Exec.Memory(Exec.Memory(Exec.IP + 1));
         when Immediate => First_Value := Exec.Memory(Exec.IP + 1);
         when Relative  => First_Value := Exec.Memory(Exec.Memory(Exec.IP + 1) + Exec.Relative_Offset);
      end case;

      case JF_Instruction.Mode_2 is
         when Position  => Second_Value := Exec.Memory(Exec.Memory(Exec.IP + 2));
         when Immediate => Second_Value := Exec.Memory(Exec.IP + 2);
         when Relative  => Second_Value := Exec.Memory(Exec.Memory(Exec.IP + 2) + Exec.Relative_Offset);
      end case;

      if First_Value = 0 then
         Exec.IP := Second_Value;
         Log_Debug("JumpingFalse to " & Long_Integer'Image(Second_Value)
                   & " based on " & Long_Integer'Image(First_Value));
      else
         Exec.IP := Exec.IP + 3;
         Log_Debug("No JumpFalse based on " & Long_Integer'Image(First_Value));
      end if;
   end JumpFalse;

   -- LessThan instruction
   procedure LessThan (LT_Instruction : in Instruction;
                       Exec           : in Execution_Access) is
      First_Value  : Long_Integer;
      Second_Value : Long_Integer;
      Target_Pos   : Long_Integer;
   begin
      case LT_Instruction.Mode_1 is
         when Position  => First_Value := Exec.Memory(Exec.Memory(Exec.IP + 1));
         when Immediate => First_Value := Exec.Memory(Exec.IP + 1);
         when Relative  => First_Value := Exec.Memory(Exec.Memory(Exec.IP + 1) + Exec.Relative_Offset);
      end case;

      case LT_Instruction.Mode_2 is
         when Position => Second_Value := Exec.Memory(Exec.Memory(Exec.IP + 2));
         when Immediate => Second_Value := Exec.Memory(Exec.IP + 2);
         when Relative  => Second_Value := Exec.Memory(Exec.Memory(Exec.IP + 2) + Exec.Relative_Offset);
      end case;

      case LT_Instruction.Mode_3 is
         when Position  => Target_Pos := Exec.Memory(Exec.IP + 3);
         when Immediate => Target_Pos := Exec.IP + 3;
         when Relative  => Target_Pos := Exec.Memory(Exec.IP + 3) + Exec.Relative_Offset;
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
   end LessThan;

   -- Equals instruction
   procedure Equals (Equals_Instruction : in Instruction;
                     Exec               : in Execution_Access) is
      First_Value  : Long_Integer;
      Second_Value : Long_Integer;
      Target_Pos   : Long_Integer;
   begin
      case Equals_Instruction.Mode_1 is
         when Position => First_Value := Exec.Memory(Exec.Memory(Exec.IP + 1));
         when Immediate => First_Value := Exec.Memory(Exec.IP + 1);
         when Relative  => First_Value := Exec.Memory(Exec.Memory(Exec.IP + 1) + Exec.Relative_Offset);
      end case;

      case Equals_Instruction.Mode_2 is
         when Position => Second_Value := Exec.Memory(Exec.Memory(Exec.IP + 2));
         when Immediate => Second_Value := Exec.Memory(Exec.IP + 2);
         when Relative  => Second_Value := Exec.Memory(Exec.Memory(Exec.IP + 2) + Exec.Relative_Offset);
      end case;

      case Equals_Instruction.Mode_3 is
         when Position  => Target_Pos := Exec.Memory(Exec.IP + 3);
         when Immediate => Target_Pos := Exec.IP + 3;
         when Relative  => Target_Pos := Exec.Memory(Exec.IP + 3) + Exec.Relative_Offset;
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
   end Equals;

   procedure Adjust (Adjust_Instruction : in Instruction;
                     Exec               : in Execution_Access) is
      Offset : Long_Integer;
   begin
      case Adjust_Instruction.Mode_1 is
         when Position  => Offset := Exec.Memory(Exec.Memory(Exec.IP + 1));
         when Immediate => Offset := Exec.Memory(Exec.IP + 1);
         when Relative  => Offset := Exec.Memory(Exec.Memory(Exec.IP + 1) + Exec.Relative_Offset);
      end case;

      Exec.Relative_Offset := Exec.Relative_Offset + Offset;
      Log_Debug("New relative offset: " & Long_Integer'Image(Exec.Relative_Offset));
      Exec.IP := Exec.IP + 2;
   end Adjust;

   function Run (Exec : in out Execution_Access) return Force is
      Current_Instruction : Instruction;
   begin
      Program_Loop :
      loop
         Log_Debug("IP: " & Long_Integer'Image(Exec.IP));
         Current_Instruction := Decode_Instruction(InstructionInt => Exec.Memory(Exec.IP));
         case Current_Instruction.Op is
            when Add => Add(Add_Instruction => Current_Instruction,
                            Exec => Exec);
            when Mul => Mul(Mul_Instruction => Current_Instruction,
                            Exec => Exec);
            when Input => Input(Input_Instruction => Current_Instruction,
                                Exec => Exec);
            when Output => Output(Output_Instruction => Current_Instruction,
                                  Exec => Exec);
            when JumpTrue => JumpTrue(JT_Instruction => Current_Instruction,
                                      Exec           => Exec);
            when JumpFalse => JumpFalse(JF_Instruction => Current_Instruction,
                                        Exec => Exec);
            when LessThan => LessThan(LT_Instruction => Current_Instruction,
                                      Exec => Exec);
            when Equals => Equals(Equals_Instruction => Current_Instruction,
                                  Exec => Exec);
            when Adjust => Adjust(Adjust_Instruction => Current_Instruction,
                                  Exec => Exec);
            when Finished => exit Program_Loop;
         end case;
      end loop Program_Loop;
      if Current_Instruction.Op /= Finished then
         Put_Line("ABORTED: Invalid instruction found at location "
                  & Long_Integer'Image(Exec.IP) & ": "
                  & Operation'Image(Current_Instruction.Op));
      end if;

      if Exec.Output_Force = null then
         Put_Line("ERROR: Did not recieve an answer from the robot");
         return Unknown;
      end if;

      return Exec.Output_Force.all;
   end Run;

   procedure Print_Grid (Output_Grid : in Grid) is
   begin
      Y_Loop:
      for Y in Coordinate'Range loop
         X_Loop:
         for X in Coordinate'Range loop
            if Output_Grid(Y)(X).all = Stationary then
               Put('.');
            elsif Output_Grid(Y)(X).all = Pulled then
               Put('#');
            else
               Put('?');
            end if;
         end loop X_Loop;
         Put_Line("");
      end loop Y_Loop;
   end Print_Grid;

   function Calculate_Answer(Output_Grid : in Grid) return Integer is
      Result : Integer := 0;
   begin
      Y_Loop:
      for Y in Coordinate'Range loop
         X_Loop:
         for X in Coordinate'Range loop
            if Output_Grid(Y)(X).all = Pulled then
               Result := Result + 1;
            end if;
         end loop X_Loop;
      end loop Y_Loop;
      return Result;
   end Calculate_Answer;

   Intcode_App       : Application;
   Intcode_Execution : Execution_Access;
   Environment       : Grid := (others => new Grid_Row'(others => new Force'(Unknown)));

   Measured_Force    : Force;
   Answer            : Integer;
begin
   Load_File (Intcode => Intcode_App);

   for Y in Coordinate'Range loop
      for X in Coordinate'Range loop
         Intcode_Execution := new Execution'(Reset (Intcode => Intcode_App));
         Intcode_Execution.Input_X := new Coordinate'(X);
         Intcode_Execution.Input_Y := new Coordinate'(Y);
         Measured_Force := Run (Exec => Intcode_Execution);
         Environment(Y)(X) := new Force'(Measured_Force);
      end loop;
   end loop;

   Print_Grid(Output_Grid => Environment);

   Answer := Calculate_Answer(Output_Grid => Environment);

   Put_Line("Number of points that are affected by the tractor beam:" & Integer'Image(Answer));
end Day19_1;
