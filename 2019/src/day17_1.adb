with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;

procedure Day17_1 is
   -- ### CONSTANTS ### --
   DEBUG : constant Boolean := False; -- Set this to True to enable debug logging
   Memory_Size : constant Long_Integer := 100_000; -- Index of last memory location (sufficient capacity)
   Grid_Size   : constant Integer := 2 ** 7; -- Max X and negative X value for the grid surface.

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

   type Object is (Scaffold, Open_Space, Robot_Up, Robot_Down, Robot_Left, Robot_Right, Robot_Fallen, Invalid);
   type Object_Access is access Object;

   type Grid_Row is array (0 .. Grid_Size) of Object_Access;
   type Grid_Row_Access is access all Grid_Row;
   type Grid is array (0 .. Grid_Size) of Grid_Row_Access;

   type Rotation is (Counter_Clock, Clockwise);
   type Rotation_Access is access Rotation;

   type Direction is (Up, Down, Left, Right);

   type Execution is record
      Memory          : Application;
      IP              : Long_Integer;
      Input_Value     : Long_Integer;
      Relative_Offset : Long_Integer;
      Current_X       : Integer;
      Current_Y       : Integer;
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
      Open (File => Input, Mode => In_File, Name => "data/day17.input");
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
      Result := Execution'(Memory           => Intcode,
                           IP               => 0,
                           Input_Value      => 0, -- Not used
                           Relative_Offset  => 0,
                           Current_X        => 0,
                           Current_Y        => 0);
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

      Exec.Memory(Target_Pos) := Exec.Input_Value;
      Log_Debug("Adding " & Long_Integer'Image(Exec.Input_Value)
                & " to " & Long_Integer'Image(Target_Pos));
      Exec.IP := Exec.IP + 2;
   end Input;

   procedure Output (Output_Instruction : in Instruction;
                     Exec               : in Execution_Access;
                     Output_Grid        : in out Grid) is
      Output_Value : Long_Integer;
      Curr_Char    : Character;
      Curr_Object  : Object;
   begin
      case Output_Instruction.Mode_1 is
         when Position  => Output_Value := Exec.Memory(Exec.Memory(Exec.IP + 1));
         when Immediate => Output_Value := Exec.Memory(Exec.IP + 1);
         when Relative  => Output_Value := Exec.Memory(Exec.Memory(Exec.IP + 1) + Exec.Relative_Offset);
      end case;

      Curr_Char := Character'Val(Integer(Output_value));
      if Integer(Output_Value) = 10 then
         Exec.Current_Y := Exec.Current_Y + 1;
         Exec.Current_X := 0;
      else
         case Curr_Char is
            when '#' => Curr_Object := Scaffold;
            when '.' => Curr_Object := Open_Space;
            when '^' => Curr_Object := Robot_Up;
            when 'v' => Curr_Object := Robot_Down;
            when '<' => Curr_Object := Robot_Left;
            when '>' => Curr_Object := Robot_Right;
            when 'X' => Curr_Object := Robot_Fallen;
            when others => Curr_Object := Invalid;
         end case;
         Output_Grid(Exec.Current_Y)(Exec.Current_X) := new Object'(Curr_Object);
         Exec.Current_X := Exec.Current_X + 1;
      end if;

      Put(Curr_Char);

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

   procedure Run (Exec        : in out Execution_Access;
                  Video_Frame : in out Grid) is
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
                                  Exec               => Exec,
                                  Output_Grid        => Video_Frame);
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
   end Run;

   function Get_Alignment_Parameter(Video_Frame : in Grid;
                                    X           : in Integer;
                                    Y           : in Integer) return Integer is
      Result : Integer := 0;
   begin
      if X /= 0 and then Y /= 0 and then -- Not in leftmost column or top row
        X /= (Grid_Size - 1) and then Y /= (Grid_Size - 1) and then -- Not in rightmost column or bottom row
        Video_Frame(Y-1)(X).all = Scaffold and then
        Video_Frame(Y+1)(X).all = Scaffold and then
        Video_Frame(Y)(X-1).all = Scaffold and then
        Video_Frame(Y)(X+1).all = Scaffold then
         Result := X * Y;
      end if;
      return Result;
   end Get_Alignment_Parameter;

   function Calculate_Alignment_Parameter_Sum(Video_Frame : in Grid) return Integer is
      Result : Integer := 0;
   begin
      Row_Loop:
      for Y in Video_Frame'Range loop
         Col_Loop:
         for X in Video_Frame(Y)'Range loop
            Result := Result + Get_Alignment_Parameter(Video_Frame, X, Y);
         end loop Col_Loop;
      end loop Row_Loop;
      return Result;
   end Calculate_Alignment_Parameter_Sum;

   Intcode_App       : Application;
   Intcode_Execution : Execution_Access;
   Camera_Image      : Grid := (others => new Grid_Row'(others => new Object'(Invalid)));
   Answer            : Integer;
begin
   Load_File (Intcode => Intcode_App);
   Intcode_Execution := new Execution'(Reset (Intcode => Intcode_App));

   Run (Exec        => Intcode_Execution,
        Video_Frame => Camera_Image);

   Answer := Calculate_Alignment_Parameter_Sum(Video_Frame => Camera_Image);

   Put_Line("Sum of alignment parameters:" & Integer'Image(Answer));
end Day17_1;
