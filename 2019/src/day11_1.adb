with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;

procedure Day11_1 is
   -- ### CONSTANTS ### --
   DEBUG : constant Boolean := False; -- Set this to True to enable debug logging
   Memory_Size : constant Long_Integer := 100_000; -- Index of last memory location (sufficient capacity)
   Grid_Size   : constant Integer := 2 ** 10; -- Max X and negative X value for the grid surface.

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

   type Color is (White, Black, Initial);
   type Color_Access is access Color;

   type Grid_Row is array (-Grid_Size .. Grid_Size) of Color_Access;
   type Grid_Row_Access is access all Grid_Row;
   type Grid is array (-Grid_Size .. Grid_Size) of Grid_Row_Access;

   type Rotation is (Counter_Clock, Clockwise);
   type Rotation_Access is access Rotation;

   type Direction is (Up, Down, Left, Right);

   type Execution is record
      Memory          : Application;
      IP              : Long_Integer;
      Input_Value     : Long_Integer;
      Relative_Offset : Long_Integer;
      Output_Color    : Color_Access;
      Output_Rotation : Rotation_Access;
   end record;
   type Execution_Access is access all Execution;

   type Robot is record
      Program     : Execution_Access;
      Pos_X       : Integer;
      Pos_Y       : Integer;
      Orientation : Direction;
      Paint_Count : Integer;
   end record;

   -- ### VARIABLES ### --
   Last_Output : Long_Integer := 0;

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
      Open (File => Input, Mode => In_File, Name => "data/day11.input");
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
                           Input_Value      => 0, -- Initially black
                           Relative_Offset  => 0,
                           Output_Color     => null,
                           Output_Rotation  => null);
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
                     Exec               : in Execution_Access) is
      Output_Value : Long_Integer;
   begin
      case Output_Instruction.Mode_1 is
         when Position  => Output_Value := Exec.Memory(Exec.Memory(Exec.IP + 1));
         when Immediate => Output_Value := Exec.Memory(Exec.IP + 1);
         when Relative  => Output_Value := Exec.Memory(Exec.Memory(Exec.IP + 1) + Exec.Relative_Offset);
      end case;
      if Exec.Output_Color = null then
         if Output_Value = 0 then
            Exec.Output_Color := new Color'(Black);
         else
            Exec.Output_Color := new Color'(White);
         end if;
         Log_Debug("Output: Colour should be " & Color'Image(Exec.Output_Color.all));
      elsif Exec.Output_Rotation = null then
         if Output_Value = 0 then
            Exec.Output_Rotation := new Rotation'(Counter_Clock);
         else
            Exec.Output_Rotation := new Rotation'(Clockwise);
         end if;
         Log_Debug("Output: Rotation should be " & Rotation'Image(Exec.Output_Rotation.all));
      end if;

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

   procedure Paint (Painting_Robot : in out Robot;
                    Surface_Grid   : in out Grid) is
      New_Color : Color_Access;
   begin
      New_Color := Painting_Robot.Program.Output_Color;
      if Surface_Grid(Painting_Robot.Pos_Y)(Painting_Robot.Pos_X).all = Initial then
         Painting_Robot.Paint_Count := Painting_Robot.Paint_Count + 1;
      end if;

      Surface_Grid(Painting_Robot.Pos_Y)(Painting_Robot.Pos_X) := New_Color;

      Log_Debug("Painted (" & Integer'Image(Painting_Robot.Pos_X) & ","
                & Integer'Image(Painting_Robot.Pos_Y) & ") "
                & Color'Image(New_Color.all));
   end Paint;

   procedure Rotate_And_Move (Painting_Robot : in out Robot) is
      Old_X         : Integer := Painting_Robot.Pos_X;
      Old_Y         : Integer := Painting_Robot.Pos_Y;
   begin
      -- Rotate
      if Painting_Robot.Program.Output_Rotation.all = Counter_Clock then
         case Painting_Robot.Orientation is
            when Up    => Painting_Robot.Orientation := Left;
            when Right => Painting_Robot.Orientation := Up;
            when Down  => Painting_Robot.Orientation := Right;
            when Left  => Painting_Robot.Orientation := Down;
         end case;
      else --Clockwise
         case Painting_Robot.Orientation is
            when Up    => Painting_Robot.Orientation := Right;
            when Right => Painting_Robot.Orientation := Down;
            when Down  => Painting_Robot.Orientation := Left;
            when Left  => Painting_Robot.Orientation := Up;
         end case;
      end if;

      -- Move
      case Painting_Robot.Orientation is
         when Up    => Painting_Robot.Pos_Y := Old_Y + 1;
         when Right => Painting_Robot.Pos_X := Old_X + 1;
         when Down  => Painting_Robot.Pos_Y := Old_Y - 1;
         when Left  => Painting_Robot.Pos_X := Old_X - 1;
      end case;

      Log_Debug("Rotated and moved "
                & Rotation'Image(Painting_Robot.Program.Output_Rotation.all)
                & " from (" & Integer'Image(Old_X) & ","
                & Integer'Image(Old_Y) & ") to ("
                & Integer'Image(Painting_Robot.Pos_X) & ","
                & Integer'Image(Painting_Robot.Pos_Y) & ")");
   end Rotate_And_Move;

   procedure Update_Input (Painting_Robot : in out Robot;
                           Surface_Grid   : in out Grid) is
      Current_Color : Color := Surface_Grid(Painting_Robot.Pos_Y)(Painting_Robot.Pos_X).all;
   begin
      case Current_Color is
         when Initial => Painting_Robot.Program.Input_Value := 0; -- Initial is black
         when Black   => Painting_Robot.Program.Input_Value := 0;
         when White   => Painting_Robot.Program.Input_Value := 1;
      end case;

      Log_Debug("Feeding new color " & Color'Image(Current_Color) & " to robot");
   end Update_Input;

   procedure Reset_Output (Painting_Robot : in out Robot) is
   begin
      Painting_Robot.Program.Output_Color := null;
      Painting_Robot.Program.Output_Rotation := null;
      Log_Debug("Reset robot outputs");
   end Reset_Output;

   procedure Run (Painting_Robot : in out Robot;
                  Surface_Grid   : in out Grid) is
      Current_Instruction : Instruction;
      Exec                : Execution_Access := Painting_Robot.Program;
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

         if Exec.Output_Color /= null and then Exec.Output_Rotation /= null then
            Paint(Painting_Robot, Surface_Grid);
            Rotate_And_Move(Painting_Robot);
            Update_Input(Painting_Robot, Surface_Grid);
            Reset_Output(Painting_Robot);
         end if;
      end loop Program_Loop;
      if Current_Instruction.Op /= Finished then
         Put_Line("ABORTED: Invalid instruction found at location "
                  & Long_Integer'Image(Exec.IP) & ": "
                  & Operation'Image(Current_Instruction.Op));
      end if;
   end Run;

   Intcode_App                   : Application;
   Intcode_Execution             : Execution;
   Emergency_Hull_Painting_Robot : Robot;
   Ship_Surface                  : Grid := (others => new Grid_Row'(others => new Color'(Initial)));
begin
   Load_File (Intcode => Intcode_App);
   Intcode_Execution := Reset (Intcode => Intcode_App);

   Emergency_Hull_Painting_Robot := Robot'(Program     => new Execution'(Intcode_Execution),
                                           Pos_X       => 0,
                                           Pos_Y       => 0,
                                           Orientation => Up,
                                           Paint_Count => 0);
   Run (Painting_Robot => Emergency_Hull_Painting_Robot,
        Surface_Grid   => Ship_Surface);

   Put_Line("Number of tiles painted:"
            & Integer'Image(Emergency_Hull_Painting_Robot.Paint_Count));
end Day11_1;
