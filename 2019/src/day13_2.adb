with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;

procedure Day13_2 is
   -- ### CONSTANTS ### --
   DEBUG : constant Boolean := False; -- Set this to True to enable debug logging
   MANUAL : constant Boolean := False; -- Set this to True to play manually.
   Memory_Size : constant Long_Integer := 100_000; -- Index of last memory location (sufficient capacity)
   Grid_Size   : constant Integer := 2 ** 6; -- Max X and negative X value for the grid surface.

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   subtype Coordinate is Integer range 0 .. Grid_Size;
   type Coordinate_Access is access all Coordinate;

   type Application is array (0 .. Memory_Size) of Long_Integer;

   type Operation is (Add, Mul, Input, Output, JumpTrue, JumpFalse, LessThan, Equals, Adjust, Finished);
   type Parameter_Mode is (Position, Immediate, Relative);

   type Instruction is record
      Op     : Operation;
      Mode_1 : Parameter_Mode;
      Mode_2 : Parameter_Mode;
      Mode_3 : Parameter_Mode;
   end record;

   type Tile is (Empty, Wall, Block, Horizontal_Paddle, Ball);
   type Tile_Access is access all Tile;

   type Score_Int is access all Integer;

   type Execution is record
      Memory          : Application;
      IP              : Long_Integer;
      Input_Value     : Long_Integer;
      Relative_Offset : Long_Integer;
      Output_X        : Coordinate_Access;
      Output_Y        : Coordinate_Access;
      Output_Tile     : Tile_Access;
      Score_Display   : Boolean;
      Score           : Score_Int;
      Ball_X          : Coordinate_Access;
      Paddle_X        : Coordinate_Access;
   end record;
   type Execution_Access is access all Execution;

   type Grid_Row is array (Coordinate'Range) of Tile_Access;
   type Grid_Row_Access is access all Grid_Row;
   type Grid is array (Coordinate'Range) of Grid_Row_Access;

   -- ### Global variables ### --
   Last_Score : Integer := 0;

   procedure Log_Debug (Log_Line : in String) is
   begin
      if (DEBUG) then
         Put_Line(Log_Line);
      end if;
   end Log_Debug;

   procedure Print_Grid (Surface_Grid : in Grid) is
      Max_X : Integer := -Integer'Last;
      Max_Y : Integer := -Integer'Last;
   begin
      Put_Line("Score:" & Integer'Image(Last_Score));
      Row_Loop:
      for R in Surface_Grid'Range loop
         Column_Loop:
         for C in Surface_Grid(R)'Range loop
            if Surface_Grid(R)(C).all /= Empty then
               Max_Y := Integer'Max(Max_Y, R);
               Max_X := Integer'Max(Max_X, C);
            end if;
         end loop Column_Loop;
      end loop Row_Loop;

      Y_Loop:
      for Y in Surface_Grid'First .. Max_Y loop
         X_Loop:
         for X in Surface_Grid(Y)'First .. Max_X loop
            case Surface_Grid(Y)(X).all is
               when Empty => Put(' ');
               when Wall => Put('|');
               when Block => Put('#');
               when Horizontal_Paddle => Put('=');
               when Ball => Put('o');
            end case;
         end loop X_Loop;
         Put_Line("");
      end loop Y_Loop;
   end Print_Grid;

   procedure Load_File (Intcode : out Application) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 0;
      Curr_Char    : Character;
      Buffer       : Unbounded_String;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day13.input");
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
                           Output_X         => null,
                           Output_Y         => null,
                           Output_Tile      => null,
                           Score_Display    => False,
                           Score            => null,
                           Ball_X           => null,
                           Paddle_X         => null);
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

   procedure Get_User_Input(Exec : in Execution_Access) is
      Input_Char : Character;
   begin
      Put_Line(">>> ");
      Ada.Text_IO.Get(Item => Input_Char);
      case Input_Char is
         when '<' => Exec.Input_Value := -1;
         when '>' => Exec.Input_Value := 1;
         when ' ' => Exec.Input_Value := 0;
         when others => Get_User_Input(Exec);
      end case;
   end Get_User_Input;

   procedure Get_Automated_Input(Exec : in Execution_Access) is
   begin
      if Exec.Ball_X.all > Exec.Paddle_X.all then
         Exec.Input_Value := 1;
      elsif Exec.Ball_X.all < Exec.Paddle_X.all then
         Exec.Input_Value := -1;
      else
         Exec.Input_Value := 0;
      end if;
   end Get_Automated_Input;

   procedure Input (Input_Instruction : in Instruction;
                    Exec              : in Execution_Access;
                    Current_Grid      : in Grid) is
      Target_Pos : Long_Integer;
   begin
      case Input_Instruction.Mode_1 is
         when Position  => Target_Pos := Exec.Memory(Exec.IP + 1);
         when Immediate => Target_Pos := Exec.IP + 1;
         when Relative  => Target_Pos := Exec.Memory(Exec.IP + 1) + Exec.Relative_Offset;
      end case;

      if MANUAL then
         Print_Grid(Current_Grid);
         Get_User_Input(Exec);
      else
         Get_Automated_Input(Exec);
      end if;
      Exec.Memory(Target_Pos) := Exec.Input_Value;
      Log_Debug("Adding " & Long_Integer'Image(Exec.Input_Value)
                & " to " & Long_Integer'Image(Target_Pos));
      Exec.IP := Exec.IP + 2;
   end Input;

   procedure Output (Output_Instruction : in Instruction;
                     Exec               : in Execution_Access) is
      Output_Value : Long_Integer;
      New_Tile     : Tile;
   begin
      case Output_Instruction.Mode_1 is
         when Position  => Output_Value := Exec.Memory(Exec.Memory(Exec.IP + 1));
         when Immediate => Output_Value := Exec.Memory(Exec.IP + 1);
         when Relative  => Output_Value := Exec.Memory(Exec.Memory(Exec.IP + 1) + Exec.Relative_Offset);
      end case;

      -- Check if we get a score or a tile
      if Exec.Score_Display then
         if Exec.Output_Y = null then
            if Output_Value /= 0 then
               Put_Line("Error: Expected 0 as Y coordinate");
            end if;
            Exec.Output_Y := new Coordinate'(Integer(Output_Value));
         else
            Exec.Score := new Integer'(Integer(Output_Value));
         end if;
      else
         if Exec.Output_X = null then
            if Output_Value = -1 then
               Exec.Score_Display := True;
            else
               Exec.Output_X := new Coordinate'(Integer(Output_Value));
            end if;
         elsif Exec.Output_Y = null then
            Exec.Output_Y := new Coordinate'(Integer(Output_Value));
         else
            New_Tile := Tile'Val(Output_Value);
            Exec.Output_Tile := new Tile'(New_Tile);
            if New_Tile = Ball then
               Exec.Ball_X := new Coordinate'(Exec.Output_X.all);
            elsif New_Tile = Horizontal_Paddle then
               Exec.Paddle_X := new Coordinate'(Exec.Output_X.all);
            end if;
         end if;
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

   procedure Reset_Output (Exec : in out Execution_Access) is
   begin
      Exec.Output_X := null;
      Exec.Output_Y := null;
      Exec.Output_Tile := null;
      Exec.Score_Display := False;
      Exec.Score := null;
      Log_Debug("Reset outputs");
   end Reset_Output;

   procedure Insert_Quarter (Exec : in out Execution_Access) is
   begin
      Exec.Memory(0) := 2; -- Don't insert a quarter. Just hack it!
   end Insert_Quarter;

   procedure Run (Exec         : in out Execution_Access;
                  Surface_Grid : in out Grid) is
      Current_Instruction : Instruction;
      Tile_Complete  : Boolean;
      Score_Complete : Boolean;
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
                                Exec => Exec,
                                Current_Grid => Surface_Grid);
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

         Tile_Complete := Exec.Output_X /= null and then Exec.Output_Y /= null and then Exec.Output_Tile /= null;
         Score_Complete := Exec.Score_Display and then Exec.Output_Y /= null and then Exec.Score /= null;
         if Tile_Complete then
            Surface_Grid(Exec.Output_Y.all)(Exec.Output_X.all) := Exec.Output_Tile;
            Reset_Output(Exec);
         elsif Score_Complete then
            Last_Score := Exec.Score.all;
            Reset_Output(Exec);
         end if;
      end loop Program_Loop;
      if Current_Instruction.Op /= Finished then
         Put_Line("ABORTED: Invalid instruction found at location "
                  & Long_Integer'Image(Exec.IP) & ": "
                  & Operation'Image(Current_Instruction.Op));
      end if;
   end Run;

   Intcode_App       : Application;
   Intcode_Execution : Execution_Access;
   Game_Surface      : Grid := (others => new Grid_Row'(others => new Tile'(Empty)));
begin
   Load_File (Intcode => Intcode_App);
   Intcode_Execution := new Execution'(Reset (Intcode => Intcode_App));

   Insert_Quarter(Exec => Intcode_Execution);

   Run (Exec => Intcode_Execution, Surface_Grid   => Game_Surface);

   Put_Line("Game over! Final score:" & Integer'Image(Last_Score));
end Day13_2;
