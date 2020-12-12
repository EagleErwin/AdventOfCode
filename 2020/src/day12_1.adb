with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Containers.Generic_Array_Sort;

procedure Day12_1 is
   -- ### CONSTANTS ### --
   Enable_Debug     : constant Boolean := False;
   Number_Of_Inputs : constant Integer := 781;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Direction is (North, East, South, West); -- Direction matters!
   type Position is record
      Longitude : Integer; --X
      Latitude  : Integer; --Y
      Heading   : Direction;
   end record;

   type Action is (North, South, East, West, Left, Right, Forward);
   type Navigation_Instruction is record
      Nav_Action : Action;
      Value      : Integer;
   end record;

   type Navigation_Sequence is array (1 .. Number_Of_Inputs) of Navigation_Instruction;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   function Char_To_Action (Char : in Character) return Action is
   begin
      case Char is
         when 'N' => return North;
         when 'S' => return South;
         when 'E' => return East;
         when 'W' => return West;
         when 'L' => return Left;
         when 'R' => return Right;
         when 'F' => return Forward;
         when others =>
            Put_Line("ERROR: Unable to create action from Character " & Char);
            return Forward; -- We have to return something.
      end case;
   end Char_To_Action;

   procedure Load_File (Result : out Navigation_Sequence) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 1;

      Action_Char  : Character;
      Action_Value : Integer;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day12.input");
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Action_Char := Element(Current_Line, 1);
         Action_Value := Integer'Value(Slice(Source => Current_Line,
                                             Low    => 2,
                                             High   => Length(Current_Line)));
         Result(Idx) := Navigation_Instruction'(Nav_Action => Char_To_Action(Action_Char),
                                                Value      => Action_Value);
         Idx := Idx + 1;
      end loop;
      Close (File => Input);
   end Load_File;

   procedure Rotate (Ferry : in out Position;
                     Instr : in     Navigation_Instruction) is
      Steps : Integer := Instr.Value / 90;
   begin
      Rotate_Loop:
      for N in 1 .. Steps loop
         case Instr.Nav_Action is
            when Left   =>
               if Ferry.Heading = North then
                  Ferry.Heading := West;
               else
                  Ferry.Heading := Direction'Pred(Ferry.Heading);
               end if;
            when Right  =>
               if Ferry.Heading = West then
                  Ferry.Heading := North;
               else
                  Ferry.Heading := Direction'Succ(Ferry.Heading);
               end if;
            when others => Put_Line("ERROR: Invalid action to rotate "
                                    & Action'Image(Instr.Nav_Action));
         end case;
      end loop Rotate_Loop;
   end Rotate;

   procedure Move (Ferry  : in out Position;
                   Amount : in     Integer) is
   begin
      case Ferry.Heading is
         when North => Ferry.Latitude := Ferry.Latitude + Amount;
         when South => Ferry.Latitude := Ferry.Latitude - Amount;
         when East => Ferry.Longitude := Ferry.Longitude + Amount;
         when West => Ferry.Longitude := Ferry.Longitude - Amount;
      end case;
   end Move;

   procedure Update_Ferry (Instruction : in     Navigation_Instruction;
                           Ferry       : in out Position) is
   begin
      case Instruction.Nav_Action is
         when North   => Ferry.Latitude := Ferry.Latitude + Instruction.Value;
         when South   => Ferry.Latitude := Ferry.Latitude - Instruction.Value;
         when East    => Ferry.Longitude := Ferry.Longitude + Instruction.Value;
         when West    => Ferry.Longitude := Ferry.Longitude - Instruction.Value;
         when Left    => Rotate(Ferry, Instruction);
         when Right   => Rotate(Ferry, Instruction);
         when Forward => Move(Ferry, Instruction.Value);
      end case;
   end Update_Ferry;

   Computer_Output : Navigation_Sequence;
   Ferry_Position  : Position := Position'(Longitude => 0,
                                           Latitude  => 0,
                                           Heading   => East);
   Idx : Integer := 0;
   Answer          : Integer;
begin
   Load_File(Computer_Output);

   Move_Loop:
   for N of Computer_Output loop
      Idx := Idx + 1;
      Update_Ferry(N, Ferry_Position);
      Log_Debug(Integer'Image(Idx) & ": ("
                & Integer'Image(Ferry_Position.Longitude) & ','
                & Integer'Image(Ferry_Position.Latitude) & ") facing "
                & Direction'Image(Ferry_Position.Heading));
   end loop Move_Loop;

   Answer := abs Ferry_Position.Longitude + abs Ferry_Position.Latitude;

   Put_line("The Manhattan distance at the end is "
            & Integer'Image(Answer));
end Day12_1;
