with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Containers.Generic_Array_Sort;

procedure Day12_2 is
   -- ### CONSTANTS ### --
   Enable_Debug     : constant Boolean := False;
   Number_Of_Inputs : constant Integer := 781;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Direction is (North, East, South, West); -- Direction matters!
   type Position is record
      Longitude : Long_Integer; --X
      Latitude  : Long_Integer; --Y
      Heading   : Direction;
   end record;

   type Waypoint_Position is record
      Delta_Lon : Long_Integer;
      Delta_Lat : Long_Integer;
   end record;

   type Action is (North, South, East, West, Left, Right, Forward);
   type Navigation_Instruction is record
      Nav_Action : Action;
      Value      : Long_Integer;
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
      Action_Value : Long_Integer;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day12.input");
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Action_Char := Element(Current_Line, 1);
         Action_Value := Long_Integer'Value(Slice(Source => Current_Line,
                                                  Low    => 2,
                                                  High   => Length(Current_Line)));
         Result(Idx) := Navigation_Instruction'(Nav_Action => Char_To_Action(Action_Char),
                                                Value      => Action_Value);
         Idx := Idx + 1;
      end loop;
      Close (File => Input);
   end Load_File;

   procedure Rotate (Waypoint : in out Waypoint_Position;
                     Instr    : in     Navigation_Instruction) is
      Steps        : Long_Integer := Instr.Value / 90;
      Old_Waypoint : Waypoint_Position;
   begin
      Rotate_Loop:
      for N in 1 .. Steps loop
         Old_Waypoint := Waypoint;
         case Instr.Nav_Action is
            when Left   =>
               Waypoint.Delta_Lon := -Old_Waypoint.Delta_Lat;
               Waypoint.Delta_Lat := Old_Waypoint.Delta_Lon;
            when Right  =>
               Waypoint.Delta_Lon := Old_Waypoint.Delta_Lat;
               Waypoint.Delta_Lat := -Old_Waypoint.Delta_Lon;
            when others => Put_Line("ERROR: Invalid action to rotate "
                                    & Action'Image(Instr.Nav_Action));
         end case;
      end loop Rotate_Loop;
   end Rotate;

   procedure Move (Ferry    : in out Position;
                   Waypoint : in Waypoint_Position;
                   Amount   : in     Long_Integer) is
   begin
      for N in 1 .. Amount loop
         Ferry.Latitude  := Ferry.Latitude + Waypoint.Delta_Lat;
         Ferry.Longitude := Ferry.Longitude + Waypoint.Delta_Lon;
      end loop;
   end Move;

   procedure Update_Situation (Instruction : in     Navigation_Instruction;
                               Ferry       : in out Position;
                               Waypoint    : in out Waypoint_Position) is
   begin
      case Instruction.Nav_Action is
         when North   => Waypoint.Delta_Lat := Waypoint.Delta_Lat + Instruction.Value;
         when South   => Waypoint.Delta_Lat := Waypoint.Delta_Lat - Instruction.Value;
         when East    => Waypoint.Delta_Lon := Waypoint.Delta_Lon + Instruction.Value;
         when West    => Waypoint.Delta_Lon := Waypoint.Delta_Lon - Instruction.Value;
         when Left    => Rotate(Waypoint, Instruction);
         when Right   => Rotate(Waypoint, Instruction);
         when Forward => Move(Ferry, Waypoint, Instruction.Value);
      end case;
   end Update_Situation;

   Computer_Output : Navigation_Sequence;
   Ferry_Position  : Position := Position'(Longitude => 0,
                                           Latitude  => 0,
                                           Heading   => East);
   Waypoint : Waypoint_Position := Waypoint_Position'(Delta_Lon => 10,
                                                      Delta_Lat => 1);
   Idx    : Integer := 0;
   Answer : Long_Integer;
begin
   Load_File(Computer_Output);

   Move_Loop:
   for N of Computer_Output loop
      Idx := Idx + 1;
      Update_Situation(N, Ferry_Position, Waypoint);
      Log_Debug(Integer'Image(Idx) & ": ("
                & Long_Integer'Image(Ferry_Position.Longitude) & ','
                & Long_Integer'Image(Ferry_Position.Latitude) & ") facing "
                & Direction'Image(Ferry_Position.Heading));
      Log_Debug("    (" & Long_Integer'Image(Waypoint.Delta_Lon) & ','
                & Long_Integer'Image(Waypoint.Delta_Lat)
                & ") is the relative position of the waypoint.");
   end loop Move_Loop;

   Answer := abs Ferry_Position.Longitude + abs Ferry_Position.Latitude;

   Put_line("The Manhattan distance at the end is "
            & Long_Integer'Image(Answer));
end Day12_2;
