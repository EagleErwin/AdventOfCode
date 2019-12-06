with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;

procedure Day05_1 is
   -- ### CONSTANTS ### --
   Memory_Size : constant Integer := 999; -- Index of last memory location (sufficient capacity)
   Required_Value : constant Integer := 19_690_720;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Application is array (0 .. Memory_Size) of Integer;

   type Operation is (Add, Mul, Input, Output, Finished);
   type Parameter_Mode is (Position, Immediate);

   type Instruction is record
      Op     : Operation;
      Mode_1 : Parameter_Mode;
      Mode_2 : Parameter_Mode;
      Mode_3 : Parameter_Mode;
   end record;

   -- ### GLOBAL VARIABLES ### --
   Diagnostic_Code : Integer;

   procedure Load_File (Intcode : out Application) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 0;
      Curr_Char    : Character;
      Buffer       : Unbounded_String;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day05.input");
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

   procedure Init (Intcode : in out Application;
                   Noun : in Integer;
                   Verb : in Integer) is
   begin
      Intcode(1) := Noun;
      Intcode(2) := Verb;
   end Init;

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
      Input_Value : Integer := 1;
   begin
      Intcode(Target_Pos) := Input_Value;
      --Put_Line("Adding " & Integer'Image(Input_Value) & " to " & Integer'Image(Target_Pos));
      IP := IP + 2;
   end Input;

   procedure Output (Position : in Integer;
                     Intcode  : in out Application;
                     IP       : in out Integer) is
   begin
      Diagnostic_Code := Intcode(Position);
      Put_Line("Output: " & Integer'Image(Intcode(Position)));
      IP := IP + 2;
   end Output;

   procedure Run (Intcode : in out Application) is
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
            when Finished => exit Program_Loop;
            when others => exit Program_Loop;
         end case;
      end loop Program_Loop;
      if Current_Instruction.Op /= Finished then
         Put_Line("ABORTED: Invalid instruction found at location " & Integer'Image(Instruction_Pointer) & ": " & Operation'Image(Current_Instruction.Op));
      end if;
   end Run;

   function Verify (Intcode : in Application) return Boolean is
   begin
      return Intcode(0) = Required_Value;
   end Verify;

   Intcode_App : Application;
   Intcode_Try : Application;
begin
   Load_File (Intcode => Intcode_App);
   Intcode_Try := Reset (Intcode => Intcode_App);
   Run (Intcode => Intcode_Try);

   Put_Line(Integer'Image(Diagnostic_Code));
end Day05_1;
