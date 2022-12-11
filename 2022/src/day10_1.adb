with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day10_1 is
   -- ## CONSTANTS ## --
   Input_Length           : constant Integer := 141;
   Number_Of_Instructions : constant Integer := 2 * Input_Length;

   -- ## TYPES ## --
   type Operation is (AddX, Noop, Wait);

   type Instruction is record
      Op  : Operation;
      Arg : Integer;
   end record;

   type Program is array (1 .. Number_Of_Instructions) of Instruction;

   type Execution is record
      Application     : Program;
      Cycle           : Integer;
      X               : Integer;
   end record;

   function To_Instruction (Input : in Unbounded_String) return Instruction is
      Current_Op  : Operation;
      Current_Arg : Integer := 0;
   begin
      Current_Op := Operation'Value(Slice(Input, 1, 4));

      if Current_Op /= Noop then
         Current_Arg := Integer'Value(Slice(Input, 6, Length(Input)));
      end if;

      return (Current_Op, Current_Arg);
   end To_Instruction;

   procedure Load_File (Instructions : out Program) is
      Input               : File_Type;
      Current_Line        : Unbounded_String;
      Current_Instruction : Instruction;

      Instruction_Index : Integer := 0;
   begin
      Instructions := (others => (Noop, 0));
      Open (File => Input, Mode => In_File, Name => "data/day10.input");
      Read_Line_Loop:
      while not End_Of_File (Input) loop
         Instruction_Index := Instruction_Index + 1;
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Current_Instruction := To_Instruction(Current_Line);

         if (Current_Instruction.Op = AddX) then
            -- Insert a wait in front of the AddX operation.
            Instructions(Instruction_Index).Op  := Wait;
            Instructions(Instruction_Index).Arg := 0;
            Instruction_Index := Instruction_Index + 1;
         end if;

         Instructions(Instruction_Index) := Current_Instruction;
      end loop Read_Line_Loop;
      Close (File => Input);
   end Load_File;

   procedure AddX (Input : in out Execution;
                   Args  : in Integer) is
   begin
      Input.X := Input.X + Args;
   end AddX;

   function Execute (Input : in out Execution) return Integer is
      Strength : Integer := 0;
      Current_Instruction : Instruction;
   begin
      Execution_Loop:
      while Input.Cycle < Input.Application'Length loop
         Input.Cycle := Input.Cycle + 1;

         if (Input.Cycle = 20 or else Input.Cycle = 60 or else
             Input.Cycle = 100 or else Input.Cycle = 140 or else
             Input.Cycle = 180 or else Input.Cycle = 220) then
            --  Put_Line("Strength during cycle" & Integer'Image(Input.Cycle)
            --           & " is " & Integer'Image(Input.Cycle) & "*"
            --           & Integer'Image(Input.X) & "="
            --           & Integer'Image(Input.Cycle * Input.X));
            Strength := Strength + (Input.Cycle * Input.X);
         end if;
         --  Put_Line("Cycle: " & Integer'Image(Input.Cycle));
         Current_Instruction := Input.Application(Input.Cycle);
         --  Put_Line(Operation'Image(Current_Instruction.Op) & " " & Integer'Image(Current_Instruction.Arg));
         case Current_Instruction.Op is
            when Wait => Put("");
            when Noop => Put("");
            when AddX => AddX(Input, Current_Instruction.Arg);
         end case;
         --  Put_Line("X: " & Integer'Image(Input.X));
      end loop Execution_Loop;
      return Strength;
   end Execute;

   Application : Program;
   Run         : Execution;
   Answer      : Integer := 0;
begin
   Load_File(Application);

   --  Print_Loop:
   --  for I in Application'Range loop
   --     Put_Line(Operation'Image(Application(I).Op) & " " & Integer'Image(Application(I).Arg));
   --  end loop Print_Loop;

   Run := (Application, 0, 1);

   Answer := Execute(Run);

   Put_Line("The sum of signal strengs during the given cycles is"
            & Integer'Image(Answer));
end Day10_1;
