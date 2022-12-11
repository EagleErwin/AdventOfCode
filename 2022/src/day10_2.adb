with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day10_2 is
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

   procedure Draw (Input : in Execution) is
      Drawn_Index  : Integer := (Input.Cycle - 1) mod 40;
      Sprite_Pos_1 : Integer := Input.X - 1;
      Sprite_Pos_2 : Integer := Input.X;
      Sprite_Pos_3 : Integer := Input.X + 1;
   begin
      if (Drawn_Index = Sprite_Pos_1 or else
          Drawn_Index = Sprite_Pos_2 or else
          Drawn_Index = Sprite_Pos_3) then
         Put("#");
      else
         Put(".");
      end if;

      if Drawn_Index = 39 then
         Put_Line("");
      end if;
   end Draw;

   procedure Execute (Input : in out Execution) is
      Current_Instruction : Instruction;
   begin
      Execution_Loop:
      while Input.Cycle < Input.Application'Length loop
         Input.Cycle := Input.Cycle + 1;

         Draw (Input);

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
   end Execute;

   Application : Program;
   Run         : Execution;
begin
   Load_File(Application);

   Run := (Application, 0, 1);

   Execute(Run);
end Day10_2;
