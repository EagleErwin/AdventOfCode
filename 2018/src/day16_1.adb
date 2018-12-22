with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
procedure Day16_1 is
   Max_Number_Of_Instructions : constant Integer := 1000; -- Estimation
   type Unsigned_8 is mod 2**8; -- For bitwise operations

   subtype Data_Line is Unbounded_String;
   subtype Register_Pointer is Integer range 0 .. 3;
   type Register_Array is array (Register_Pointer) of Integer;

   subtype Opcode is Integer range 0 .. 15;

   type Instruction is record
      Op_Code : Opcode;
      A,B : Integer;
      C : Register_Pointer;
   end record;

   type Action is record
      Before : Register_Array;
      After : Register_Array;
      Operation : Instruction;
   end record;

   type Action_Access is access all Action;

   type Action_Array is array (1 .. Max_Number_Of_Instructions) of Action_Access;

   -- addr (add register)
   -- stores into register C the result of adding register A and register B.
   procedure Addr (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      Memory(Instr.C) := Memory(Instr.A) + Memory(Instr.B);
   end Addr;

   -- addi (add immediate)
   -- stores into register C the result of adding register A and value B.
   procedure Addi (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      Memory(Instr.C) := Memory(Instr.A) + Instr.B;
   end Addi;

   -- mulr (multiply register)
   -- stores into register C the result of multiplying register A and register B.
   procedure Mulr (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      Memory(Instr.C) := Memory(Instr.A) * Memory(Instr.B);
   end Mulr;

   -- muli (multiply immediate)
   -- stores into register C the result of multiplying register A and value B.
   procedure Muli (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      Memory(Instr.C) := Memory(Instr.A) * Instr.B;
   end Muli;

   -- banr (bitwise AND register)
   -- stores into register C the result of the bitwise AND of register A and register B.
   procedure Banr (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      Memory(Instr.C) := Integer(Unsigned_8(Memory(Instr.A)) and Unsigned_8(Memory(Instr.B)));
   end Banr;

   -- bani (bitwise AND immediate)
   -- stores into register C the result of the bitwise AND of register A and value B.
   procedure Bani (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      Memory(Instr.C) := Integer(Unsigned_8(Memory(Instr.A)) and Unsigned_8(Instr.B));
   end Bani;

   -- borr (bitwise OR register)
   -- stores into register C the result of the bitwise OR of register A and register B.
   procedure Borr (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      Memory(Instr.C) := Integer(Unsigned_8(Memory(Instr.A)) or Unsigned_8(Memory(Instr.B)));
   end Borr;

   -- bori (bitwise OR immediate)
   -- stores into register C the result of the bitwise OR of register A and value B.
   procedure Bori (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      Memory(Instr.C) := Integer(Unsigned_8(Memory(Instr.A)) or Unsigned_8(Instr.B));
   end Bori;

   -- setr (set register)
   -- copies the contents of register A into register C. (Input B is ignored.)
   procedure Setr (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      Memory(Instr.C) := Memory(Instr.A);
   end Setr;

   -- seti (set immediate)
   -- stores value A into register C. (Input B is ignored.)
   procedure Seti (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      Memory(Instr.C) := Instr.A;
   end Seti;

   -- gtir (greater-than immediate/register)
   -- sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
   procedure Gtir (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      if Instr.A > Memory(Instr.B) then
         Memory(Instr.C) := 1;
      else
         Memory(Instr.C) := 0;
      end if;
   end Gtir;

   -- gtri (greater-than register/immediate)
   -- sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
   procedure Gtri (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      if Memory(Instr.A) > Instr.B then
         Memory(Instr.C) := 1;
      else
         Memory(Instr.C) := 0;
      end if;
   end Gtri;

   -- gtrr (greater-than register/register)
   -- sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
   procedure Gtrr (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      if Memory(Instr.A) > Memory(Instr.B) then
         Memory(Instr.C) := 1;
      else
         Memory(Instr.C) := 0;
      end if;
   end Gtrr;

   -- eqir (equal immediate/register)
   -- sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
   procedure Eqir (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      if Instr.A = Memory(Instr.B) then
         Memory(Instr.C) := 1;
      else
         Memory(Instr.C) := 0;
      end if;
   end Eqir;

   -- eqri (equal register/immediate)
   -- sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
   procedure Eqri (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      if Memory(Instr.A) = Instr.B then
         Memory(Instr.C) := 1;
      else
         Memory(Instr.C) := 0;
      end if;
   end Eqri;

   -- eqrr (equal register/register)
   -- sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
   procedure Eqrr (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      if Memory(Instr.A) = Memory(Instr.B) then
         Memory(Instr.C) := 1;
      else
         Memory(Instr.C) := 0;
      end if;
   end Eqrr;

   function String_To_Instruction (Input : in Unbounded_String) return Instruction is
      type State_Integer is mod 4;
      Read_State   : State_Integer := 0;

      Result : Instruction;
      Current_Elem : Character;
      Buffer : Unbounded_String;
   begin
      for I in 1 .. Length(Input) loop
         Current_Elem := Element(Input, I);
         if Current_Elem = ' ' then
            if Read_State = 0 then
               Result.Op_Code := Opcode'Value(To_String(Buffer));
            elsif Read_State = 1 then
               Result.A := Integer'Value(To_String(Buffer));
            elsif Read_State = 2 then
               Result.B := Integer'Value(To_String(Buffer));
            else
               Put_Line("[ERROR] Unexpected space");
            end if;
            Read_State := Read_State + 1;
            Buffer := Null_Unbounded_String;
         else
            Buffer := Buffer & Current_Elem;
         end if;
      end loop;
      Result.C := Integer'Value(To_String(Buffer));

      return Result;
   end String_To_Instruction;

   procedure Load_File (Input_Data : out Action_Array) is
      type State_Integer is mod 4;
      Input        : File_Type;
      Current_Line : Data_Line;
      Continue     : Boolean := True;
      -- For the state machine
      -- 0 means: Expecting 'Before'
      -- 1 means: Expecting numbers
      -- 2 means: Expecting 'After'
      -- 3 means: Expecting empty line
      Read_State   : State_Integer := 0;

      Action_Index         : Integer := 0;
      State_Before         : Register_Array;
      State_After          : Register_Array;
      Executed_Instruction : Instruction;
   begin
      Input_Data := (others => null);
      Open (File => Input, Mode => In_File, Name => "data/day16.input");

      Read_Loop:
      while Continue loop
         Current_Line := To_Unbounded_String(Get_Line(File => Input));
         if Read_State = 0 then
            -- Check if we are at the end of part 1.
            if Length(Current_Line) = 0 then
               Continue := False;
               exit Read_Loop;
            end if;
            Action_Index := Action_Index + 1;
            State_Before := (0 => Integer'Value((1 => Element(Current_Line, 10))),
                             1 => Integer'Value((1 => Element(Current_Line, 13))),
                             2 => Integer'Value((1 => Element(Current_Line, 16))),
                             3 => Integer'Value((1 => Element(Current_Line, 19))));
         elsif Read_State = 1 then
            Executed_Instruction := String_To_Instruction(Current_Line);
         elsif Read_State = 2 then
            if Element(Current_Line, 1) /= 'A' then
               Put_Line("[ERROR] Unexpected line in state 2!");
               Continue := False;
               exit Read_Loop;
            end if;
            State_After := (0 => Integer'Value((1 => Element(Current_Line, 10))),
                            1 => Integer'Value((1 => Element(Current_Line, 13))),
                            2 => Integer'Value((1 => Element(Current_Line, 16))),
                            3 => Integer'Value((1 => Element(Current_Line, 19))));
         elsif Read_State = 3 then
            Input_Data(Action_Index) := new Action'(Before    => State_Before,
                                                    After     => State_After,
                                                    Operation => Executed_Instruction);
         end if;

         Read_State := Read_State + 1;
      end loop Read_Loop;

      Close (File => Input);
   end Load_File;

   procedure Execute_Opcode(Code : in Opcode;
                            Instr : in Instruction;
                            Memory : in out Register_Array) is
   begin
      if Code = 0 then
         Addr(Instr  => Instr,
              Memory => Memory);
      elsif Code = 1 then
         Addi(Instr  => Instr,
              Memory => Memory);
      elsif Code = 2 then
         Mulr(Instr  => Instr,
              Memory => Memory);
      elsif Code = 3 then
         Muli(Instr  => Instr,
              Memory => Memory);
      elsif Code = 4 then
         Banr(Instr  => Instr,
              Memory => Memory);
      elsif Code = 5 then
         Bani(Instr  => Instr,
              Memory => Memory);
      elsif Code = 6 then
         Borr(Instr  => Instr,
              Memory => Memory);
      elsif Code = 7 then
         Bori(Instr  => Instr,
              Memory => Memory);
      elsif Code = 8 then
         Setr(Instr  => Instr,
              Memory => Memory);
      elsif Code = 9 then
         Seti(Instr  => Instr,
              Memory => Memory);
      elsif Code = 10 then
         Gtir(Instr  => Instr,
              Memory => Memory);
      elsif Code = 11 then
         Gtri(Instr  => Instr,
              Memory => Memory);
      elsif Code = 12 then
         Gtrr(Instr  => Instr,
              Memory => Memory);
      elsif Code = 13 then
         Eqir(Instr  => Instr,
              Memory => Memory);
      elsif Code = 14 then
         Eqri(Instr  => Instr,
              Memory => Memory);
      elsif Code = 15 then
         Eqrr(Instr  => Instr,
              Memory => Memory);
      end if;
   end Execute_Opcode;

   -- Checks if the provided action behaves like 3 or more opcodes.
   function Check_Sample(Input_Action : in Action_Access) return Boolean is
      Matching_Opcodes : Integer := 0;
      Match : Boolean;
      Current_Memory : Register_Array;
   begin
      Opcode_Loop:
      for I in Opcode'Range loop
         Match := True;
         Current_Memory := Input_Action.Before;
         Execute_Opcode(Code   => I,
                        Instr  => Input_Action.Operation,
                        Memory => Current_Memory);
         Validation_Loop:
         for J in Register_Pointer'Range loop
            if Current_Memory(J) /= Input_Action.After(J) then
               Match := False;
               exit Validation_Loop;
            end if;
         end loop Validation_Loop;

         if Match then
            Matching_Opcodes := Matching_Opcodes + 1;
         end if;
      end loop Opcode_Loop;

      return Matching_Opcodes >= 3;
   end Check_Sample;

   -- Return how many Actions from the provided data can be from 3 or more possible opcodes
   function Get_Number_Of_Interesting_Samples(Input_Data : in Action_Array) return Integer is
      Result : Integer := 0;
   begin
      Actions_Loop:
      for I in Input_Data'Range loop
         exit Actions_Loop when Input_Data(I) = null;
         if Check_Sample(Input_Action => Input_Data(I)) then
            Result := Result + 1;
         end if;
      end loop Actions_Loop;
      return Result;
   end Get_Number_Of_Interesting_Samples;

   Input_Data  : Action_Array;
   Answer : Integer;
begin
   Load_File (Input_Data => Input_Data);

   Answer := Get_Number_Of_Interesting_Samples(Input_Data => Input_Data);

   Put_Line("Answer:" & Integer'Image(Answer));
end Day16_1;
