with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
procedure Day21_1 is
   Number_Of_Instructions : constant Integer := 36;

   subtype Data_Line is Unbounded_String;
   subtype Register_Pointer is Integer range 0 .. 5;
   type Unsigned_32 is mod 2**64;
   type Register_Array is array (Register_Pointer) of Long_Integer;

   subtype Instruction_String is String (1 .. 4);
   type Instruction_Name is (addr, addi, mulr, muli, banr, bani, borr, bori,
                             setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr);
   type Instruction is record
      Name : Instruction_Name;
      A,B : Integer;
      C : Register_Pointer;
   end record;

   type Program is array (0 .. Number_Of_Instructions - 1) of Instruction;

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
      Memory(Instr.C) := Memory(Instr.A) + Long_Integer(Instr.B);
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
      Memory(Instr.C) := Memory(Instr.A) * Long_Integer(Instr.B);
   end Muli;

   -- banr (bitwise AND register)
   -- stores into register C the result of the bitwise AND of register A and register B.
   procedure Banr (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      Memory(Instr.C) := Long_Integer(Unsigned_32(Memory(Instr.A)) and Unsigned_32(Memory(Instr.B)));
   end Banr;

   -- bani (bitwise AND immediate)
   -- stores into register C the result of the bitwise AND of register A and value B.
   procedure Bani (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      Memory(Instr.C) := Long_Integer(Unsigned_32(Memory(Instr.A)) and Unsigned_32(Instr.B));
   end Bani;

   -- borr (bitwise OR register)
   -- stores into register C the result of the bitwise OR of register A and register B.
   procedure Borr (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      Memory(Instr.C) := Long_Integer(Unsigned_32(Memory(Instr.A)) or Unsigned_32(Memory(Instr.B)));
   end Borr;

   -- bori (bitwise OR immediate)
   -- stores into register C the result of the bitwise OR of register A and value B.
   procedure Bori (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      Memory(Instr.C) := Long_Integer(Unsigned_32(Memory(Instr.A)) or Unsigned_32(Instr.B));
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
      Memory(Instr.C) := Long_Integer(Instr.A);
   end Seti;

   -- gtir (greater-than immediate/register)
   -- sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
   procedure Gtir (Instr : in Instruction;
                   Memory : in out Register_Array) is
   begin
      if Long_Integer(Instr.A) > Memory(Instr.B) then
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
      if Memory(Instr.A) > Long_Integer(Instr.B) then
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
      if Long_Integer(Instr.A) = Memory(Instr.B) then
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
      if Memory(Instr.A) = Long_Integer(Instr.B) then
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
      type State_Integer is mod 3;
      Read_State   : State_Integer := 0;

      Instr_String : Instruction_String;
      Result : Instruction;
      Current_Elem : Character;
      Buffer : Unbounded_String;
   begin
      Instr_String := Slice(Source => Input,
                            Low    => 1,
                            High   => 4);
      Result.Name := Instruction_Name'Value(Instr_String);

      for I in 6 .. Length(Input) loop
         Current_Elem := Element(Input, I);
         if Current_Elem = ' ' then
            if Read_State = 0 then
               Result.A := Integer'Value(To_String(Buffer));
            elsif Read_State = 1 then
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

   procedure Load_File (Input_Program : out Program;
                        IP_Register : out Register_Pointer) is
      Input               : File_Type;
      Current_Line        : Data_Line;
      Current_Line_Number : Integer := 0;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day21.input");

      Current_Line := To_Unbounded_String(Get_Line(File => Input));
      IP_Register := Register_Pointer'Value((1 => Element(Current_Line, 5)));
      Row_Loop:
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(File => Input));
         Input_Program(Current_Line_Number) := String_To_Instruction(Input => Current_Line);
         Current_Line_Number := Current_Line_Number + 1;
      end loop Row_Loop;

      Close (File => Input);
   end Load_File;

   procedure Run(Application : in Program;
                 Memory : in out Register_Array;
                IP_Register : in Register_Pointer) is
      Tick_Limit : constant Integer := 100_000_000; -- I hope the minimal required number of ticks will be less
      IP : Integer := 0;
      Tick : Integer := 0;
   begin
      while Memory(IP_Register) < Application'Length loop
         if Tick > Tick_Limit then
            return;
         end if;

         IP := Integer(Memory(IP_Register));
--           Put_Line("Executing instruction "
--                    & Instruction_Name'Image(Application(IP).Name)
--                    & Integer'Image(Application(IP).A)
--                    & " " & Integer'Image(Application(IP).B)
--                    & " " & Integer'Image(Application(IP).C)
--                    & " (" & Long_Integer'Image(Memory(0))
--                    & "," & Long_Integer'Image(Memory(1))
--                    & "," & Long_Integer'Image(Memory(2))
--                    & "," & Long_Integer'Image(Memory(3))
--                    & "," & Long_Integer'Image(Memory(4))
--                    & ",[" & Integer'Image(IP) & "])");
         case Application(IP).Name is
            when addr => Addr(Instr  => Application(IP),
                              Memory => Memory);
            when addi => Addi(Instr  => Application(IP),
                              Memory => Memory);
            when mulr => Mulr(Instr  => Application(IP),
                              Memory => Memory);
            when muli => Muli(Instr  => Application(IP),
                              Memory => Memory);
            when banr => Banr(Instr  => Application(IP),
                              Memory => Memory);
            when bani => Bani(Instr  => Application(IP),
                              Memory => Memory);
            when borr => Borr(Instr  => Application(IP),
                              Memory => Memory);
            when bori => Bori(Instr  => Application(IP),
                              Memory => Memory);
            when setr => Setr(Instr  => Application(IP),
                              Memory => Memory);
            when seti => Seti(Instr  => Application(IP),
                              Memory => Memory);
            when gtir => Gtir(Instr  => Application(IP),
                              Memory => Memory);
            when gtri => Gtri(Instr  => Application(IP),
                              Memory => Memory);
            when gtrr => Gtrr(Instr  => Application(IP),
                              Memory => Memory);
            when eqir => Eqir(Instr  => Application(IP),
                              Memory => Memory);
            when eqri => Eqri(Instr  => Application(IP),
                              Memory => Memory);
            when eqrr => Eqrr(Instr  => Application(IP),
                              Memory => Memory);
         end case;
         Memory(IP_Register) := Memory(IP_Register) + 1;
         Tick := Tick + 1;
      end loop;
   end Run;

   File        : Program;
   Registers   : Register_Array := (0 => 0, others => 0);
   Reg_0_Start : Integer := 0;
   IP_Register : Register_Pointer;
   Executed_Instructions : Integer := 100_000;
   Answer : Integer := -1;
begin
   Load_File (Input_Program => File, IP_Register => IP_Register);

   Run(Application => File, Memory => Registers, IP_Register => IP_Register);

   Put_Line("Answer: " & Integer'Image(Answer) & " after executing " & Integer'Image(Executed_Instructions) & " instructions");
end Day21_1;
