with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
procedure Day19_1 is
   Number_Of_Instructions : constant Integer := 36;

   subtype Data_Line is Unbounded_String;
   subtype Register_Pointer is Integer range 0 .. 5;
   type Unsigned_8 is mod 2**8;
   type Register_Array is array (Register_Pointer) of Integer;

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
      A_Pos : constant Integer := 6;
      B_Pos : constant Integer := A_Pos + 2;
      Instr_String : Instruction_String;
      Result : Instruction;
   begin
      Instr_String := Slice(Source => Input,
                            Low    => 1,
                            High   => 4);
      Result.Name := Instruction_Name'Value(Instr_String);
      Result.A := Register_Pointer'Value((1 => Element(Input, A_Pos)));

      -- B is either one or two characters long.
      if Element(Input, B_Pos + 1) = ' ' then
         Result.B := Register_Pointer'Value((1 => Element(Input, B_Pos)));
         Result.C := Register_Pointer'Value((1 => Element(Input, B_Pos + 2)));
      else
         Result.B := Integer'Value(Slice(Input, B_Pos, B_Pos + 1));
         Result.C := Register_Pointer'Value((1 => Element(Input, B_Pos + 3)));
      end if;

      return Result;
   end String_To_Instruction;

   procedure Load_File (Input_Program : out Program;
                        IP_Register : out Register_Pointer) is
      Input               : File_Type;
      Current_Line        : Data_Line;
      Current_Line_Number : Integer := 0;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day19.input");

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
      IP : Integer := 0;
   begin
      while Memory(IP_Register) < Application'Length loop
         IP := Memory(IP_Register);
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
      end loop;
   end Run;

   File        : Program;
   Registers   : Register_Array := (others => 0);
   IP_Register : Register_Pointer;
begin
   Load_File (Input_Program => File, IP_Register => IP_Register);

   Run(Application => File, Memory => Registers, IP_Register => IP_Register);

   Put_Line("Value in register 0: " & Integer'Image(Registers(0)));
   -- Too high: 474619769
end Day19_1;
