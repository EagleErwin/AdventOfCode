with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day05_2 is
   Nr_Of_Instructions : constant Integer := 501; -- Yes, really.
   Max_Nr_Of_Crates   : constant Integer :=  80;
   Nr_Of_Piles        : constant Integer :=   9;

   type Instruction is record
      Amount : Integer;
      From   : Integer;
      To     : Integer;
   end record;

   type Instruction_Array is array (1 .. Nr_Of_Instructions) of Instruction;

   type Stack is array (1 .. Max_Nr_Of_Crates) of Character;

   type Pile is record
      Crates       : Stack;
      Nr_Of_Crates : Integer;
   end record;

   type Ship is array (1 .. Nr_Of_Piles) of Pile;

   type Parser_Buffer is array (1 .. Max_Nr_Of_Crates) of Unbounded_String;

   function Get_Inventory(Input_Data      : in Parser_Buffer;
                          Number_Of_Lines : in Integer) return Ship is
      Current_Line : Unbounded_String;
      Current_Char : Character;
      Result : Ship := (others => ((others => '.'), 0));
   begin
      Parse_Loop:
      for I in reverse 1 .. Number_Of_Lines loop
         Current_Line := Input_Data(I);
         Pile_Loop:
         for N in 1 .. Nr_Of_Piles loop
            Current_Char := Element(Current_Line, (N * 4) - 2);
            if (Current_Char /= ' ') then
               Result(N).Nr_Of_Crates := Result(N).Nr_Of_Crates + 1;
               Result(N).Crates(Result(N).Nr_Of_Crates) := Current_Char;
            end if;
         end loop Pile_Loop;
      end loop Parse_Loop;

      return Result;
   end Get_Inventory;

   procedure Load_File (Inventory               : out Ship;
                        Rearrangement_Procedure : out Instruction_Array) is
      Input             : File_Type;
      Current_Line      : Unbounded_String;
      Parse_Instruction : Boolean := False;

      Buffer_Index      : Integer := 0;
      Buffer            : Parser_Buffer := (others => To_Unbounded_String(""));

      Instruction_Index  : Integer := 0;
      Instruction_Amount : Integer;
      Instruction_From   : Integer;
      Instruction_To     : Integer;
   begin
      Rearrangement_Procedure := (others => (0, 0, 0));
      Open (File => Input, Mode => In_File, Name => "data/day05.input");
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         if (not Parse_Instruction) then
            -- Stacks
            if (Element(Current_Line, 2) = '1') then
               -- End of piles
               Inventory := Get_Inventory(Buffer, Buffer_Index);
               -- The empty line can be ignored.
               Current_Line := To_Unbounded_String(Get_Line(Input));
               Parse_Instruction := True;
            else
               Buffer_Index := Buffer_Index + 1;
               Buffer(Buffer_Index) := Current_Line;
            end if;
         else
            -- Rearrangement procedure
            if (Element(Current_Line, 7) = ' ') then
               Instruction_Amount   := Integer'Value("" & Element(Current_Line, 6));
               Instruction_From := Integer'Value("" & Element(Current_Line, 13));
               Instruction_To     := Integer'Value("" & Element(Current_Line, 18));
            else
               Instruction_Amount   := Integer'Value(Element(Current_Line, 6) & Element(Current_Line, 7));
               Instruction_From := Integer'Value("" & Element(Current_Line, 14));
               Instruction_To     := Integer'Value("" & Element(Current_Line, 19));
            end if;

            Instruction_Index := Instruction_Index + 1;
            Rearrangement_Procedure(Instruction_Index) := (Amount => Instruction_Amount,
                                                           From   => Instruction_From,
                                                           To     => Instruction_To);
         end if;
      end loop;
      Close (File => Input);
   end Load_File;

   procedure Rearrange(Playfield : in out Ship;
                       Instructions: in Instruction_Array) is
      Current_Instruction : Instruction;
      Current_Crate       : Character;
      From_Pile_Index     : Integer;
      To_Pile_Index       : Integer;
   begin
      Instruction_Loop:
      for I in Instructions'Range loop
         Current_Instruction := Instructions(I);

         From_Pile_Index := Playfield(Current_Instruction.From).Nr_Of_Crates;
         To_Pile_Index   := Playfield(Current_Instruction.To).Nr_Of_Crates;
         Current_Crate := Playfield(Current_Instruction.From).Crates(From_Pile_Index);

         Playfield(Current_Instruction.From).Nr_Of_Crates := From_Pile_Index - Current_Instruction.Amount;
         Playfield(Current_Instruction.To).Nr_Of_Crates := To_Pile_Index + Current_Instruction.Amount;

         Move_Loop:
         for J in 1 .. Current_Instruction.Amount loop
            Playfield(Current_Instruction.To).Crates(To_Pile_Index + J) :=
              Playfield(Current_Instruction.From).Crates(From_Pile_Index - (Current_Instruction.Amount - J));
         end loop Move_Loop;
      end loop Instruction_Loop;
   end Rearrange;

   Piles                   : Ship;
   Rearrangement_Procedure : Instruction_Array;

   Answer : String := ".........";
begin
   Load_File(Piles, Rearrangement_Procedure);

   Rearrange(Piles, Rearrangement_Procedure);

   Top_Crate_Loop:
   for I in Piles'Range loop
      Answer(I) := Piles(I).Crates(Piles(I).Nr_Of_Crates);
   end loop Top_Crate_Loop;

   Put_Line("The message from the top containers is " & Answer);
end Day05_2;
