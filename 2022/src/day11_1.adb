with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day11_1 is
   -- ## CONSTANTS ## --
   Number_Of_Monkeys      : constant Integer := 8;
   Total_Number_Of_Items  : constant Integer := 50; -- Estimate, must be large enough

   -- ## TYPES ## --
   type Operation_Operator is (Add, Multiply, Pow);

   type Operation is record
      Op  : Operation_Operator;
      Arg : Integer;
   end record;

   type Item_Array is array (1 .. Total_Number_Of_Items) of Integer;

   type Input_Record is record
      Items               : Item_Array;
      Nr_Of_Items         : Integer;
      Worry_Updater       : Operation;
      Test_Mod            : Integer;
      True_Target         : Integer;
      False_Target        : Integer;
      Removed_Item_Idx    : Item_Array;
      Nr_Of_Removed_Items : Integer;
   end record;

   type Input_Array is array (0 .. Number_Of_Monkeys - 1) of Input_Record;

   type Monkey_Array is array (0 .. Number_Of_Monkeys - 1) of Integer;

   -- ## GLOBAL VARIABLES ## --
   Inspection_Count : Monkey_Array := (others => 0);

   procedure Fill_Item_Data (Input_Line : in Unbounded_String;
                             Monkey     : in out Input_Record) is
      Comma_Pos          : Integer;
      Current_Item_Worry : Integer;
   begin
      -- Assume always at least one item for a monkey, and all worry levels >= 10.
      Current_Item_Worry := Integer'Value(Slice(Input_Line, 19, 20));
      Monkey.Items(1) := Current_Item_Worry;
      Monkey.Nr_Of_Items := 1;

      Comma_Pos := Index(Input_Line, ",");
      Add_Item_Loop:
      while Comma_Pos > 0 loop
         Current_Item_Worry := Integer'Value(Slice(Input_Line, Comma_Pos + 2, Comma_Pos + 3));
         Monkey.Nr_Of_Items := Monkey.Nr_Of_Items + 1;
         Monkey.Items(Monkey.Nr_Of_Items) := Current_Item_Worry;

         Comma_Pos := Index(Input_Line, ",", Comma_Pos + 1);
      end loop Add_Item_Loop;
   end Fill_Item_Data;

   procedure Fill_Operation_Data (Input_Line : in Unbounded_String;
                                  Monkey     : in out Input_Record) is
      Operation_Char : Character := Element(Input_Line, 24);
   begin
      case Operation_Char is
         when '+' => Monkey.Worry_Updater.Op := Add;
         when '*' =>
            if Slice(Input_Line, 26, Length(Input_Line)) = "old" then
               Monkey.Worry_Updater.Op  := Pow;
               Monkey.Worry_Updater.Arg := 2;
            else
               Monkey.Worry_Updater.Op := Multiply;
            end if;
         when others => Put_Line("ERROR: Unsupported operation " & Operation_Char);
      end case;

      if (Monkey.Worry_Updater.Op /= Pow) then
         Monkey.Worry_Updater.Arg := Integer'Value(Slice(Input_Line, 26, Length(Input_Line)));
      end if;
   end Fill_Operation_Data;

   procedure Fill_Test_Data (Input_Line : in Unbounded_String;
                             Monkey     : in out Input_Record) is
   begin
      Monkey.Test_Mod := Integer'Value(Slice(Input_Line, 22, Length(Input_Line)));
   end Fill_Test_Data;

   procedure Load_File (Input_Data : out Input_Array) is
      Input          : File_Type;
      Current_Line   : Unbounded_String;

      Monkey_Index   : Integer;
      Current_Monkey : Input_Record;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day11.input");
      Read_Line_Loop:
      while not End_Of_File (Input) loop
         Current_Monkey := (Items => (others => 0),
                            Nr_Of_Items => 0,
                            Worry_Updater => (Add, 0),
                            Test_Mod => 1,
                            True_Target => -1,
                            False_Target => -1,
                            Removed_Item_Idx => (others => 0),
                            Nr_Of_Removed_Items => 0);
         -- Read the monkey name
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Monkey_Index := Integer'Value("" & Element(Current_Line, 8));
         -- Read the starting items
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Fill_Item_Data(Current_Line, Current_Monkey);
         -- Read the operation
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Fill_Operation_Data(Current_Line, Current_Monkey);
         -- Read the test
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Fill_Test_Data(Current_Line, Current_Monkey);
         -- Read the true-case
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Current_Monkey.True_Target :=
           Integer'Value(Slice(Current_Line, 30, Length(Current_Line)));
         -- Read the false-case
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Current_Monkey.False_Target :=
           Integer'Value(Slice(Current_Line, 31, Length(Current_Line)));
         -- Read the empty line, can be EOF already.
         if not End_Of_File (Input) then
            Current_Line := To_Unbounded_String(Get_Line(Input));
         end if;

         Input_Data(Monkey_Index) := Current_Monkey;
      end loop Read_Line_Loop;
      Close (File => Input);
   end Load_File;

   procedure Print_Inventory (Monkeys : in Input_Array) is
   begin
      Monkey_Loop:
      for I in 0 .. Number_Of_Monkeys - 1 loop
         Put("Monkey" & Integer'Image(I) & ":");
         Item_Loop:
         for J in 1 .. Monkeys(I).Nr_Of_Items loop
            Put(Integer'Image(Monkeys(I).Items(J)) & ",");
         end loop Item_Loop;
         Put_Line("");
      end loop Monkey_Loop;
   end Print_Inventory;

   procedure Reduce_Monkey_Inventory (Monkey : in out Input_Record) is
      Removals_Processed : Integer := 0;
      Removed_Index : Integer;
   begin
      Removed_Items_Loop:
      for I in 1 .. Monkey.Nr_Of_Removed_Items loop
         Removed_Index := Monkey.Removed_Item_Idx(I) - Removals_Processed;
         Shift_Position_Loop:
         for N in Removed_Index .. Monkey.Items'Length - 1 loop
            Monkey.Items(N) := Monkey.Items(N + 1);
         end loop Shift_Position_Loop;
         Monkey.Nr_Of_Items := Monkey.Nr_Of_Items - 1;
         Removals_Processed := Removals_Processed + 1;
      end loop Removed_Items_Loop;
      Monkey.Nr_Of_Removed_Items := 0;
   end Reduce_Monkey_Inventory;

   procedure Inspect (Monkeys : in out Input_Array) is
      Current_Monkey  : Input_Record;
      Current_Item    : Integer;
      Current_Op      : Operation_Operator;
      Current_Arg     : Integer;

      Current_Test_Mod : Integer;
      Current_Destination : Integer;
   begin
      Monkey_Loop:
      for M in Monkeys'Range loop
         Current_Monkey := Monkeys(M);
         Item_Loop:
         for I in 1 .. Current_Monkey.Nr_Of_Items loop
            Inspection_Count(M) := Inspection_Count(M) + 1;
            Current_Item := Current_Monkey.Items(I);
            Current_Op  := Current_Monkey.Worry_Updater.Op;
            Current_Arg := Current_Monkey.Worry_Updater.Arg;

            -- Update worry level
            case Current_Op is
               when Add => Monkeys(M).Items(I) := Current_Item + Current_Arg;
               when Multiply => Monkeys(M).Items(I) := Current_Item * Current_Arg;
               when Pow => Monkeys(M).Items(I) := Current_Item * Current_Item;
            end case;

            -- Decrease worry level
            Monkeys(M).Items(I) := Integer (Float'Floor(Float(Monkeys(M).Items(I)) / Float(3)));

            -- Execute test
            Current_Test_Mod := Current_Monkey.Test_Mod;
            if Monkeys(M).Items(I) mod Current_Test_Mod = 0 then
               Current_Destination := Current_Monkey.True_Target;
            else
               Current_Destination := Current_Monkey.False_Target;
            end if;

            -- Throw item
            Monkeys(M).Nr_Of_Removed_Items := Monkeys(M).Nr_Of_Removed_Items + 1;
            Monkeys(M).Removed_Item_Idx(Monkeys(M).Nr_Of_Removed_Items) := I;
            Monkeys(Current_Destination).Nr_Of_Items := Monkeys(Current_Destination).Nr_Of_Items + 1;
            Monkeys(Current_Destination).Items(Monkeys(Current_Destination).Nr_Of_Items) := Monkeys(M).Items(I);
         end loop Item_Loop;

         Reduce_Monkey_Inventory(Monkeys(M));
      end loop Monkey_Loop;
   end Inspect;

   function Find_Monkey_Business return Integer is
      Max_Value : Integer := 0;
      Snd_Value : Integer := 0;

      Current_Value : Integer;
   begin
      for I in Inspection_Count'Range loop
         Current_Value := Inspection_Count(I);
         if (Current_Value > Max_Value) then
            Snd_Value := Max_Value;
            Max_Value := Current_Value;
         elsif (Current_Value > Snd_Value) then
            Snd_Value := Current_Value;
         end if;
      end loop;

      return Max_Value * Snd_Value;
   end Find_Monkey_Business;

   Input  : Input_Array;
   Answer : Integer := 0;
begin
   Load_File(Input);

   Inspection_Loop:
   for Round in 1 .. 20 loop
      Inspect(Input);
      --Print_Inventory(Input);
   end loop Inspection_Loop;

   Answer := Find_Monkey_Business;

   Put_Line("The level of monkey business after 20 rounds is"
            & Integer'Image(Answer));
end Day11_1;
