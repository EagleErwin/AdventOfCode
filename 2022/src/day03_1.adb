with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day03_1 is
   Input_Size : constant Integer := 300;

   type Inventory is array (1 .. Input_Size) of Unbounded_String;

   procedure Load_File (Backpacks : out Inventory) is
      Input          : File_Type;
      Current_Line   : Unbounded_String;
      Backpack_Index : Integer := 0;

   begin
      Open (File => Input, Mode => In_File, Name => "data/day03.input");
      while not End_Of_File (Input) loop
         Backpack_Index := Backpack_Index + 1;
         Current_Line := To_Unbounded_String(Get_Line(Input));

         Backpacks(Backpack_Index) := Current_Line;
      end loop;
      Close (File => Input);
   end Load_File;

   function Get_Priority (Item : in Character) return Integer is
      Lowercase_Start  : constant Integer := Character'Pos('a'); -- 97
      Uppercase_Start  : constant Integer := Character'Pos('A'); -- 65
      Lowercase_Offset : constant Integer :=  1 - Lowercase_Start;
      Uppercase_Offset : constant Integer := 27 - Uppercase_Start;

      Ascii_Value : Integer := Character'Pos(Item);
      Priority    : Integer;
   begin
      if (Ascii_Value < Lowercase_Start) then
         -- Uppercase letter
         Priority := Ascii_Value + Uppercase_Offset;
      else
         -- Lowercase letter
         Priority := Ascii_Value + Lowercase_Offset;
      end if;
      return Priority;
   end Get_Priority;


   function Find_Item (Backpack : in Unbounded_String) return Integer is
      Compartment_Size   : Integer := Length(Backpack) / 2;
      Compartment_1_Item : Character;
      Compartment_2_Item : Character;

      Duplicate          : Character;
   begin
      Scan_Backpack_Loop:
      for I in 1 .. Compartment_Size loop
         Compartment_1_Item := Element(Backpack, I);
         Find_In_Backpack_Loop:
         for J in Compartment_Size + 1 .. Length(Backpack) loop
            Compartment_2_Item := Element(Backpack, J);
            if (Compartment_1_Item = Compartment_2_Item) then
               Duplicate := Compartment_1_Item;
               exit Scan_Backpack_Loop;
            end if;
         end loop Find_In_Backpack_Loop;
      end loop Scan_Backpack_Loop;

      return Get_Priority(Duplicate);
   end Find_Item;

   Backpacks : Inventory;
   Answer    : Integer := 0;
begin
   Load_File(Backpacks);

   Analyze_Loop:
   for I in Backpacks'Range loop
      Answer := Answer + Find_Item(Backpacks(I));
   end loop Analyze_Loop;

   Put_Line("The sum of the priorities of the items is "
            & Integer'Image(Answer));
end Day03_1;
