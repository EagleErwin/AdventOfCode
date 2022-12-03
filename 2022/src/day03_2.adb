with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Day03_2 is
   Input_Size : constant Integer := 300;
   Group_Size : constant Integer := 3;

   type Elf_Group is array (1 .. Group_Size) of Unbounded_String;

   type Inventory is array (1 .. (Input_Size / Group_Size)) of Elf_Group;

   procedure Load_File (Groups : out Inventory) is
      Input          : File_Type;
      Current_Line   : Unbounded_String;
      Current_Group  : Elf_Group;
      Backpack_Index : Integer := 0;
      Group_Index    : Integer := 0;

   begin
      Open (File => Input, Mode => In_File, Name => "data/day03.input");
      while not End_Of_File (Input) loop
         Backpack_Index := Backpack_Index + 1;
         Current_Line   := To_Unbounded_String (Get_Line (Input));

         Current_Group (Backpack_Index) := Current_Line;

         if (Backpack_Index = Group_Size) then
            Group_Index          := Group_Index + 1;
            Groups (Group_Index) := Current_Group;

            Backpack_Index := 0;
         end if;
      end loop;
      Close (File => Input);
   end Load_File;

   function Get_Priority (Item : in Character) return Integer is
      Lowercase_Start  : constant Integer := Character'Pos ('a'); -- 97
      Uppercase_Start  : constant Integer := Character'Pos ('A'); -- 65
      Lowercase_Offset : constant Integer := 1 - Lowercase_Start;
      Uppercase_Offset : constant Integer := 27 - Uppercase_Start;

      Ascii_Value : Integer := Character'Pos (Item);
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

   function Find_Common_Item (Group : in Elf_Group) return Integer is
      Backpack_1      : Unbounded_String := Group (1);
      Backpack_2      : Unbounded_String := Group (2);
      Backpack_3      : Unbounded_String := Group (3);
      Backpack_1_Size : Integer          := Length (Backpack_1);
      Backpack_2_Size : Integer          := Length (Backpack_2);
      Backpack_3_Size : Integer          := Length (Backpack_3);

      Backpack_1_Item : Character;
      Backpack_2_Item : Character;
      Backpack_3_Item : Character;

      Duplicate : Character;
   begin
      Backpack_1_Loop :
      for I in 1 .. Backpack_1_Size loop
         Backpack_1_Item := Element (Backpack_1, I);
         Backpack_2_Loop :
         for J in 1 .. Backpack_2_Size loop
            Backpack_2_Item := Element (Backpack_2, J);
            if (Backpack_1_Item = Backpack_2_Item) then
               Backpack_3_Loop :
               for K in 1 .. Backpack_3_Size loop
                  Backpack_3_Item := Element (Backpack_3, K);
                  if (Backpack_1_Item = Backpack_3_Item) then
                     Duplicate := Backpack_1_Item;
                     exit Backpack_1_Loop;
                  end if;
               end loop Backpack_3_Loop;
            end if;
         end loop Backpack_2_Loop;
      end loop Backpack_1_Loop;

      return Get_Priority (Duplicate);
   end Find_Common_Item;

   Groups : Inventory;
   Answer : Integer := 0;
begin
   Load_File (Groups);

   Analyze_Loop :
   for I in Groups'Range loop
      Answer := Answer + Find_Common_Item (Groups (I));
   end loop Analyze_Loop;

   Put_Line
     ("The sum of the priorities of the items is " & Integer'Image (Answer));
end Day03_2;
