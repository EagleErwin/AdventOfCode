with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps; use Ada.Containers;

procedure Day06_1 is
   -- ### CONSTANTS ### --
   Number_Of_Days : constant Integer := 80;

   -- ### TYPE DEFINITIONS ### --
   function Natural_Hash (id : Integer) return Hash_Type is
   begin
      -- I don't know how to calculate a Hash that makes more sense.
      return Hash(Integer'Image(id));
   end Natural_Hash;

   -- The value in the Population map is the number of fishes that has the timer
   -- set to the value of the key.
   package Population is new Hashed_Maps(Key_Type        => Integer,
                                         Element_Type    => Integer,
                                         Hash            => Natural_Hash,
                                         Equivalent_Keys => "=" );
   use Population;

   procedure Load_File (Fishes : out Population.Map) is
      Input        : File_Type;
      Current_Line : Unbounded_String;
      Current_Value : Integer;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day06.input");
      Current_Line := To_Unbounded_String(Get_Line(Input));

      for I in 1 .. Length(Current_Line) loop
         if (I rem 2) /= 0 then
            Current_Value := Integer'Value((1 => Element(Current_Line, I)));
            if not Fishes.Contains(Current_Value) then
               Fishes.Insert(Current_Value, 0);
            end if;
            Fishes(Current_Value) := Fishes(Current_Value) + 1;
         end if;
      end loop;

      -- Make sure that all ages are present in the map
      Fill_Elements_Loop:
      for J in 0 .. 8 loop
         if not Fishes.Contains(J) then
            Fishes.Insert(J, 0);
         end if;
      end loop Fill_Elements_Loop;

      Close (File => Input);
   end Load_File;

   procedure Reproduce (Fishes : in out Population.Map) is
      Current_Value : Integer;
      Previous_Value : Integer := 0;
   begin
      for I in reverse 0 .. 8 loop
         Current_Value := Fishes(I);
         Fishes(I) := Previous_Value;
         Previous_Value := Current_Value;
      end loop;
      -- Previous_Value is number of fishes that were 0. They are 6 now.
      Fishes(6) := Fishes(6) + Previous_Value;
      -- Previous_Value is number of fishes that were 0.
      -- They all gave birth to a new fish of value 8.
      Fishes(8) := Previous_Value;
   end Reproduce;

   Fishes        : Population.Map;
   Answer        : Integer := 0;
begin
   Load_File(Fishes);

   Reproduce_Loop:
   for N in 1 .. Number_Of_Days loop
      Reproduce(Fishes);
   end loop Reproduce_Loop;

   Count_Loop:
   for I in Fishes.Iterate loop
      Answer := Answer + Fishes(Key(I));
   end loop Count_Loop;

   Put_line("A total of " & Integer'Image(Answer) & " lanternfish exist after "
            & Integer'Image(Number_Of_Days) & " days.");
end Day06_1;
