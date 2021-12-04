with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day03_2 is
   -- ### CONSTANTS ### --
   Input_Length : constant Integer := 12;
   Nr_Of_Inputs : constant Integer := 1_000;

   -- ### TYPE DEFINITIONS ### --
   type Bitmask is mod (2**Input_Length);
   type Admin_Record is record
      Value   : Bitmask;
      Removed : Boolean;
   end record;

   type Administration is array (1 .. Nr_Of_Inputs) of Admin_Record;

   procedure Process_Line(Input  : in     Unbounded_String;
                          Idx    : in     Integer;
                          Output : in out Administration) is
      Current_Value : Bitmask := 0;
      Current_Record : Admin_Record;
   begin
      Current_Value := Bitmask'Value("2#" & To_String(Input) & "#");
      Current_Record.Value := Current_Value;
      Current_Record.Removed := False;
      Output(Idx) := Current_Record;
   end Process_Line;

   function Calculate_O2(Input : in Administration) return Bitmask is
      Working_Admin : Administration := Input;
      Mask : Bitmask := 2 ** (Input_Length - 1);
      Sum : Integer;
      Result : Bitmask := 0;
      Found : Boolean := False;
   begin
      Mask_Loop:
      for N in 1 .. Input_Length loop
         Sum := 0;
         Input_Loop:
         for I in Working_Admin'Range loop
            if (not Working_Admin(I).Removed) then
               if ((Mask and Working_Admin(I).Value) > 0) then
                  Sum := Sum + 1;
               elsif ((Mask and Working_Admin(I).Value) = 0) then
                  Sum := Sum - 1;
               end if;
            end if;
         end loop Input_Loop;

         if (Sum < 0) then -- 0 is most common
            Remove_Ones:
            for M in Working_Admin'Range loop
               if (Working_Admin(M).Removed = False) then
                  if ((Mask and Working_Admin(M).Value) > 1) then
                     Working_Admin(M).Removed := True;
                  end if;
               end if;
            end loop Remove_Ones;
         elsif (Sum > 0 or else Sum = 0) then -- 1 is most common or equal amount
            Remove_Zeroes:
            for M in Working_Admin'Range loop
               if (not Working_Admin(M).Removed) then
                  if ((Mask and Working_Admin(M).Value) = 0) then
                     Working_Admin(M).Removed := True;
                  end if;
               end if;
            end loop Remove_Zeroes;
         end if;

         Mask := Mask / 2;
      end loop Mask_Loop;

      Find_Result:
      for I in Working_Admin'Range loop
         if (Working_Admin(I).Removed = False) then
            if (Found = False) then
               Result := Working_Admin(I).Value;
               Found := True;
            else
               Put_Line("Error! More than one value found for O2!");
            end if;
         end if;
      end loop Find_Result;

      if (Found = False) then
         Put_Line("Error! No valid O2 value found");
      end if;
      return Result;
   end Calculate_O2;

   function Calculate_CO2(Input : in Administration) return Bitmask is
      Working_Admin : Administration := Input;
      Mask : Bitmask := 2 ** (Input_Length - 1);
      Sum : Integer;
      Nr_Of_Valid_Values : Integer;
      Result : Bitmask := 0;
      Found : Boolean := False;
   begin
      Mask_Loop:
      for N in 1 .. Input_Length loop
         Nr_Of_Valid_Values := 0;

         Determine_Valid_Values:
         for J in Working_Admin'Range loop
            if (Working_Admin(J).Removed = False) then
               Nr_Of_Valid_Values := Nr_Of_Valid_Values + 1;
            end if;
         end loop Determine_Valid_Values;
         exit Mask_Loop when Nr_Of_Valid_Values = 1;

         Sum := 0;
         Input_Loop:
         for I in Working_Admin'Range loop
            if (not Working_Admin(I).Removed) then
               if ((Mask and Working_Admin(I).Value) > 0) then
                  Sum := Sum + 1;
               elsif ((Mask and Working_Admin(I).Value) = 0) then
                  Sum := Sum - 1;
               end if;
            end if;
         end loop Input_Loop;

         if (Sum < 0) then -- 1 is least common
            Remove_Zeroes:
            for M in Working_Admin'Range loop
               if (Working_Admin(M).Removed = False) then
                  if ((Mask and Working_Admin(M).Value) = 0) then
                     Working_Admin(M).Removed := True;
                  end if;
               end if;
            end loop Remove_Zeroes;
         elsif (Sum > 0 or else Sum = 0) then -- 0 is least common or equal amount
            Remove_Ones:
            for M in Working_Admin'Range loop
               if (not Working_Admin(M).Removed) then
                  if ((Mask and Working_Admin(M).Value) > 0) then
                     Working_Admin(M).Removed := True;
                  end if;
               end if;
            end loop Remove_Ones;
         end if;

         Mask := Mask / 2;
      end loop Mask_Loop;

      Find_Result:
      for I in Working_Admin'Range loop
         if (Working_Admin(I).Removed = False) then
            if (Found = False) then
               Result := Working_Admin(I).Value;
               Found := True;
            else
               Put_Line("Error! More than one value found for CO2!");
            end if;
         end if;
      end loop Find_Result;

      if (Found = False) then
         Put_Line("Error! No valid CO2 value found");
      end if;
      return Result;
   end Calculate_CO2;

   Input          : File_Type;
   Idx            : Integer := 1;
   Current_Line   : Unbounded_String;
   Values         : Administration;

   Oxygen_Generator_Rating : Bitmask := 0;
   CO2_Scrubber_Rating     : Bitmask := 0;
   Answer                  : Integer;
begin
   Open (File => Input, Mode => In_File, Name => "data/day03.input");
   while not End_Of_File (Input) loop
      Current_Line := To_Unbounded_String(Get_Line(Input));
      Process_Line(Current_Line, Idx, Values);
      Idx := Idx + 1;
   end loop;
   Close (File => Input);

   Oxygen_Generator_Rating := Calculate_O2(Values);
   CO2_Scrubber_Rating     := Calculate_CO2(Values);

   Answer := Integer(Oxygen_Generator_Rating) * Integer(CO2_Scrubber_Rating);

   Put_line("The answer is " & Integer'Image(Answer)
            & ", calculated by multiplying Oxygen Generator Rating "
            & Bitmask'Image(Oxygen_Generator_Rating)
            & " and CO2 Scrubber Rating "
            & Bitmask'Image(CO2_Scrubber_Rating) & ".");
end Day03_2;
