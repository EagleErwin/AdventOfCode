with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Containers.Generic_Array_Sort;

procedure Day10_2 is
   -- ### CONSTANTS ### --
   Enable_Debug : constant Boolean := False;
   Number_Of_Inputs : constant Integer := 96;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Input_Numbers is array (Natural range <>) of Integer;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   procedure Log_Debug_Array(Array_To_Log : in Input_Numbers) is
   begin
      if Enable_Debug then
         for I in Array_To_Log'Range loop
            Put(Integer'Image(Array_To_Log(I)));
         end loop;
         Put_Line("");
      end if;
   end Log_Debug_Array;

   procedure Load_File (Adapters : out Input_Numbers) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 1;
      Max_Value    : Integer := 0;
   begin
      Adapters(0) := 0; -- The outlet
      Open (File => Input, Mode => In_File, Name => "data/day10.input");
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Adapters(Idx) := Integer'Value(To_String(Current_Line));
         if Max_Value < Adapters(Idx) then
            Max_Value := Adapters(Idx);
         end if;
         Idx := Idx + 1;
      end loop;
      Close (File => Input);

      Adapters(Number_Of_Inputs + 1) := Max_Value + 3;
   end Load_File;

   procedure Make_The_Chain(Adapters : in out Input_Numbers) is
      procedure Sort is new Ada.Containers.Generic_Array_Sort(Index_Type   => Natural,
                                                              Element_Type => Integer,
                                                              Array_Type   => Input_Numbers);
   begin
      Sort(Adapters);
   end Make_The_Chain;

   function Find_Options_In_Section(Adapters : in Input_Numbers) return Long_Integer is
   begin
      Log_Debug("Found section");
      Log_Debug_Array(Adapters);
      if Adapters'Length = 1 then
         return 1;
      elsif Adapters'Length = 2 then
         return 1;
      elsif Adapters'Length = 3 then
         if Adapters(Adapters'Last) - Adapters(Adapters'First) <=3 then
            return 2;
         else
            return 1;
         end if;
      elsif Adapters'Length = 4 then
         if Adapters(Adapters'Last) - Adapters(Adapters'First) = 3 then
            -- 0, 1, 2, 3: Skip none, skip both middle numbers, or skip one middle number (2 options)
            return 4;
         else
            -- 0, 2, 3, 4: Skip none, or skip one middle number
            return 3;
         end if;
      elsif Adapters'Length = 5 then
         if Adapters(Adapters'Last) - Adapters(Adapters'First) = 4 then
            -- 0, 1, 2, 3, 4: Skip none, skip 2 middle numbers (3 options), or skip one middle number (3 options)
            return 7;
         else
            Put_Line("To be implemented for 5");
            return 5;
         end if;
      else
         Put_Line("Should implement this (section size " & Integer'Image(Adapters'Length) & ") as well");
         return 1;
      end if;
   end Find_Options_In_Section;

   function Find_Number_Of_Options(Adapters : in Input_Numbers) return Long_Integer is
   begin
      Log_Debug("Searching for section");
      Log_Debug_Array(Adapters);
      if Adapters'Length = 0 then
         Log_Debug("End of Adapters reached!");
         return 1;
      elsif Adapters'Length <= 2 then
         return Find_Options_In_Section(Adapters);
      else
         Find_Section_Loop:
         for I in Adapters'Range loop
            if I < Adapters'Last and then Adapters(I+1) - Adapters(I) = 3 then
               return Find_Options_In_Section(Adapters(Adapters'First .. I)) *
                 Find_Number_Of_Options(Adapters(I + 1 .. Adapters'Last));
            end if;
         end loop Find_Section_Loop;
         Put_Line("This should not happen");
      end if;
   end Find_Number_Of_Options;


   Adapter_Chain : Input_Numbers( 0 .. Number_Of_Inputs + 1); -- Outlet is index 0, device is the last element
   Answer : Long_Integer;
begin
   Load_File(Adapter_Chain);
   Make_The_Chain(Adapter_Chain);

   Answer := Find_Number_Of_Options(Adapter_Chain);

   Put_line("The number of distinct ways to arrange the adapters is " & Long_Integer'Image(Answer));
end Day10_2;
