with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Containers.Generic_Array_Sort;

procedure Day10_1 is
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

   procedure Load_File (Adapters : out Input_Numbers) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 1;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day10.input");
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Adapters(Idx) := Integer'Value(To_String(Current_Line));
         Idx := Idx + 1;
      end loop;
      Close (File => Input);
   end Load_File;

   procedure Make_The_Chain(Adapters : in out Input_Numbers;
                            Steps_Of_One : out Integer;
                            Steps_Of_Three : out Integer) is
      procedure Sort is new Ada.Containers.Generic_Array_Sort(Index_Type   => Natural,
                                                              Element_Type => Integer,
                                                              Array_Type   => Input_Numbers);
      This_Yolt : Integer;
      Next_Yolt : Integer;
   begin
      Sort(Adapters);

      Steps_Of_Three := 1; -- The last step from adapter to device is always three

      if Adapters(1) = 1 then
         Steps_Of_One := 1; -- Step from outlet to first adapter is one
      elsif Adapters(1) = 3 then
         Steps_Of_Three := Steps_Of_Three + 1; -- Step from outlet to first adapter is three
      end if;



      Analyze_Loop:
      for Idx in Adapters'Range loop
         Log_Debug(Integer'Image(Adapters(Idx)));
         if Idx /= Number_Of_Inputs then
            This_Yolt := Adapters(Idx);
            Next_Yolt := Adapters(Idx + 1);
            if Next_Yolt - This_Yolt = 1 then
               Steps_Of_One := Steps_Of_One + 1;
            elsif Next_Yolt - This_Yolt = 3 then
               Steps_Of_Three := Steps_Of_Three + 1;
            end if;
         end if;
      end loop Analyze_Loop;
   end Make_The_Chain;

   Adapter_Chain : Input_Numbers( 1 .. Number_Of_Inputs) := (others => 0);
   Diff_One : Integer;
   Diff_Three : Integer;
   Answer : Integer;
begin
   Load_File(Adapter_Chain);

   Make_The_Chain(Adapter_Chain, Diff_One, Diff_Three);

   Answer := Diff_One * Diff_Three;

   Put_line("The product of the number of 1-jolt differences (" & Integer'Image(Diff_One)
            & ") and the number of 3-jolt differences (" & Integer'Image(Diff_Three)
            & ") is " & Integer'Image(Answer));
end Day10_1;
