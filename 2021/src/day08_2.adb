with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day08_2 is
   -- ### CONSTANTS ### --

   -- ### TYPE DEFINITIONS ### --
   type Seven_Segment_Digit is new Integer range 0 .. 9;
   type Options is array (0 .. 9) of Boolean;

   type Wires is record
      A     : Boolean;
      B     : Boolean;
      C     : Boolean;
      D     : Boolean;
      E     : Boolean;
      F     : Boolean;
      G     : Boolean;
      Possible_Values : Options;
      Solution_Found  : Boolean;
      Solution        : Seven_Segment_Digit;
   end record;

   type Input_Samples is array (1 .. 10) of Wires;
   type Output_Values is array (1 .. 4) of Wires;

   type Input_Entry is record
      Samples : Input_Samples;
      Output  : Output_Values;
   end record;

   package Input_Data is new Vectors(Index_Type   => Natural,
                                     Element_Type => Input_Entry);
   use Input_Data;

   function Get_Input_Entry(Text_Line : in Unbounded_String) return Input_Entry is
      Result  : Input_Entry;

      Empty_Value : Wires := (False, False, False, False, False, False, False, (others => True), False, 0);
      Samples : Input_Samples := (others => Empty_Value);
      Output  : Output_Values := (others => Empty_Value);

      Pipe_Index   : Integer := 0;
      Sample_Index : Integer := 1;
      Current_Char : Character;
      Buffer       : Unbounded_String := To_Unbounded_String("");
   begin
      Sample_Parser_Loop:
      for E in 1 .. Length(Text_Line) loop
         Current_Char := Element(Text_Line, E);
         case Current_Char is
            when '|' => Pipe_Index := E; exit Sample_Parser_Loop;
            when ' ' => Sample_Index := Sample_Index + 1;
            when 'a' => Samples(Sample_Index).A := True;
            when 'b' => Samples(Sample_Index).B := True;
            when 'c' => Samples(Sample_Index).C := True;
            when 'd' => Samples(Sample_Index).D := True;
            when 'e' => Samples(Sample_Index).E := True;
            when 'f' => Samples(Sample_Index).F := True;
            when 'g' => Samples(Sample_Index).G := True;
            when others => Put_Line("Error! Invalid character '" & Current_Char & "'found!");
         end case;
      end loop Sample_Parser_Loop;

      Output_Parser_Loop:
      for F in Pipe_Index .. Length(Text_Line) loop
         Current_Char := Element(Text_Line, F);
         case Current_Char is
            when '|' => Sample_Index := 0;
            when ' ' => Sample_Index := Sample_Index + 1;
            when 'a' => Output(Sample_Index).A := True;
            when 'b' => Output(Sample_Index).B := True;
            when 'c' => Output(Sample_Index).C := True;
            when 'd' => Output(Sample_Index).D := True;
            when 'e' => Output(Sample_Index).E := True;
            when 'f' => Output(Sample_Index).F := True;
            when 'g' => Output(Sample_Index).G := True;
            when others => Put_Line("Error! Invalid character '" & Current_Char & "'found!");
         end case;
      end loop Output_Parser_Loop;

      Result.Samples := Samples;
      Result.Output := Output;
      return Result;
   end Get_Input_Entry;

   procedure Load_File (Entries : out Input_Data.Vector) is
      Input         : File_Type;
      Current_Line  : Unbounded_String;
      Current_Entry : Input_Entry;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day08.input");
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Current_Entry := Get_Input_Entry(Current_Line);
         Entries.Append(Current_Entry);
      end loop;
      Close (File => Input);
   end Load_File;

   function Count_Segments(Sample : in Wires) return Integer is
      Result : Integer := 0;
   begin
      if Sample.A then
         Result := Result + 1;
      end if;
      if Sample.B then
         Result := Result + 1;
      end if;
      if Sample.C then
         Result := Result + 1;
      end if;
      if Sample.D then
         Result := Result + 1;
      end if;
      if Sample.E then
         Result := Result + 1;
      end if;
      if Sample.F then
         Result := Result + 1;
      end if;
      if Sample.G then
         Result := Result + 1;
      end if;
      return Result;
   end Count_Segments;

   -- Returns how many options there are for the given array of candidates.
   -- If there is only one option left, Solution contains the value of that option.
   function Count_Remaining_Options(Candidates : in Options;
                                    Solution   : out Integer) return Integer is
      Result : Integer := 0;
   begin
      Count_Loop:
      for I in Candidates'Range loop
         if Candidates(I) then
            Solution := I;
            Result := Result + 1;
         end if;
      end loop Count_Loop;
      return Result;
   end Count_Remaining_Options;

   procedure Remove_Possible_Value(Outputs         : in out Output_Values;
                                   Value_To_Remove : in Integer) is
   begin
      for I in 1 .. 4 loop
         Outputs(I).Possible_Values(Value_To_Remove) := False;
      end loop;
   end Remove_Possible_Value;

   procedure Remove_Possible_Value(Samples         : in out Input_Samples;
                                   Value_To_Remove : in Integer) is
   begin
      for I in 1 .. 10 loop
         Samples(I).Possible_Values(Value_To_Remove) := False;
      end loop;
   end Remove_Possible_Value;

   -- Solve the 1, 4, 7 and 8 cases.
   procedure Solve_Obvious (Current_Entry : in out Input_Entry) is
      Current_Nr_Of_Segments : Integer;
      Result : Integer := 0;
   begin
      Obvious_Values_Loop:
      for S in 1 .. 4 loop
         Current_Nr_Of_Segments := Count_Segments(Current_Entry.Output(S));
         if Current_Nr_Of_Segments = 2 then
            Current_Entry.Output(S).Solution_Found := True;
            Current_Entry.Output(S).Solution := 1;
            Remove_Possible_Value(Current_Entry.Output, 1);
         elsif Current_Nr_Of_Segments = 4 then
            Current_Entry.Output(S).Solution_Found := True;
            Current_Entry.Output(S).Solution := 4;
            Remove_Possible_Value(Current_Entry.Output, 4);
         elsif Current_Nr_Of_Segments = 3 then
            Current_Entry.Output(S).Solution_Found := True;
            Current_Entry.Output(S).Solution := 7;
            Remove_Possible_Value(Current_Entry.Output, 7);
         elsif Current_Nr_Of_Segments = 7 then
            Current_Entry.Output(S).Solution_Found := True;
            Current_Entry.Output(S).Solution := 8;
            Remove_Possible_Value(Current_Entry.Output, 8);
         elsif Current_Nr_Of_Segments = 5 then
            Put("");
         elsif Current_Nr_Of_Segments = 6 then
            Put("");
         else
            Put_Line("ERROR! Number of segments is " & Integer'Image(Current_Nr_Of_Segments));
         end if;
      end loop Obvious_Values_Loop;

      Obvious_Samples_Loop:
      for S in 1 .. 10 loop
         Current_Nr_Of_Segments := Count_Segments(Current_Entry.Samples(S));
         if Current_Nr_Of_Segments = 2 then
            Current_Entry.Samples(S).Solution_Found := True;
            Current_Entry.Samples(S).Solution := 1;
            Remove_Possible_Value(Current_Entry.Samples, 1);
         elsif Current_Nr_Of_Segments = 4 then
            Current_Entry.Samples(S).Solution_Found := True;
            Current_Entry.Samples(S).Solution := 4;
            Remove_Possible_Value(Current_Entry.Samples, 4);
         elsif Current_Nr_Of_Segments = 3 then
            Current_Entry.Samples(S).Solution_Found := True;
            Current_Entry.Samples(S).Solution := 7;
            Remove_Possible_Value(Current_Entry.Samples, 7);
         elsif Current_Nr_Of_Segments = 7 then
            Current_Entry.Samples(S).Solution_Found := True;
            Current_Entry.Samples(S).Solution := 8;
            Remove_Possible_Value(Current_Entry.Samples, 8);
         elsif Current_Nr_Of_Segments = 5 then
            Put("");
         elsif Current_Nr_Of_Segments = 6 then
            Put("");
         else
            Put_Line("ERROR! Number of segments is " & Integer'Image(Current_Nr_Of_Segments));
         end if;
      end loop Obvious_Samples_Loop;
   end Solve_Obvious;

   function Get_Sample(Samples : in Input_Samples;
                       Expected_Value : in Integer) return Wires is
   begin
      for I in 1 .. 10 loop
         if Samples(I).Solution_Found and then
           Samples(I).Solution = Seven_Segment_Digit(Expected_Value) then
            return Samples(I);
         end if;
      end loop;
      Put_Line("ERROR! Sample for " & Integer'Image(Expected_Value) & " not found!");
      return Samples(1);
   end Get_Sample;

   -- Returns true of Subject contains all the segments that are in Comparison
   function Contains_All(Subject : in Wires;
                         Comparison : in Wires) return Boolean is
   begin
      if Comparison.A and then not Subject.A then
         return False;
      elsif Comparison.B and then not Subject.B then
         return False;
      elsif Comparison.C and then not Subject.C then
         return False;
      elsif Comparison.D and then not Subject.D then
         return False;
      elsif Comparison.E and then not Subject.E then
         return False;
      elsif Comparison.F and then not Subject.F then
         return False;
      elsif Comparison.G and then not Subject.G then
         return False;
      end if;
      return True;
   end Contains_All;

   -- If the number of segments in a sample is 6, then:
   -- - It is a 6 if not all the segments from the 1 are lit.
   -- - It is a 9 if all segments from the 4 are lit.
   -- - It is a 0 otherwise.
   -- If the number of segments in a sample is 5, then:
   -- - It is a 3 if all segments from the 1 are lit.
   -- - It is a 5 if only one segments differs from the 6.
   -- - It is a 2 otherwise.
   procedure Solve_Samples (Samples : in out Input_Samples) is
      Sample_One  : Wires;
      Sample_Four : Wires;
      Sample_Six  : Wires;
   begin
      Sample_One := Get_Sample(Samples, 1);
      Sample_Four := Get_Sample(Samples, 4);
      Six_Segments_Loop:
      for I in 1 .. 10 loop
         if not Samples(I).Solution_Found and then
           Count_Segments(Samples(I)) = 6 then
            if not Contains_All(Samples(I), Sample_One) then
               Samples(I).Solution_Found := True;
               Samples(I).Solution := 6;
               Remove_Possible_Value(Samples, 6);
            elsif Contains_All(Samples(I), Sample_Four) then
               Samples(I).Solution_Found := True;
               Samples(I).Solution := 9;
               Remove_Possible_Value(Samples, 9);
            else
               Samples(I).Solution_Found := True;
               Samples(I).Solution := 0;
               Remove_Possible_Value(Samples, 0);
            end if;
         end if;
      end loop Six_Segments_Loop;

      Sample_Six := Get_Sample(Samples, 6);
      Five_Segments_Loop:
      for I in 1 .. 10 loop
         if not Samples(I).Solution_Found and then
           Count_Segments(Samples(I)) = 5 then
            if Contains_All(Samples(I), Sample_One) then
               Samples(I).Solution_Found := True;
               Samples(I).Solution := 3;
               Remove_Possible_Value(Samples, 3);
            elsif Contains_All(Sample_Six, Samples(I)) then
               Samples(I).Solution_Found := True;
               Samples(I).Solution := 5;
               Remove_Possible_Value(Samples, 5);
            else
               Samples(I).Solution_Found := True;
               Samples(I).Solution := 2;
               Remove_Possible_Value(Samples, 2);
            end if;
         end if;
      end loop Five_Segments_Loop;
   end Solve_Samples;

   procedure Verify_Samples(Samples : in out Input_Samples) is
   begin
      Verify_Loop:
      for I in 1 .. 10 loop
         if not Samples(I).Solution_Found then
            Put_Line("ERROR: Solution not found!");
         end if;
      end loop Verify_Loop;
   end Verify_Samples;

   procedure Solve_Values(Current_Entry : in out Input_Entry) is
      Current_Output : Wires;
      Current_Sample : Wires;
   begin
      Output_Loop:
      for I in 1 .. 4 loop
         Current_Output := Current_Entry.Output(I);
         Sample_Loop:
         for J in 1 .. 10 loop
            Current_Sample := Current_Entry.Samples(J);
            if (Contains_All(Current_Sample, Current_Output) and then
                Contains_All(Current_Output, Current_Sample)) then
               Current_Entry.Output(I).Solution_Found := True;
               Current_Entry.Output(I).Solution := Current_Sample.Solution;
               exit Sample_Loop;
            end if;
         end loop Sample_Loop;
      end loop Output_Loop;

   end Solve_Values;

   -- Count the number of 1s, 4s, 7s and 8s in the Output
   procedure Solve(Current_Entry : in out Input_Entry) is
      Result : Integer := 0;
   begin
      Solve_Obvious(Current_Entry);
      Solve_Samples(Current_Entry.Samples);

      Verify_Samples(Current_Entry.Samples);

      Solve_Values(Current_Entry);
   end Solve;

   function Get_Output_Value(Current_Entry : in out Input_Entry) return Integer is
      Parsed_Value : Integer;
   begin
      Parsed_Value := Integer(Current_Entry.Output(1).Solution) * 1_000
        + Integer(Current_Entry.Output(2).Solution) * 100
        + Integer(Current_Entry.Output(3).Solution) * 10
        + Integer(Current_Entry.Output(4).Solution);
      return Parsed_Value;
   end Get_Output_Value;

   Entries : Input_Data.Vector;
   Answer  : Integer := 0;
begin
   Load_File(Entries);

   Solve_Loop:
   for E in Iterate(Entries) loop
      Solve(Entries(E));
   end loop Solve_Loop;

   Calculate_Sum_Loop:
   for F in Iterate(Entries) loop
      Answer := Answer + Get_Output_Value(Entries(F));
   end loop Calculate_Sum_Loop;

   Put_line("The sum of all output values is " & Integer'Image(Answer) & ".");
end Day08_2;
