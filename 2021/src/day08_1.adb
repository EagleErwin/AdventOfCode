with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day08_1 is
   -- ### CONSTANTS ### --

   type Seven_Segment_Digit is new Integer range 0 .. 9;

   type Options is array (0 .. 9) of Boolean;

   -- ### TYPE DEFINITIONS ### --
   --  A
   -- B C
   --  D
   -- E F
   --  G
   type Seven_Segment_Number is record
      A : Boolean;
      B : Boolean;
      C : Boolean;
      D : Boolean;
      E : Boolean;
      F : Boolean;
      G : Boolean;
   end record;

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

      Empty_Value : Wires := (False, False, False, False, False, False, False, (others => False), False, 0);
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


   -- Count the number of 1s, 4s, 7s and 8s in the Output
   function Count_Number(Entries : Input_Data.Vector) return Integer is
      Current_Entry : Input_Entry;
      Current_Nr_Of_Segments : Integer;
      Result : Integer := 0;
   begin
      Entries_Loop:
      for E in Iterate(Entries) loop
         Current_Entry := Entries(E);

         Count_Loop:
         for I in 1 .. 4 loop
            Current_Nr_Of_Segments := Count_Segments(Current_Entry.Output(I));
            if (Current_Nr_Of_Segments = 2 or else -- 1
                Current_Nr_Of_Segments = 4 or else -- 4
                Current_Nr_Of_Segments = 3 or else -- 7
                Current_Nr_Of_Segments = 7) then --8
                Result := Result + 1;
            end if;
         end loop Count_Loop;
      end loop Entries_Loop;
      return Result;
   end Count_Number;

   Entries : Input_Data.Vector;
   Answer  : Integer := 0;
begin
   Load_File(Entries);

   Answer := Count_Number(Entries);

   Put_line("The digits 1, 4, 7 and 8 appear a total of "
            & Integer'Image(Answer) & " times.");
end Day08_1;
