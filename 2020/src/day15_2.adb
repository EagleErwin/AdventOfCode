with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;     use Ada.Containers;
with Ada.Containers.Hashed_Maps; use Ada.Containers;

procedure Day15_2 is
   -- ### CONSTANTS ### --
   Enable_Debug     : constant Boolean := False;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   package Sequence_Vector is new Vectors (Natural, Integer);
   use     Sequence_Vector;

   function Integer_Hash (I : Integer) return Hash_Type is
   begin
      return Hash_Type(I);
   end Integer_Hash;

   package Dictionary_Map is new Hashed_Maps(Key_Type        => Integer,
                                             Element_Type    => Integer,
                                             Hash            => Integer_Hash,
                                             Equivalent_Keys => "=");
   use Dictionary_Map;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   procedure Load_File (Sequence : out Sequence_Vector.Vector) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 1;

      Buffer : Unbounded_String := To_Unbounded_String("");
      Curr_Char : Character;
      Spoken_Word : Integer;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day15.input");
      Read_Loop:
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Parser_Loop:
         for I in 1 .. Length(Current_Line) loop
            Curr_Char := Element(Current_Line, I);
            if Curr_Char = ',' then
               Spoken_Word := Integer'Value(To_String(Buffer));
               Sequence.Append(Spoken_Word);
               Buffer := To_Unbounded_String("");
            else
               Buffer := Buffer & Curr_Char;
            end if;
         end loop Parser_Loop;
      end loop Read_Loop;
      -- Don't forget the last one
      Spoken_Word := Integer'Value(To_String(Buffer));
      Sequence.Append(Spoken_Word);
      Close (File => Input);
   end Load_File;

   -- Returns the time since last word is spoken, or 0 if it was not spoken yet.
   function Last_Spoken(History    : in out Sequence_Vector.Vector;
                        Dictionary : in out Dictionary_Map.Map) return Integer is
      Last       : Integer := History.Last_Element;
      Last_Index : Integer := Integer(History.Last_Index);
   begin
      if Dictionary.Contains(Last) then
         Log_Debug("Found number " & Integer'Image(Last) & " at index "
                  & Integer'Image(Dictionary(Last)) & " in dictionary");
         Log_Debug("Returning " & Integer'Image(Last_Index - Dictionary(Last)));
         return Last_Index - Dictionary(Last);
      else
         return 0;
      end if;
   end Last_Spoken;

   function Speak(History : in out Sequence_Vector.Vector;
                  Subject : in Integer) return Integer is
      New_Value  : Integer;
      Last_Value : Integer;
      Dictionary : Dictionary_Map.Map;
      Idx        : Integer := 0;
   begin
      Fill_Dict_Loop:
      for I of History loop
         Log_Debug("Inserting " & Integer'Image(I) & " on index " & Integer'Image(Idx));
         Dictionary.Insert(I, Idx);
         Idx := Idx + 1;
      end loop Fill_Dict_Loop;

      Speak_Loop:
      while Integer(History.Length) < Subject loop
         Last_Value := History.Last_Element;
         New_Value := Last_Spoken(History, Dictionary);
         History.Append(New_Value);
         if not Dictionary.Contains(Last_Value) then
            Dictionary.Insert(Last_Value, History.Last_Index - 1);
         else
            Log_Debug("Updating index of " & Integer'Image(Last_Value)
                      & " to " & Integer'Image(History.Last_Index - 1));
            Dictionary(Last_Value) := History.Last_Index - 1;
         end if;

      end loop Speak_Loop;
      return History(Subject - 1);
   end Speak;

   Spoken_Words : Sequence_Vector.Vector;
   Answer       : Integer;
begin
   Load_File(Spoken_Words);

   Answer := Speak(Spoken_Words, 30_000_000);

   if Enable_Debug then
      Print_Loop:
      for E of Spoken_Words loop
         Put_Line(Integer'Image(E));
      end loop Print_Loop;
   end if;

   Put_line("The 30000000th spoken word is " & Integer'Image(Answer));
end Day15_2;
