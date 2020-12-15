with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day15_1 is
   -- ### CONSTANTS ### --
   Enable_Debug     : constant Boolean := False;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   package Sequence_Vector is new Vectors (Natural, Integer);
   use     Sequence_Vector;

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
   function Last_Spoken(History : in out Sequence_Vector.Vector) return Integer is
      Last : Integer;
      Counter : Integer := 1;
   begin
      Last := History.Last_Element;
      Search_Loop:
      for I in reverse 0 .. (History.Length - 2) loop
         if History(Integer(I)) = Last then
            return Counter;
         end if;
         Counter := Counter + 1;
      end loop Search_Loop;

      Counter := 0;
      return Counter;
   end Last_Spoken;

   function Speak(History : in out Sequence_Vector.Vector;
                  Subject : in Integer) return Integer is
      New_Value : Integer;
   begin
      Speak_Loop:
      while Integer(History.Length) < Subject loop
         New_Value := Last_Spoken(History);
         History.Append(New_Value);
      end loop Speak_Loop;
      return History(Subject - 1);
   end Speak;

   Spoken_Words : Sequence_Vector.Vector;
   Answer       : Integer;
begin
   Load_File(Spoken_Words);

   Answer := Speak(Spoken_Words, 2020);

   if Enable_Debug then
      Print_Loop:
      for E of Spoken_Words loop
         Put_Line(Integer'Image(E));
      end loop Print_Loop;
   end if;

   Put_line("The 2020th spoken word is " & Integer'Image(Answer));
end Day15_1;
