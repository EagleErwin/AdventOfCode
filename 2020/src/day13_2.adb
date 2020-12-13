with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Containers;           use Ada.Containers;
with Ada.Containers.Vectors;

procedure Day13_2 is
   -- ### CONSTANTS ### --
   Enable_Debug : constant Boolean := False;
   Dont_Care    : constant Integer := 0;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Bus is record
      ID  : Integer;
      Idx : Integer;
   end record;

   type Repetition is record
      Period : Long_Integer;
      Start_Timestamp : Long_Integer;
   end record;

   package Timestamp_Vector is new Vectors (Natural, Bus);
   use     Timestamp_Vector;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   procedure To_Vector(Input  : in     Unbounded_String;
                       Output : in out Vector) is
      Curr_Char  : Character;
      Buffer     : Unbounded_String := To_Unbounded_String("");
      Curr_Value : Integer;
      Idx        : Integer := 0;
   begin
      Parser_Loop:
      for I in 1 .. Length(Input) loop
         Curr_Char := Element(Input, I);
         case Curr_Char is
            when 'x' => Buffer := To_Unbounded_String(Integer'Image(Dont_Care));
            when ',' =>
               Curr_Value := Integer'Value(To_String(Buffer));
               if Curr_Value /= Dont_Care then
                  Output.Append(Bus'(ID  => Curr_Value, Idx => Idx));
                  Log_Debug("Adding bus: " & Integer'Image(Curr_Value));
               else
                  Log_Debug("Skipping bus");
               end if;
               Buffer := To_Unbounded_String("");
               Idx := Idx + 1;
            when others => Buffer := Buffer & Curr_Char;
         end case;
      end loop Parser_Loop;
      -- Don't forget the last one!
      Curr_Value := Integer'Value(To_String(Buffer));
      Output.Append(Bus'(ID  => Curr_Value, Idx => Idx));
      Log_Debug("Adding bus: " & Integer'Image(Curr_Value));
   end To_Vector;

   procedure Load_File (TOA     : out Long_Integer;
                        Bus_Ids : out Vector) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 1;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day13.input");
      -- First line is the time of arrival
      TOA := Long_Integer'Value(Get_Line(Input));
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         To_Vector(Current_Line, Bus_Ids);
         Idx := Idx + 1;
      end loop;
      Close (File => Input);
   end Load_File;

   function Verify_Times(Bus_Ids : in Vector;
                         Timestamp : in Long_Integer) return Boolean is
      Current_Bus : Bus;
   begin
      Time_Validation_Loop:
      for Idx in Bus_Ids.First_Index .. Bus_Ids.Last_Index loop
         Current_Bus := Bus_Ids(Idx);
         if Current_Bus.ID /= Dont_Care and then
           (Timestamp + Long_Integer(Idx)) mod Long_Integer(Current_Bus.ID) /= 0 then
            Log_Debug("Fail!");
            return False;
         end if;
         Log_Debug("Pass!");
      end loop Time_Validation_Loop;
      return True;
   end Verify_Times;

   -- Retrieves the time stamp where the two busses have the correct order.
   function Find_First_Occur(Repetition_A : in Repetition;
                             New_Bus      : in Bus) return Long_Integer is
      Timestamp : Long_Integer := Repetition_A.Start_Timestamp;
   begin
      if New_Bus.ID = Dont_Care then
         return Repetition_A.Period;
      end if;

      Log_Debug("Repetition_A: Period: " & Long_Integer'Image(Repetition_A.Period)
                & " Start: " & Long_Integer'Image(Repetition_A.Start_Timestamp)
                & " New_Bus: ID: " & Integer'Image(New_Bus.ID)
                & " Idx: " & Integer'Image(New_Bus.Idx));
      while True loop
         Timestamp := Timestamp + Repetition_A.Period;
         if (Timestamp + Long_Integer(New_Bus.Idx)) mod Long_Integer(New_Bus.ID) = 0 then
            return Timestamp;
         end if;
      end loop;
      Put_Line("ERROR: Broke out of infinite loop!");
      return Timestamp;
   end Find_First_Occur;

   -- Calculate the repetion time of all busses
   function Calculate_Repetion(Bus_Ids : in Vector) return Repetition is
      Head : Vector := Bus_Ids;
      Last_Bus : Bus := Bus_Ids.Last_Element;
      Intermediate : Repetition;
      Start_Timestamp : Long_Integer;
      Result : Repetition;
   begin
      Log_Debug("Calculate for " & Integer'Image(Integer(Bus_Ids.Length)));
      Head.Delete_Last;
      if Bus_Ids.Length = 1 then
         Log_Debug("Only one left");
         Result := Repetition'(Period          => Long_Integer(Bus_Ids.First_Element.ID),
                               Start_Timestamp => 0);
      else
         Intermediate := Calculate_Repetion(Head);
         Start_Timestamp := Find_First_Occur(Intermediate, Last_Bus);
         Log_Debug("New start timestamp: " & Long_Integer'Image(Start_Timestamp));
         Result := Repetition'(Period => Intermediate.Period * Long_Integer(Last_Bus.ID),
                               Start_Timestamp => Start_Timestamp);
      end if;
      return Result;
   end Calculate_Repetion;

   function Find_Candidate (Bus_Ids : in Vector) return Long_Integer is
   begin
      return Calculate_Repetion(Bus_Ids).Start_Timestamp;
   end Find_Candidate;

   Earliest_Tmestamp : Long_Integer;
   Busses            : Vector;

   Time_Of_Departure : Long_Integer;
begin
   Load_File(Earliest_Tmestamp, Busses);

   Time_Of_Departure := Find_Candidate(Busses);

   Put_line("The earliest timestamp that matches is "
            & Long_Integer'Image(Time_Of_Departure));
end Day13_2;
