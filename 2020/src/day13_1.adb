with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Containers;           use Ada.Containers;
with Ada.Containers.Vectors;

procedure Day13_1 is
   -- ### CONSTANTS ### --
   Enable_Debug     : constant Boolean := False;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   package Timestamp_Vector is new Vectors (Natural, Integer);
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
   begin
      Parser_Loop:
      for I in 1 .. Length(Input) loop
         Curr_Char := Element(Input, I);
         case Curr_Char is
            when 'x' => Buffer := To_Unbounded_String("-1");
            when ',' =>
               Curr_Value := Integer'Value(To_String(Buffer));
               if Curr_Value > 0 then
                  Output.Append(Curr_Value);
                  Log_Debug("Adding bus: " & Integer'Image(Curr_Value));
               end if;
               Buffer := To_Unbounded_String("");
            when others => Buffer := Buffer & Curr_Char;
         end case;
      end loop Parser_Loop;
      -- Don't forget the last one!
      Curr_Value := Integer'Value(To_String(Buffer));
      if Curr_Value > 0 then
         Output.Append(Curr_Value);
         Log_Debug("Adding bus: " & Integer'Image(Curr_Value));
      end if;
   end To_Vector;

   procedure Load_File (TOA     : out Integer;
                        Bus_Ids : out Vector) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 1;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day13.input");
      -- First line is the time of arrival
      TOA := Integer'Value(Get_Line(Input));
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         To_Vector(Current_Line, Bus_Ids);
         Idx := Idx + 1;
      end loop;
      Close (File => Input);
   end Load_File;

   -- Increment the (possible) departure time and check for all busses if the
   -- modulo is 0;
   function Find_Candidate (Bus_Ids   : in     Vector;
                            Arrival   : in     Integer; -- Arrival at the airport
                            Departure :    out Integer) return Integer is
      Bus_To_Take : Integer;
   begin
      Departure := Arrival;
      Find_Loop:
      while True loop
         Bus_Loop:
         for Bus of Bus_Ids loop
            if Departure mod Bus = 0 then
               Log_Debug("Found bus to take! " & Integer'Image(Bus));
               Bus_To_Take := Bus;
               exit Find_Loop;
            end if;
         end loop Bus_Loop;
         Departure := Departure + 1;
      end loop Find_Loop;

      return Bus_To_Take;
   end Find_Candidate;

   Earliest_Tmestamp : Integer;
   Busses            : Vector;

   Candidate         : Integer;
   Time_Of_Departure : Integer;
   Answer            : Integer;
begin
   Load_File(Earliest_Tmestamp, Busses);

   Candidate := Find_Candidate(Busses, Earliest_Tmestamp, Time_Of_Departure);

   Answer := Candidate * (Time_Of_Departure - Earliest_Tmestamp);

   Put_line("The ID (" & Integer'Image(Candidate)
            & ") multiplied by the time to wait ("
            & Integer'Image(Time_Of_Departure) & " - "
            & Integer'Image(Earliest_Tmestamp) &" = "
            & Integer'Image(Time_Of_Departure - Earliest_Tmestamp) & ") is "
            & Integer'Image(Answer));
end Day13_1;
