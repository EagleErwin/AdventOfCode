with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Containers.Generic_Array_Sort; use Ada.Containers;
procedure Day04_2 is
   Data_Line_Amount : constant Integer := 1159; -- Number of lines in the dataset
   Max_Number_Of_Guards : Integer := 3500; -- Will be less
   Max_Number_Of_Sleeps : Integer := 60; -- Will be when guard falls asleep every minute

   subtype Data_Line is Unbounded_String;
   type Raw_Data is array (Natural range <>) of Data_Line;

   type Nap is record
      Sleep : Integer;
      Wake  : Integer;
   end record;
   
   type Nap_Access is access all Nap;

   type Nap_Array is array (1 .. Max_Number_Of_Sleeps) of Nap_Access;
   type Guard is record
      Id : Integer; -- Id 0 will be 'no guard'
      Nr_Of_Naps : Integer;
      Naps : Nap_Array;
   end record;
   
   type Guard_Array is array (1 .. Max_Number_Of_Guards) of Guard;

   type Event is (Start_Shift, Fall_Asleep, Wake_Up, None);
   
   procedure Input_Sort is new Generic_Array_Sort
     (Index_Type => Natural,
      Element_Type => Unbounded_String,
     Array_Type => Raw_Data);
      
   procedure Load_File (Data : out Raw_Data) is
      Current_Line   : Data_Line;
      Input          : File_Type;
      Row_Idx        : Integer := 0;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day04.input");
      while not End_Of_File (Input) loop
         Current_Line   := To_Unbounded_String (Get_Line (Input));
         Row_Idx        := Row_Idx + 1;
         Data (Row_Idx) := Current_Line;
      end loop;
      Close (File => Input);

      Input_Sort(Data);
   end Load_File;
   
   function Get_Event_Type (Input : in Data_Line) return Event is
      Position : constant Integer := 20;
      Result : Event;
   begin
      if Element(Input, Position) = 'G' then
         Result := Start_Shift;
      elsif Element(Input, Position) = 'f' then
         Result := Fall_Asleep;
      elsif Element(Input, Position) = 'w' then
         Result := Wake_Up;
      else
         Put_Line("Warning! Unrecognized event detected");
         Result := None;
      end if;
           
      return Result;
   end Get_Event_Type;
   
   function Get_Minute (Input : in Data_Line) return Integer is
      Pos    : constant Integer := 16;
      Buffer : Unbounded_String;
   begin
      Buffer := Buffer & Element(Input, Pos) & Element(Input, Pos + 1);
      return Integer'Value(To_String(Buffer));
   end Get_Minute;
   
   function Get_Guard_Id (Input : in Data_Line) return Integer is
      Pos : Integer := 27;
      Buffer : Unbounded_String;
   begin
      while Element(Input, Pos) /= ' ' loop
         Buffer := Buffer & Element(Input, Pos);
         Pos := Pos + 1;
      end loop;
      return Integer'Value(To_String(Buffer));
   end Get_Guard_Id;
   
   procedure Parse_Data (Input : in Raw_Data; Output: out Guard_Array) is
      State : Event;
      Guard_Index : Integer;
      Nap_Index : Integer := 0;
      Current_Nap_Start : Integer := 0;
   begin
      for I in Input'Range loop
         State := Get_Event_Type(Input(I));
         if State = Start_Shift then
            Guard_Index := Get_Guard_Id(Input(I));
            Nap_Index := 0;
            Output(Guard_Index).Id := Guard_Index;
         elsif State = Fall_Asleep then
            Current_Nap_Start := Get_Minute(Input(I));
         elsif State = Wake_Up then
            Nap_Index := Output(Guard_Index).Nr_Of_Naps + 1;
            Output(Guard_Index).Naps(Nap_Index) := new Nap'(Sleep => Current_Nap_Start,
                                                            Wake => Get_Minute(Input(I)) - 1);
            Output(Guard_Index).Nr_Of_Naps := Nap_Index;
         end if;
      end loop;
   end Parse_Data;
   
   function Get_Sleep_Duration (Input : in Nap_Access) return Integer is
   begin
      return Input.Wake - Input.Sleep;
   end Get_Sleep_Duration;
      
   function Get_Best_Minute (Input: in Guard; Amount : out Integer) return Integer is
      type Clock is array (0 .. 59) of Integer;
      Administration : Clock := (others => 0);
      Best_Minute : Integer := 0;
      Longest_Nap : Integer := 0;
   begin
      Nap_Loop:
      for I in Input.Naps'Range loop
         exit Nap_Loop when Input.Naps(I) = null;
         for M in Integer range Input.Naps(I).Sleep .. Input.Naps(I).Wake loop
            Administration(M) := Administration(M)+1;
         end loop;
      end loop Nap_Loop;

      Best_Minute_Loop:
      for J in Administration'Range loop
         if Administration(J) > Longest_Nap then
            Longest_Nap := Administration(J);
            Best_Minute := J;
         end if;
      end loop Best_Minute_Loop;
      
      Amount := Longest_Nap;
      
      return Best_Minute;
   end Get_Best_Minute;

   Sorted_Data : Raw_Data (1 .. Data_Line_Amount);
   Parsed_Data : Guard_Array;
   Answer : Integer := 0;
   Best_Guard : Guard;
   Best_Minute : Integer := 0;
   Best_Amount : Integer := 0;
   Current_Amount : Integer;
   Current_Minute : Integer;
begin
   Load_File (Data => Sorted_Data);
   
   Parse_Data (Input => Sorted_Data, Output => Parsed_Data);
  
   for I in Parsed_Data'Range loop
      Current_Minute := Get_Best_Minute(Parsed_Data(I), Current_Amount);
      if Current_Amount > Best_Amount then
         Best_Guard := Parsed_Data(I);
         Best_Minute := Current_Minute;
         Best_Amount := Current_Amount;
      end if;
   end loop;

   Put_Line("Id of the best guard: " & Integer'Image(Best_Guard.Id));
   Put_Line("Minute of the most naps: " & Integer'Image(Best_Minute) & " ( " & Integer'Image(Best_Amount) & " times)");
   
   Answer := Best_Guard.Id * Best_Minute;
   Put_Line("Answer: " & Integer'Image(Answer));
end Day04_2;
