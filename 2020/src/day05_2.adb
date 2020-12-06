with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day05_2 is
   -- ### CONSTANTS ### --
   Enable_Debug : constant Boolean := False;
   Number_Of_Inputs : constant Integer := 868;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Seat_Id_Array is array (1 .. Number_Of_Inputs) of Integer;
   type Row_String is new String (1 .. 10); -- 7 bits plus Ada base indication
   type Column_String is new String (1 .. 6); -- 3 bits plus Ada base indication

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   function Convert_To_Seat_Id (Position : Unbounded_String) return Integer is
      Row : Row_String := "2#_______#";
      Column : Column_String := "2#___#";

      Curr_Char : Character;
   begin
      Row_Loop:
      for R in 1 .. 7 loop
         Curr_Char := Element(Position, R);
         if Curr_Char = 'B' then
            Row(R + 2) := '1';
         elsif Curr_Char = 'F' then
            Row(R + 2) := '0';
         else
            Put_Line("Invalid character encountered: " & Curr_Char
                     & ", expected 'B' or 'F'");
         end if;
      end loop Row_Loop;

      Column_Loop:
      for C in 1 .. 3 loop
         Curr_Char := Element(Position, C+7);
         if Curr_Char = 'R' then
            Column(C + 2) := '1';
         elsif Curr_Char = 'L' then
            Column(C + 2) := '0';
         else
            Put_Line("Invalid character encountered: " & Curr_Char
                     & ", expected 'R' or 'L'");
         end if;
      end loop Column_Loop;
      return Integer'Value(String(Row)) * 8 + Integer'Value(String(Column));
   end Convert_To_Seat_Id;

   function Array_Contains(Haystack : in Seat_Id_Array;
                           Needle : in Integer) return Boolean is
   begin
      Search_Loop:
      for I of Haystack loop
         if I = Needle then
            return True;
         end if;
      end loop Search_Loop;
      return False;
   end Array_Contains;

   function Find_Seat_Id(Seat_Ids : in Seat_Id_Array;
                        Max_Seat_Id : in Integer) return Integer is
   begin
      All_Possible_Seat_Ids_Loop:
      for My_Id in 1 .. Max_Seat_Id loop
         if not Array_Contains(Seat_Ids, My_Id) then
            Log_Debug("Checking free seat " & Integer'Image(My_Id));
            if Array_Contains(Seat_Ids, (My_Id - 1)) and then
              Array_Contains(Seat_Ids, (My_Id + 1)) then
               return My_Id;
            end if;
         end if;
      end loop All_Possible_Seat_Ids_Loop;
      Put_Line("Seat not found!");
      return -1;
   end Find_Seat_Id;

   procedure Load_File (Boarding_Positions : out Seat_Id_Array) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 1;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day05.input");
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Boarding_Positions(Idx) := Convert_To_Seat_Id(Current_Line);
         Idx := Idx + 1;
      end loop;
      Close (File => Input);
   end Load_File;

   Boarding_Passes : Seat_Id_Array;
   Max_Seat_Id : Integer := 0;

   Answer : Integer := -1;
begin
   Load_File(Boarding_Passes);

   for I in Boarding_Passes'Range loop
      if Boarding_Passes(I) > Max_Seat_Id then
         Max_Seat_Id := Boarding_Passes(I);
      end if;
   end loop;
   Log_Debug("Found max seat ID: " & Integer'Image(Max_Seat_Id));
   Answer := Find_Seat_Id(Boarding_Passes, Max_Seat_Id);

   Put_line("My seat ID is " & Integer'Image(Answer));
end Day05_2;
