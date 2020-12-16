with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;     use Ada.Containers;
with Ada.Containers.Hashed_Maps; use Ada.Containers;

procedure Day16_1 is
   -- ### CONSTANTS ### --
   Enable_Debug     : constant Boolean := False;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Value_Administration is array (0 .. 1_000) of Boolean;

   type Parser_State is (Fields, My_Ticket, Nearby_Tickets);
   type Fields_Parser_State is (Field_Name, Range_1_Begin, Range_1_End,
                                Separator, Range_2_Begin, Range_2_End);

   package Number_Vector is new Vectors (Natural, Integer);
   use     Number_Vector;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   procedure Add_Range(Start : in Integer;
                       Stop  : in Integer;
                       Admin : in out Value_Administration) is
   begin
      for I in Start .. Stop loop
         Admin(I) := True;
      end loop;
   end Add_Range;

   procedure Add_Allowed_Values(Input : in Unbounded_String;
                                Administration : in out Value_Administration) is
      Buffer : Unbounded_String := To_Unbounded_String("");
      Curr_Char : Character;
      State : Fields_Parser_State := Field_Name;
      Range_Begin : Integer;
      Range_End : Integer;
   begin
      Parser_Loop:
      for I in 1 .. Length(Input) loop
         Curr_Char := Element(Input, I);
         case State is
            when Field_Name =>
               if Curr_Char = ':' then
                  State := Range_1_Begin;
               end if;
            when Range_1_Begin =>
               if Curr_Char = '-' then
                  Range_Begin := Integer'Value(To_String(Buffer));
                  State := Range_1_End;
                  Buffer := To_Unbounded_String("");
               elsif Curr_Char /= ' ' then
                  Buffer := Buffer & Curr_Char;
               end if;
            when Range_1_End =>
               if Curr_Char = ' ' then
                  Range_End := Integer'Value(To_String(Buffer));
                  Add_Range(Range_Begin, Range_End, Administration);
                  State := Separator;
                  Buffer := To_Unbounded_String("");
               else
                  Buffer := Buffer & Curr_Char;
               end if;
            when Separator =>
               if Curr_Char = ' ' then
                  Buffer := To_Unbounded_String("");
                  State := Range_2_Begin;
               end if;
            when Range_2_Begin =>
               if Curr_Char = '-' then
                  Range_Begin := Integer'Value(To_String(Buffer));
                  State := Range_2_End;
                  Buffer := To_Unbounded_String("");
               else
                  Buffer := Buffer & Curr_Char;
               end if;
            when Range_2_End =>
               Buffer := Buffer & Curr_Char;
         end case;
      end loop Parser_Loop;
      Range_End := Integer'Value(To_String(Buffer));
      Add_Range(Range_Begin, Range_End, Administration);
   end Add_Allowed_Values;

   procedure Append_Values(Input : in Unbounded_String;
                           Numbers : in out Number_Vector.Vector) is
      Buffer : Unbounded_String := To_Unbounded_String("");
      Curr_Char : Character;
   begin
      Parser_Loop:
      for I in 1 .. Length(Input) loop
         Curr_Char := Element(Input, I);
         if Curr_Char = ',' then
            Numbers.Append(Integer'Value(To_String(Buffer)));
            Buffer := To_Unbounded_String("");
         else
            Buffer := Buffer & Curr_Char;
         end if;
      end loop Parser_Loop;
      -- Don't forget the last one
      Numbers.Append(Integer'Value(To_String(Buffer)));
   end Append_Values;

   procedure Load_File (Numbers : out Number_Vector.Vector;
                        Administration : out Value_Administration) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 1;

      State : Parser_State := Fields;
      Buffer : Unbounded_String := To_Unbounded_String("");
   begin
      Administration := (others => False);
      Open (File => Input, Mode => In_File, Name => "data/day16.input");
      Read_Loop:
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         case State is
            when Fields =>
               if Current_Line = To_Unbounded_String("") then
                  State := My_Ticket;
               else
                  Add_Allowed_Values(Current_Line, Administration);
               end if;
            when My_Ticket =>
               if Current_Line = To_Unbounded_String("") then
                  State := Nearby_Tickets;
               elsif Current_Line /= To_Unbounded_String("your ticket:") then
                  Log_Debug("Not needed");
               end if;
            when Nearby_Tickets =>
               if Current_Line /= To_Unbounded_String("nearby tickets:") then
                  Append_Values(Current_Line, Numbers);
               end if;
         end case;
      end loop Read_Loop;
      Close (File => Input);
   end Load_File;

   function Validate(Numbers : in Number_Vector.Vector;
                     Allowed : in Value_Administration) return Integer is
      Answer : Integer := 0;
   begin
      Check_Loop:
      for N of Numbers loop
         if not Allowed(N) then
            Answer := Answer + N;
         end if;
      end loop Check_Loop;
      return Answer;
   end Validate;

   Ticket_Numbers : Number_Vector.Vector;
   Allowed_Values : Value_Administration;

   Answer       : Integer;
begin
   Load_File(Ticket_Numbers, Allowed_Values);

   Answer := Validate(Ticket_Numbers, Allowed_Values);

   Put_line("The ticket scanning error rate is " & Integer'Image(Answer));
end Day16_1;
