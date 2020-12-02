with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day02_1 is
   -- ### CONSTANTS ### --
   Enable_Debug : constant Boolean := False;
   Number_Of_Inputs : constant Integer := 1000;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;
   type Password_Record is record
      Min_Occurs : Integer;
      Max_Occurs : Integer;
      Letter     : Character;
      Password   : Unbounded_String;
   end record;

   type Password_Database is array (1 .. Number_Of_Inputs) of Password_Record;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   function Get_Min_Occurs(Input_String: in Unbounded_String) return Integer is
      Buffer : Unbounded_String := To_Unbounded_String("");
      Curr_Char : Character;
   begin
      Parser_Loop:
      for I in 1 .. Length(Input_String) loop
         Curr_Char := Element(Input_String, I);
         if Curr_Char = '-' then
            exit Parser_Loop;
         else
            Buffer := Buffer & Curr_Char;
         end if;
      end loop Parser_Loop;

      return Integer'Value(To_String(Buffer));
   end Get_Min_Occurs;

   function Get_Max_Occurs(Input_String: in Unbounded_String) return Integer is
      Buffer : Unbounded_String := To_Unbounded_String("");
      Curr_Char : Character;
   begin
      Parser_Loop:
      for I in 1 .. Length(Input_String) loop
         Curr_Char := Element(Input_String, I);
         if Curr_Char = '-' then
            Buffer := To_Unbounded_String("");
         elsif Curr_Char = ' ' then
               exit Parser_Loop;
         else
            Buffer := Buffer & Curr_Char;
         end if;
      end loop Parser_Loop;

      return Integer'Value(To_String(Buffer));
   end Get_Max_Occurs;

   function Get_Letter(Input_String: in Unbounded_String) return Character is
      Curr_Char : Character;
   begin
      Parser_Loop:
      for I in 1 .. Length(Input_String) loop
         Curr_Char := Element(Input_String, I);
         if Curr_Char = ' ' then
            return Element(Input_String, I+1);
         end if;
      end loop Parser_Loop;

      Put_Line("Error! Unable to fetch character from input.");
      return ' ';
   end Get_Letter;

   function Get_Password(Input_String: in Unbounded_String)
                         return Unbounded_String is
      Buffer : Unbounded_String := To_Unbounded_String("");
      Curr_Char : Character;
   begin
      Parser_Loop:
      for I in 1 .. Length(Input_String) loop
         Curr_Char := Element(Input_String, I);
         if Curr_Char = ':' then
            Buffer := To_Unbounded_String("");
         elsif Curr_Char /= ' ' then
            Buffer := Buffer & Curr_Char;
         end if;
      end loop Parser_Loop;

      return Buffer;
   end Get_Password;

   function Create_Password_Record(Raw_Data: in Unbounded_String)
                                   return Password_Record is
      Result : Password_Record;

      Min : Integer;
      Max : Integer;
      Letter : Character;
      Password : Unbounded_String;
   begin
      Min := Get_Min_Occurs(Raw_Data);
      Max := Get_Max_Occurs(Raw_Data);
      Letter := Get_Letter(Raw_Data);
      Password := Get_Password(Raw_Data);

      Result := Password_Record'(Min_Occurs => Min,
                                 Max_Occurs => Max,
                                 Letter     => Letter,
                                 Password   => Password);
      return Result;
   end Create_Password_Record;

   procedure Load_File (Database : out Password_Database) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 1;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day02.input");
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Database(Idx) := Create_Password_Record(Current_Line);
         Idx := Idx + 1;
      end loop;
      Close (File => Input);
   end Load_File;

   function Validate_Password(To_Validate : in Password_Record)
                              return Boolean is
      Curr_Char : Character;
      Occurences : Integer := 0;
      Result : Boolean;
   begin
      Check_Loop:
      for I in 1 .. Length(To_Validate.Password) loop
         Curr_Char := Element(To_Validate.Password, I);
         if Curr_Char = To_Validate.Letter then
            Occurences := Occurences + 1;
         end if;
      end loop Check_Loop;

      Result := Occurences >= To_Validate.Min_Occurs and then
        Occurences <= To_Validate.Max_Occurs;

      if Result then
         Log_Debug("Password " & To_String(To_Validate.Password) & " is valid.");
      end if;
      return Result;
   end Validate_Password;

   Database : Password_Database;
   Answer : Integer := 0;
begin
   Load_File(Database);

   for I in Database'Range loop
      if Validate_Password(Database(I)) then
         Answer := Answer + 1;
      end if;
   end loop;

   Put_line("The number of valid passwords is " & Integer'Image(Answer));
end Day02_1;
