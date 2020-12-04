with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day04_2 is
   -- ### CONSTANTS ### --
   Enable_Debug : constant Boolean := False;
   Number_Of_Inputs : constant Integer := 400; --Estimation, should be large enough

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;
   type Document is record
      Byr : Integer;
      Iyr : Integer;
      Eyr : Integer;
      Hgt : Unbounded_String;
      Hcl : Unbounded_String;
      Ecl : Unbounded_String;
      Pid : Unbounded_String;
      Cid : Integer;
      Byr_Valid : Boolean;
      Iyr_Valid : Boolean;
      Eyr_Valid : Boolean;
      Hgt_Valid : Boolean;
      Hcl_Valid : Boolean;
      Ecl_Valid : Boolean;
      Pid_Valid : Boolean;
      Cid_Valid : Boolean;
   end record;

   type Field is (Byr, Iyr, Eyr, Hgt, Hcl, Ecl, Pid, Cid);

   type Length_Unit is (cm, inch);

   type Administration is array (1 .. Number_Of_Inputs) of Document;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   function Create_New_Document return Document is
   begin
      return Document'(Byr       => 0,
                       Iyr       => 0,
                       Eyr       => 0,
                       Hgt       => To_Unbounded_String("0"),
                       Hcl       => To_Unbounded_String("0"),
                       Ecl       => To_Unbounded_String("0"),
                       Pid       => To_Unbounded_String("0"),
                       Cid       => 0,
                       Byr_Valid => False,
                       Iyr_Valid => False,
                       Eyr_Valid => False,
                       Hgt_Valid => False,
                       Hcl_Valid => False,
                       Ecl_Valid => False,
                       Pid_Valid => False,
                       Cid_Valid => False);
   end Create_New_Document;

   procedure Update_Document(Key     : in Unbounded_String;
                             Value   : in Unbounded_String;
                             Subject : in out Document) is
      Curr_Field : Field;
   begin
      Log_Debug(To_String(Key) & ":" & To_String(Value));
      Curr_Field := Field'Value(To_String(Key));
      case Curr_Field is
         when Byr =>
            Subject.Byr := Integer'Value(To_String(Value));
            Subject.Byr_Valid := True;
         when Iyr =>
            Subject.Iyr := Integer'Value(To_String(Value));
            Subject.Iyr_Valid := True;
         when Eyr =>
            Subject.Eyr := Integer'Value(To_String(Value));
            Subject.Eyr_Valid := True;
         when Hgt =>
            Subject.Hgt := Value;
            Subject.Hgt_Valid := True;
         when Hcl =>
            Subject.Hcl := Value;
            Subject.Hcl_Valid := True;
         when Ecl =>
            Subject.Ecl := Value;
            Subject.Ecl_Valid := True;
         when Pid =>
            Subject.Pid := Value;
            Subject.Pid_Valid := True;
         when Cid =>
            Subject.Cid := Integer'Value(To_String(Value));
            Subject.Cid_Valid := True;
      end case;
   end Update_Document;

   procedure Process_Line(Line : in Unbounded_String;
                          Passport : in out Document) is
      Curr_Char : Character;
      Buffer : Unbounded_String := To_Unbounded_String("");
      Curr_Key : Unbounded_String;
      Curr_Value : Unbounded_String;
   begin
      Parser_Loop:
      for I in 1 .. Length(Line) loop
         Curr_Char := Element(Line, I);
         if Curr_Char = ':' then
            Curr_Key := Buffer;
            Buffer := To_Unbounded_String("");
         elsif Curr_Char = ' ' then
            Curr_Value := Buffer;
            Update_Document(Curr_Key, Curr_Value, Passport);
            Buffer := To_Unbounded_String("");
         else
            Buffer := Buffer & Curr_Char;
         end if;
      end loop Parser_Loop;
      -- Don't forget the last one
      Curr_Value := Buffer;
      Update_Document(Curr_Key, Curr_Value, Passport);
   end Process_Line;

   procedure Load_File (Documents : out Administration) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Passport     : Document;
      Idx          : Integer := 1;
      Key_Field    : Unbounded_String := To_Unbounded_String("");
      Value_Field  : Unbounded_String := To_Unbounded_String("");
   begin
      Passport := Create_New_Document;
      Open (File => Input, Mode => In_File, Name => "data/day04.input");
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         -- New record
         if Current_Line = To_Unbounded_String("") then
            Documents(Idx) := Passport;
            Idx := Idx + 1;
            Key_Field := To_Unbounded_String("");
            Value_Field := To_Unbounded_String("");
            Passport := Create_New_Document;
         else
            Process_Line(Current_Line, Passport);
         end if;
      end loop;
      Close (File => Input);
      -- Don't forget the last one
      Documents(Idx) := Passport;
   end Load_File;

   function Validate_Byr(To_Validate : in Document) return Boolean is
   begin
      if To_Validate.Byr_Valid then
         return To_Validate.Byr >= 1920 and then To_Validate.Byr <= 2002;
      else
         return True; -- Assume missing data as valid, because existence is checked somewhere else
      end if;
   end Validate_Byr;

   function Validate_Iyr(To_Validate : in Document) return Boolean is
   begin
      if To_Validate.Iyr_Valid then
         return To_Validate.Iyr >= 2010 and then To_Validate.Iyr <= 2020;
      else
         return True; -- Assume missing data as valid, because existence is checked somewhere else
      end if;
   end Validate_Iyr;

   function Validate_Eyr(To_Validate : in Document) return Boolean is
   begin
      if To_Validate.Eyr_Valid then
         return To_Validate.Eyr >= 2020 and then To_Validate.Eyr <= 2030;
      else
         return True; -- Assume missing data as valid, because existence is checked somewhere else
      end if;
   end Validate_Eyr;

   function Validate_Hgt(To_Validate : in Document) return Boolean is
      Curr_Char : Character;
      Buffer : Unbounded_String := To_Unbounded_String("");
      Value_String : Unbounded_String;
      Value : Integer;
      Unit : Length_Unit;
      Value_Valid : Boolean := False;
   begin
      if To_Validate.Hgt_Valid then
         Parser_Loop:
         for I in 1 .. Length(To_Validate.Hgt) loop
            Curr_Char := Element(To_Validate.Hgt, I);
            if Curr_Char = 'i' then
               if Element(To_Validate.Hgt, I+1) /= 'n' then
                  return False;
               end if;
               Unit := inch;
               exit Parser_Loop;
            elsif Curr_Char = 'c' then
               if Element(To_Validate.Hgt, I+1) /= 'm' then
                  return False;
               end if;
               Unit := cm;
               exit Parser_Loop;
            else
               Buffer := Buffer & Curr_Char;
            end if;
         end loop Parser_Loop;

         Value := Integer'Value(To_String(Buffer));
         case Unit is
            when cm => Value_Valid := Value >= 150 and then Value <= 193;
            when inch => Value_Valid := Value >=59 and then Value <= 76;
         end case;

         return Length(To_Validate.Hgt) = (Length(Buffer) + 2) -- Unit occupies 2 characters
           and then Value_Valid;
      else
         return True; -- Assume missing data as valid, because existence is checked somewhere else
      end if;
   exception
      when others => return False;
   end Validate_Hgt;

   function Validate_Hcl(To_Validate : in Document) return Boolean is
      Curr_Char : Character;
   begin
      if To_Validate.Hcl_Valid then
         if Length(To_Validate.Hcl) = 7 then
            if Element(To_Validate.Hcl, 1) = '#' then
               for I in 2 .. 7 loop
                  Curr_Char := Element(To_Validate.Hcl, I);
                  if Curr_Char /= '0' and then
                    Curr_Char /= '1' and then
                    Curr_Char /= '2' and then
                    Curr_Char /= '3' and then
                    Curr_Char /= '4' and then
                    Curr_Char /= '5' and then
                    Curr_Char /= '6' and then
                    Curr_Char /= '7' and then
                    Curr_Char /= '8' and then
                    Curr_Char /= '9' and then
                    Curr_Char /= 'a' and then
                    Curr_Char /= 'b' and then
                    Curr_Char /= 'c' and then
                    Curr_Char /= 'd' and then
                    Curr_Char /= 'e' and then
                    Curr_Char /= 'f'
                  then
                     return False; -- Invalid character found
                  end if;
               end loop;
               return True; -- No issues found
            else
               return False; -- First character is not #
            end if;
         else
            return False; -- Length is incorrect
         end if;
      else
         return True; -- Assume missing data as valid, because existence is checked somewhere else
      end if;
   end Validate_Hcl;

   function Validate_Ecl(To_Validate : in Document) return Boolean is
      Passport_Color : String (1..3);
   begin
      if To_Validate.Ecl_Valid then
         Passport_Color := To_String(To_Validate.Ecl);
         return Passport_Color = "amb" or else
           Passport_Color = "blu" or else
           Passport_Color = "brn" or else
           Passport_Color = "gry" or else
           Passport_Color = "grn" or else
           Passport_Color = "hzl" or else
           Passport_Color = "oth";
      else
         return True; -- Assume missing data as valid, because existence is checked somewhere else
      end if;
   exception
      when others => return False;
   end Validate_Ecl;

   function Validate_Pid(To_Validate : in Document) return Boolean is
      Pid_Number : Integer;
   begin
      if To_Validate.Pid_Valid then
         Pid_Number := Integer'Value(To_String(To_Validate.Pid)); -- Fails if not a number
         return Length(To_Validate.Pid) = 9;
      else
         return True; -- Assume missing data as valid, because existence is checked somewhere else
      end if;
   exception
      when others => return False;
   end Validate_Pid;

   function Validate_Cid(To_Validate : in Document) return Boolean is
   begin
      return True;
   end Validate_Cid;

   function Validate_Document(To_Validate : in Document)
                              return Boolean is
      Data_Present : Boolean;
      Data_Valid : Boolean := False;
   begin
      Data_Present := To_Validate.Byr_Valid and then
        To_Validate.Iyr_Valid and then
        To_Validate.Eyr_Valid and then
        To_Validate.Hgt_Valid and then
        To_Validate.Hcl_Valid and then
        To_Validate.Ecl_Valid and then
        To_Validate.Pid_Valid;

      if Data_Present then
         Log_Debug("All data present, checking consistency");
         Data_Valid := Validate_Byr(To_Validate) and then
           Validate_Iyr(To_Validate) and then
           Validate_Eyr(To_Validate) and then
           Validate_Hgt(To_Validate) and then
           Validate_Hcl(To_Validate) and then
           Validate_Ecl(To_Validate) and then
           Validate_Pid(To_Validate) and then
           Validate_Cid(To_Validate);
      end if;

      if Data_Valid then
         Log_Debug("Passport with Pid " & To_String(To_Validate.Pid) & " is valid!");
      else
         Log_Debug("Passport with Pid " & To_String(To_Validate.Pid) & " is invalid!");
      end if;

      return Data_Valid;
   end Validate_Document;

   Documents : Administration := (others => Create_New_Document);
   Answer : Integer := 0;
begin
   Load_File(Documents);

   for I in Documents'Range loop
      if Validate_Document(Documents(I)) then
         Answer := Answer + 1;
      end if;
   end loop;

   Put_line("The number of valid passwords is " & Integer'Image(Answer));
end Day04_2;
