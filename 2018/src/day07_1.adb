with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
procedure Day07_1 is
   Data_Line_Amount : constant Integer := 101; -- Number of lines in the dataset
   Blocker_Pos : constant Integer := 6;
   Waiter_Pos : constant Integer := 37;

   subtype Data_Line is Unbounded_String;
   subtype Alphabet is Character range 'A' .. 'Z';
   subtype Alphabet_String is String (1 .. 26);

   type Constraint_Record is record
      Blocker : Character;
      Victim : Character;
      Valid : Boolean;
   end record;
   
   type Data_Array is array (1 .. Data_Line_Amount) of Constraint_Record;
   type Admin_Array is array (Alphabet) of Boolean;

   procedure Load_File (Data : out Data_Array) is
      Current_Line   : Data_Line;
      Current_Record : Constraint_Record;
      Input          : File_Type;
      Row_Idx        : Integer := 0;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day07.input");
      while not End_Of_File (Input) loop
         Current_Line   := To_Unbounded_String (Get_Line (Input));
         Current_Record := (Blocker => Element(Current_Line, Blocker_Pos),
                            Victim => Element(Current_Line, Waiter_Pos),
                            Valid => True);
         Row_Idx        := Row_Idx + 1;
         Data (Row_Idx) := Current_Record;
      end loop;
      Close (File => Input);
   end Load_File;
   
   function Contains (Haystack : Alphabet_String; Needle : Character) return Boolean is
   begin
      for C in Haystack'Range loop
         if Haystack(C) = Needle then
            return true;
         end if;
      end loop;
      return False;
   end Contains;
   
   -- Fill Answer with the next candidate(s).
   -- Returns True if all characters are used.
   function Get_Next_Candidate(Input_Data : in out Data_Array;
                               Result : in out Alphabet_String) return Boolean is
      -- All constraints: Input_Data
      -- To keep track of all used characters: Administration
      -- To get the intermediate list of characters so far: Result
      Next_Candidate : Character;
      Current_Record : Constraint_Record;
      Potential : Boolean;
      Free_Idx : Integer := 1;
   begin
      for C in Alphabet loop
         Potential := not Contains(Result, C);
         
         for R in Input_Data'Range loop
            Current_Record := Input_Data(R);
            if (C = Current_Record.Victim and Current_Record.Valid) then
               --Put_Line(Character'Image(C) & " is not a candidate");
               Potential := False; -- C is not a candidate
            end if;
         end loop;
         if Potential then
            Next_Candidate := C;

            -- Clear all records that are blocked by C
            Clear :
            for R in Input_Data'Range loop
               if Input_Data(R).Blocker = C then
                  Input_Data(R).Valid := False;
                  --Put_Line("Invalidating " & Character'Image(Input_Data(R).Blocker) & " with " & Character'Image(Input_Data(R).Victim));
               end if;
            end loop Clear;
         end if;
         exit when Potential;
      end loop;
      
      Dermine_Next_Index:
      for Idx in Result'Range loop
         if Result(Idx) = ' ' then
            Free_Idx := Idx;
            exit Dermine_Next_Index;
         end if;
      end loop Dermine_Next_Index;
      
      Result(Free_Idx) := Next_Candidate;
      
      for C in Input_Data'Range loop
         if Input_Data(C).Valid then
            return False;
         end if;
      end loop;
      -- TODO: Add last character
      return True;
   end Get_Next_Candidate;
   
   
   Input_Data   : Data_Array;
   Administration : Admin_Array := (others => True);
   Answer : Alphabet_String := (others => ' ');
   
   Found : Boolean := False;
   
   Test : Alphabet_String := (others => 'X');
begin
   Load_File(Data => Input_Data);
   
   Infinite_Loop :
   loop
      exit Infinite_Loop when Get_Next_Candidate(Input_Data => Input_Data, Result => Answer);
   end loop Infinite_Loop;
   
   -- Add the missing characters
   Outer:
   for C in Character range 'A' .. 'Z' loop
      Found := False;
      Inner:
      for I in Answer'Range loop
         if Answer(I) = C then
            Found := True;
            exit Inner;
         end if;
      end loop Inner;
      if not Found then
         Answer(Answer'Length) := C;
      end if;
   end loop Outer;
   
   Put_Line("Final result: " & Answer);
end Day07_1;
