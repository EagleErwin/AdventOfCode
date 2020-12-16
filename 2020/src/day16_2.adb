with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;

procedure Day16_2 is
   -- ### CONSTANTS ### --
   Enable_Debug             : constant Boolean := False;
   Number_Of_Fields         : constant Integer := 20;
   Number_Of_Nearby_Tickets : constant Integer := 238;

   Part_1 : Integer := 0;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Parser_State is (Fields, My_Ticket, Nearby_Tickets);
   type Fields_Parser_State is (Field_Name, Range_1_Begin, Range_1_End,
                                Separator, Range_2_Begin, Range_2_End);

   type Int_Range is record
      Start : Integer;
      Stop  : Integer;
   end record;

   type Constraint is record
      Name : Unbounded_String;
      Field_Index : Integer;
      Range_1 : Int_Range;
      Range_2 : Int_Range;
   end record;

   type Constraint_Array is array (1 .. Number_Of_Fields) of Constraint;
   type Ticket is array (1 .. Number_Of_Fields) of Integer;
   type Ticket_Access is access Ticket;

   type Possible is array (1 .. Number_Of_Fields) of Boolean;
   type Mapping is array (1 .. Number_Of_Fields) of Possible;

   type Ticket_Array is array (1 .. Number_Of_Nearby_Tickets) of Ticket_Access;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   procedure Add_Allowed_Values(Input          : in     Unbounded_String;
                                Administration : in out Constraint_Array;
                                Index          : in     Integer) is
      Buffer : Unbounded_String := To_Unbounded_String("");
      Curr_Char : Character;
      State : Fields_Parser_State := Field_Name;
      Range_Begin : Integer;
      Range_End : Integer;
      Allowed_Range : Int_Range;
   begin
      Parser_Loop:
      for I in 1 .. Length(Input) loop
         Curr_Char := Element(Input, I);
         case State is
            when Field_Name =>
               if Curr_Char = ':' then
                  Administration(Index).Name := Buffer;
                  State := Range_1_Begin;
                  Buffer := To_Unbounded_String("");
               else
                  Buffer := Buffer & Curr_Char;
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
                  Allowed_Range := Int_Range'(Range_Begin, Range_End);
                  Administration(Index).Range_1 := Allowed_Range;
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
      Allowed_Range := Int_Range'(Range_Begin, Range_End);
      Administration(Index).Range_2 := Allowed_Range;
   end Add_Allowed_Values;

   function To_Ticket(Input : in Unbounded_String) return Ticket_Access is
      Result : Ticket_Access := new Ticket'(others => 0);
      Idx : Integer := 1;
      Buffer    : Unbounded_String := To_Unbounded_String("");
      Curr_Char : Character;
   begin
      Parser_Loop:
      for I in 1 .. Length(Input) loop
         Curr_Char := Element(Input, I);
         if Curr_Char = ',' then
            Result(Idx) := Integer'Value(To_String(Buffer));
            Idx := Idx + 1;
            Buffer := To_Unbounded_String("");
         else
            Buffer := Buffer & Curr_Char;
         end if;
      end loop Parser_Loop;
      -- Don't forget the last one
      Result(Idx) := Integer'Value(To_String(Buffer));
      return Result;
   end To_Ticket;

   procedure Load_File (My             : out Ticket;
                        Nearby         : out Ticket_Array;
                        Administration : out Constraint_Array) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 1;

      State : Parser_State := Fields;
      Buffer : Unbounded_String := To_Unbounded_String("");
   begin
      Administration := (others => Constraint'(Name    => To_Unbounded_String(""),
                                               Field_Index => 0,
                                               Range_1 => Int_Range'(0, 0),
                                               Range_2 => Int_Range'(0, 0)));
      Open (File => Input, Mode => In_File, Name => "data/day16.input");
      Read_Loop:
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         case State is
            when Fields =>
               if Current_Line = To_Unbounded_String("") then
                  State := My_Ticket;
               else
                  Add_Allowed_Values(Current_Line, Administration, Idx);
               end if;
            when My_Ticket =>
               if Current_Line = To_Unbounded_String("") then
                  State := Nearby_Tickets;
               elsif Current_Line /= To_Unbounded_String("your ticket:") then
                  My := To_Ticket(Current_Line).all;
               end if;
            when Nearby_Tickets =>
               if Current_Line = To_Unbounded_String("nearby tickets:") then
                  Idx := 0;
               else
                  Nearby(Idx) := To_Ticket(Current_Line);
               end if;
         end case;
         Idx := Idx + 1;
      end loop Read_Loop;
      Close (File => Input);
   end Load_File;

   function Is_In_Range(Value      : in Integer;
                        Limitation : in Constraint) return Boolean is
   begin
      return (Value >= Limitation.Range_1.Start and then Value <= Limitation.Range_1.Stop) or else
        (Value >= Limitation.Range_2.Start and then Value <= Limitation.Range_2.Stop);
   end Is_In_Range;

   function Validate_Ticket(Subject : in Ticket_Access;
                            Constraints : in Constraint_Array) return Boolean is
      Valid : Boolean;
   begin
      Number_Loop:
      for V of Subject.all loop
         Valid := False;
         Constraint_Loop:
         for C of Constraints loop
            if Is_In_Range(V, C) then
               Valid := True;
               exit Constraint_Loop;
            end if;
         end loop Constraint_Loop;
         if not Valid then
            Log_Debug("Invalid value found! " & Integer'Image(V));
            Part_1 := Part_1 + V;
            return False;
         end if;
      end loop Number_Loop;

      return True;
   end Validate_Ticket;

   function Reduce (Constraints : in out Constraint_Array;
                    Tickets     : in Ticket_Array;
                    Field_Mapping : in out Mapping) return Boolean is
      True_Count        : Integer := 0;
      Destination       : Integer;
      Option            : Boolean;
      Constraint_Record : Possible;
      Reduced : Boolean := False;
   begin
      Constraint_Loop:
      for Constraint_Idx in Field_Mapping'Range loop
         True_Count := 0;
         Constraint_Record := Field_Mapping(Constraint_Idx);
         Option_Loop:
         for I in Constraint_Record'Range loop
            Option := Constraint_Record(I);
            if Option then
               True_Count := True_Count + 1;
               Destination := I;
            end if;
         end loop Option_Loop;
         if True_Count = 1 then
            Log_Debug("Constraint "
                      & To_String(Constraints(Constraint_Idx).Name)
                      & " can only be field " & Integer'Image(Destination));
            Constraints(Constraint_Idx).Field_Index := Destination;
            -- Set this destination to False for all other constraints
            Reduction_Loop:
            for M of Field_Mapping loop
               M(Destination) := False;
            end loop Reduction_Loop;
            Reduced := True;
         end if;
      end loop Constraint_Loop;
      return Reduced;
   end Reduce;

   function Calculate_Answer(Constraints : in Constraint_Array;
                             Mine : in Ticket) return Long_Integer is
      Curr_Constraint : Constraint;
      Answer : Long_Integer := 1;
   begin
      Constraint_Loop:
      for C_I in Constraints'Range loop
         Curr_Constraint := Constraints(C_I);
         if Head(Curr_Constraint.Name, 3) = "dep" then
            Log_Debug("Adding " & Integer'Image(Mine(Curr_Constraint.Field_Index)));
            Answer := Answer * Long_Integer(Mine(Curr_Constraint.Field_Index));
         end if;
      end loop Constraint_Loop;
      return Answer;
   end Calculate_Answer;

   Mine          : Ticket;
   Theirs        : Ticket_Array;
   Constraints   : Constraint_Array;
   Field_Mapping : Mapping := (others => (others => True));

   Answer       : Long_Integer;

   Curr_Constraint : Constraint;
begin
   Load_File(Mine, Theirs, Constraints);

   Validation_Loop:
   for I in Theirs'Range loop
      if not Validate_Ticket(Theirs(I), Constraints) then
         Log_Debug("Invalidating ticket " & Integer'Image(I));
         Theirs(I) := new Ticket'(Mine); -- Prevernt null reference and take own ticket into account.
      end if;
   end loop Validation_Loop;

   Constraint_Loop:
   for C_I in Constraints'Range loop
      Curr_Constraint := Constraints(C_I);
      Field_Loop:
      for I in 1 .. Number_Of_Fields loop
         Ticket_Loop:
         for T of Theirs loop
            if not Is_In_Range(T(I), Curr_Constraint) then
               Log_Debug(To_String(Curr_Constraint.Name) & " is not " & Integer'Image(I));
               Field_Mapping(C_I)(I) := False;
            end if;
         end loop Ticket_Loop;
      end loop Field_Loop;
   end loop Constraint_Loop;

   Reduce_Loop:
   while Reduce(Constraints, Theirs, Field_Mapping) loop
      Log_Debug("Reducing...");
   end loop Reduce_Loop;

   Answer := Calculate_Answer(Constraints, Mine);

   Put_line("The product of the fields starting with 'departure' is "
            & Long_Integer'Image(Answer));
end Day16_2;
