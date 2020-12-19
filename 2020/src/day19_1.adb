with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Vectors;     use Ada.Containers;
with GNAT.Regexp;                use GNAT.Regexp;

procedure Day19_1 is
   -- ### CONSTANTS ### --
   Enable_Debug    : constant Boolean := False;
   Number_Of_Rules : constant Integer := 138;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Parser_State is (R, M);

   type Rule;
   type Rule_Access is access Rule;
   -- If Char is 'a' or 'b', then the Constraints are ignored
   -- Constraint 2 is optional
   type Rule is record
      Char               : Character;
      Constraint_1_First : Rule_Access;
      Constraint_1_Last  : Rule_Access;
      Constraint_2_First : Rule_Access;
      Constraint_2_Last  : Rule_Access;
   end record;

   function Natural_Hash (Id : Natural) return Hash_Type is
   begin
      return Hash_Type(Id);
   end Natural_Hash;

   package Rules_Map is new Hashed_Maps (Key_Type        => Natural,
                                         Element_Type    => Rule_Access,
                                         Hash            => Natural_Hash,
                                         Equivalent_Keys => "=");
   use     Rules_Map;

   package Message_Vector is new Vectors (Index_Type   => Natural,
                                          Element_Type => Unbounded_String);
   use     Message_Vector;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   -- Returns the references for an input in the format "NM XY" or "NM"
   procedure Parse_Rule_String(Input : in Unbounded_String;
                               Rules : in Rules_Map.Map;
                               Ref_1 : out Rule_Access;
                               Ref_2 : out Rule_Access) is
      Separator_Index : Natural;
      Part_1 : Unbounded_String;
      Part_2 : Unbounded_String;
      Id_1 : Natural;
      Id_2 : Natural;
   begin
      --Log_Debug("Parsing " & To_String(Input));
      Separator_Index := Index(Input, " ");

      if Separator_Index > 0 then
         Part_1 := To_Unbounded_String(Slice(Input, 1, Separator_Index - 1));
         --Log_Debug("Part 1 is " & To_String(Part_1));
         Id_1 := Natural'Value(To_String(Part_1));
         Ref_1 := Rules(Id_1);

         Part_2 := To_Unbounded_String(Slice(Input, 1, Separator_Index - 1));
         --Log_Debug("Part 2 is " & To_String(Part_2));
         Id_2 := Natural'Value(Slice(Input, Separator_Index + 1, Length(Input)));
         Ref_2 := Rules(Id_2);
      else
         Id_1 := Natural'Value(To_String(Input));
         Ref_1 := Rules(Id_1);
      end if;
   end Parse_Rule_String;

   procedure Add_Rule(Input : in Unbounded_String;
                      Rules : in out Rules_Map.Map) is
      Rule_Index  : Natural;
      Colon_Index : Natural;
      Quote_Index : Natural;
      Pipe_Index  : Natural;

      Input_Value  : Unbounded_String;
      Current_Rule : Unbounded_String;

      Ref_1       : Rule_Access;
      Ref_2       : Rule_Access;
   begin
      Colon_Index := Index(Input, ":");
      Rule_Index := Natural'Value(Slice(Input, 1, Colon_Index - 1));
      Input_Value := To_Unbounded_String(Slice(Input, Colon_Index + 2, Length(Input)));

      Quote_Index := Index(Input, """");
      if Quote_Index > 0 then
         -- This is a character
         Rules(Rule_Index).Char := Element(Input, Quote_Index + 1);
      else
         Pipe_Index := Index(Input, "|");
         if Pipe_Index > 0 then
            -- This is a rule with two options
            Current_Rule := To_Unbounded_String(Slice(Input, Colon_index + 2, Pipe_Index - 2));
            Parse_Rule_String(Current_Rule, Rules, Ref_1, Ref_2);
            Rules(Rule_Index).Constraint_1_First := Ref_1;
            Rules(Rule_Index).Constraint_1_Last := Ref_2;

            Current_Rule := To_Unbounded_String(Slice(Input, Pipe_Index + 2, Length(Input)));
            Parse_Rule_String(Current_Rule, Rules, Ref_1, Ref_2);
            Rules(Rule_Index).Constraint_2_First := Ref_1;
            Rules(Rule_Index).Constraint_2_Last := Ref_2;
         else
            -- This is a rule with one option
            Parse_Rule_String(Input_Value, Rules, Ref_1, Ref_2);
            Rules(Rule_Index).Constraint_1_First := Ref_1;
            Rules(Rule_Index).Constraint_1_Last  := Ref_2;
         end if;
      end if;
   end Add_Rule;

   procedure Add_Message(Input : in Unbounded_String;
                         Messages : in out Message_Vector.Vector) is
   begin
      Messages.Append(Input);
   end Add_Message;

   procedure Load_File (Rules    : out Rules_Map.Map;
                        Messages : out Message_Vector.Vector) is
      Input        : File_Type;
      Current_Line : Data_Line;
      State        : Parser_State := R;
   begin
      -- First, fill the Map with nulls
      Fill_Map_Loop:
      for I in 0 .. Number_Of_Rules - 1 loop
         Rules.Insert(I, new Rule'(Char               => '.',
                                   Constraint_1_First => null,
                                   Constraint_1_Last  => null,
                                   Constraint_2_First => null,
                                   Constraint_2_Last  => null));
      end loop Fill_Map_Loop;

      Open (File => Input, Mode => In_File, Name => "data/day19.input");
      Read_Loop:
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         if Current_Line = To_Unbounded_String("") then
            State := M;
         else
            case State is
               when R =>
                  Add_Rule(Current_Line, Rules);
               when M =>
                  Add_Message(Current_Line, Messages);
            end case;
         end if;
      end loop Read_Loop;
      Close (File => Input);
   end Load_File;

   -- Converts the rule to a regex pattern
   function To_Regex_String(Rule  : Rule_Access;
                            Rules : Rules_Map.Map) return Unbounded_String is
   begin
      if Rule.Char = 'a' or else Rule.Char = 'b' then
         -- Single character
         return To_Unbounded_String((1 => Rule.Char));
      elsif Rule.Constraint_1_Last = null then
         if Rule.Constraint_2_First = null then
            -- N (Single rule)
            return To_Unbounded_String(To_String(
                                       To_Regex_String(Rule.Constraint_1_First, Rules)
                                      ));
         else
            -- N|X (Two single rules)
            return To_Unbounded_String("(" & To_String(
                                       To_Regex_String(Rule.Constraint_1_First, Rules)
                                       & "|" & To_Regex_String(Rule.Constraint_2_First, Rules) & ")"
                                      ));
         end if;
      elsif Rule.Constraint_2_First = null then
         -- NM (Two subsequent rules)
         return To_Unbounded_String(To_String(
                                    To_Regex_String(Rule.Constraint_1_First, Rules)
                                    & To_Regex_String(Rule.Constraint_1_Last, Rules)
                                   ));
      elsif Rule.Constraint_2_Last = null then
         -- This case is not in my input
         -- NM|X
         return To_Unbounded_String("(" & To_String(
                                    To_Regex_String(Rule.Constraint_1_First, Rules)
                                    & To_Regex_String(Rule.Constraint_1_Last, Rules)
                                   ) & "|" & To_String(
                                      To_Regex_String(Rule.Constraint_2_First, Rules)
                                     ) & ")");
      else
         -- NM|XY
         return To_Unbounded_String("(" & To_String(
                                    To_Regex_String(Rule.Constraint_1_First, Rules)
                                    & To_Regex_String(Rule.Constraint_1_Last, Rules)
                                   ) & "|" & To_String(
                                    To_Regex_String(Rule.Constraint_2_First, Rules)
                                    & To_Regex_String(Rule.Constraint_2_Last, Rules)
                                   ) & ")");
      end if;
   end To_Regex_String;


   Rules      : Rules_Map.Map;
   Messages   : Message_Vector.Vector;
   Regex      : Unbounded_String;
   Regex_Pattern : Regexp;

   Answer     : Integer := 0;
begin
   Load_File(Rules, Messages);

   Regex := To_Regex_String(Rules(0), Rules);
   Log_Debug(To_String(Regex));
   Regex_Pattern := Compile(To_String(Regex));

   Validation_Loop:
   for M of Messages loop
      if Match(To_String(M), Regex_Pattern) then
         Log_Debug(To_String(M) & " is valid.");
         Answer := Answer + 1;
      else
         Log_Debug(To_String(M) & " is invalid.");
      end if;
   end loop Validation_Loop;

   Put_line("The number of valid messages is " & Integer'Image(Answer)); --53 and 57 wrong
end Day19_1;
