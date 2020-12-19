with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day18_1 is
   -- ### CONSTANTS ### --
   Enable_Debug : constant Boolean := False;
   Number_Of_Inputs : constant Integer := 377;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Node_Type is (Operator, Number);

   type Expression_Array is array (1 .. Number_Of_Inputs) of Unbounded_String;

   type Parser_State is (Number_1, Operator, Number_2);

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   procedure Load_File (Homework : out Expression_Array) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 1;
   begin
      Homework := (others => To_Unbounded_String(""));
      Open (File => Input, Mode => In_File, Name => "data/day18.input");
      Read_Loop:
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Homework(Idx) := Current_Line;
         Idx := Idx + 1;
      end loop Read_Loop;
      Close (File => Input);
   end Load_File;

   -- Applies the operation given by O on X and Y
   function Operate(X : in Long_Integer;
                    Y : in Long_Integer;
                    O : in Character) return Long_Integer is
   begin
      case O is
         when '+' => return X + Y;
         when '*' => return X * Y;
         when others => Put_Line("ERROR: Invalid operator found.");
      end case;
      return -1;
   end Operate;

   -- Calculate the operation, given that there are no parentheses in it
   function Calculate_No_Parentheses(Expression : in Unbounded_String)
                                     return Long_Integer is
      State : Parser_State := Number_1;
      Buffer : Unbounded_String := To_Unbounded_String("");
      Curr_Char : Character;

      N : Long_Integer;
      M : Long_Integer;
      O : Character;
   begin
      Log_Debug("Calculating " & To_String(Expression));
      Parser_Loop:
      for I in 1 .. Length(Expression) loop
         Curr_Char := Element(Expression, I);
         if Curr_Char = ' ' then
            case State is
               when Number_1 =>
                  N := Long_Integer'Value(To_String(Buffer));
                  State := Operator;
               when Operator =>
                  O := Element(Buffer, 1);
                  State := Number_2;
               when Number_2 =>
                  M := Long_Integer'Value(To_String(Buffer));
                  N := Operate(N, M, O);
                  State := Operator;
            end case;
            Buffer := To_Unbounded_String("");
         else
            Buffer := Buffer & Curr_Char;
         end if;
      end loop Parser_Loop;
      case State is
         when Number_1 => N := Long_Integer'Value(To_String(Buffer));
         when Operator =>
            Put_Line("ERROR: Invalid end state detected");
         when Number_2 =>
            -- Don't forget the last one
            M := Long_Integer'Value(To_String(Buffer));
            N := Operate(N, M, O);
      end case;
      Log_Debug("Calculated value " & Long_Integer'Image(N));
      return N;
   end Calculate_No_Parentheses;

   -- Returns an Unbounded_String with the reduced expression
   function Reduce_No_Parentheses(Expression : in Unbounded_String)
                                  return Unbounded_String is
   begin
      return Trim(To_Unbounded_String(Long_Integer'Image(Calculate_No_Parentheses(Expression))), Both);
   end Reduce_No_Parentheses;


   -- Return whether the given Expression has parenteses.
   -- If True, the Open_Idx and Close_Idx contain the indices of the outer pair
   function Has_Parentheses(Expression : in Unbounded_String;
                            Open_Idx   : out Integer;
                            Close_Idx  : out Integer) return Boolean is
      Number_Of_Opens : Integer := 0;
      Curr_Char : Character;
   begin
      Open_Idx := Index(Expression, "(");
      Close_Idx := 0;

      if Open_Idx <= 0 then
         return False;
      else
         Number_Of_Opens := 1;
         Find_Close_Parentheses_Loop:
         for I in Open_Idx + 1 .. Length(Expression) loop
            Curr_Char := Element(Expression, I);
            if Curr_Char = ')' then
               if Number_Of_Opens = 1 then
                  Close_Idx := I;
                  return True;
               else
                  Number_Of_Opens := Number_Of_Opens - 1;
               end if;
            elsif Curr_Char = '(' then
               Number_Of_Opens := Number_Of_Opens + 1;
            end if;
         end loop Find_Close_Parentheses_Loop;
         Put_Line("ERROR: Unable to find close parentheses after open.");
         return False;
      end if;
   end Has_Parentheses;

   -- Returns a new Expression that contains no parentheses
   function Replace_Parentheses(Expression : in Unbounded_String) return Unbounded_String is
      Open_Index  : Integer;
      Close_Index : Integer;

      First_Part : Unbounded_String; -- The part before the parentheses open
      Second_Part : Unbounded_String; -- The part between the parentheses
      Third_Part  : Unbounded_String; -- The part after the parentheses close

      Result : Unbounded_String := Expression;
   begin
      if Length(Expression) = 0 then
         return Expression;
      end if;
      Log_Debug("Replacing all parentheses in " & To_String(Expression));
      Remove_Parentheses_Loop:
      while Has_Parentheses(Result, Open_Index, Close_Index) loop
         First_Part := To_Unbounded_String(Slice(Result, 1, Open_Index - 1));
         Second_Part := To_Unbounded_String(Slice(Result, Open_Index + 1, Close_Index - 1));
         Third_Part := To_Unbounded_String(Slice(Result, Close_Index + 1, Length(Result)));

         Result := First_Part & Replace_Parentheses(Second_Part) & Third_Part;
      end loop Remove_Parentheses_Loop;

      return Reduce_No_Parentheses(Result);


   end Replace_Parentheses;

   function Calculate(Expression : in Unbounded_String) return Long_Integer is
      No_Parentheses : Unbounded_String;
   begin
      No_Parentheses := Replace_Parentheses(Expression);
      return Calculate_No_Parentheses(No_Parentheses);
   end;

   Homework      : Expression_Array;
   Current_Value : Long_Integer;
   Answer        : Long_Integer := 0;
begin
   Load_File(Homework);

   Summation_Loop:
   for Expression of Homework loop
      Current_Value := Calculate(Expression);
      Log_Debug(Long_Integer'Image(Current_Value));
      Answer := Answer + Current_Value;
   end loop Summation_Loop;

   Put_line("The sum of the resulting values is "
            & Long_Integer'Image(Answer));
end Day18_1;
