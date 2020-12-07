with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day07_2 is
   -- ### CONSTANTS ### --
   Enable_Debug : constant Boolean := False;
   Number_Of_Inputs : constant Integer := 594;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Bag;
   type Bag_Access is access Bag;
   type Content is record
      Amount: Integer;
      Bag_Color: Unbounded_String;
   end record;

   type Content_Array is array (1 .. Number_Of_Inputs) of Content;
   type Bag is record
      Color: Unbounded_String;
      Contents: Content_Array;
   end record;

   type Input_Data is array (1 .. Number_Of_Inputs) of Bag_Access;
   type Color_Array is array (1 .. Number_Of_Inputs) of Unbounded_String;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   function To_Content(Input: in Unbounded_String) return Content is
      Curr_Amount : Integer;
      Curr_Color : Unbounded_String;
   begin
      --Log_Debug(To_String(Input));
      if To_String(Input) = "no other bags" then
         Curr_Amount := 0;
         Curr_Color := To_Unbounded_String("None");
      else
         Curr_Amount := Integer'Value((1 => Element(Input, 1)));
         Curr_Color := To_Unbounded_String(Slice(Source => Input,
                                            Low    => 3,
                                            High   => Index(Input, " bag")-1));
         --Log_Debug(To_String(Curr_Color));
      end if;

      return Content'(Amount    => Curr_Amount,
                      Bag_Color => Curr_Color);
   end To_Content;

   -- Parse the input text line and update the Input_Data
   function Parse_Line (Input : in Unbounded_String) return Bag_Access is
      Curr_Color : Unbounded_String;
      Cont_Color : Unbounded_String;
      Cont_Amount : Integer := 0;

      Contents : Content_Array;

      Buffer : Unbounded_String := To_Unbounded_String("");
      End_Curr_Color_Idx : Integer;
      Curr_Char : Character;
      Skip_Space : Boolean := False;
   begin
      Contents := (others => Content'(Amount    => 0,
                                      Bag_Color => To_Unbounded_String("None")));

      End_Curr_Color_Idx := Index(Input, " bags contain");
      Curr_Color := Head(Input, End_Curr_Color_Idx - 1);
      --      Log_Debug(To_String(Curr_Color));

      Contents_Loop:
      for I in (End_Curr_Color_Idx + 14) .. Length(Input) loop
         if Skip_Space then
            Skip_Space := False;
         else
            Curr_Char := Element(Input, I);
            if Curr_Char = ',' or else Curr_Char = '.' then
               Cont_Amount := Cont_Amount + 1;
               Contents(Cont_Amount) := To_Content(Buffer);

               Buffer := To_Unbounded_String("");
               Skip_Space := True;
            else
               Buffer := Buffer & Curr_Char;
            end if; -- End_of_Content_record
         end if; -- Skip_Space
      end loop Contents_Loop;

      return new Bag'(Color => Curr_Color,
                      Contents => Contents);
   end Parse_Line;

   procedure Load_File (Rules : out Input_Data) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 1;
   begin
      Rules := (others => null);
      Open (File => Input, Mode => In_File, Name => "data/day07.input");
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Rules(Idx) := Parse_Line(Current_Line);
         Idx := Idx + 1;
      end loop;
      Close (File => Input);
   end Load_File;

   function Get_Number_Of_Bags(Color : Unbounded_String;
                               Rules : Input_Data) return Integer is
      Curr_Color : Unbounded_String;
      Curr_Amount : Integer := 0;
      Amount : Integer := 0;
   begin
      Rules_Loop:
      for Rule of Rules loop
         if Rule = null then
            exit Rules_Loop;
         elsif Rule.Color = Color then
            Log_Debug("Checking color " & To_String(Color));
            Content_Loop:
            for C of Rule.Contents loop
               if C.Amount = 0 then
                  Log_Debug("Color " & To_String(Color) & " has no more contents.");
                  exit Content_Loop;
               else
                  Amount := Amount + (C.Amount * (1 + Get_Number_Of_Bags(C.Bag_Color, Rules)));
               end if;
            end loop Content_Loop;
            Log_Debug("Color " & To_String(Color) & " has " & Integer'Image(Amount) & " contents.");
         end if;
      end loop Rules_Loop;

      return Amount;
   end Get_Number_Of_Bags;

   Rules : Input_Data;
   Answer : Integer := 0;
begin
   Load_File(Rules);

   Answer := Get_Number_Of_Bags(To_Unbounded_String("shiny gold"), Rules);
   Put_line("You need a total of " & Integer'Image(Answer)
            & " individual bags");
end Day07_2;
