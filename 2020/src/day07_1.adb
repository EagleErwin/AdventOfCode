with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day07_1 is
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

   function Color_Already_Found(Colors : in Color_Array;
                                Color : in Unbounded_String) return Boolean is
   begin
      for C of Colors loop
         if C = Color then
            return True;
         end if;
      end loop;
      return False;
   end Color_Already_Found;

   function Put_Color_In_Array(Colors : in out Color_Array;
                                Color : in Unbounded_String) return Boolean is
   begin
      Insert_Loop:
      for I in Colors'Range loop
         if Colors(I) = Color then
            return False; -- Color was already in
         elsif Colors(I) = To_Unbounded_String("") then
            Log_Debug("Adding color " & To_String(Color));
            Colors(I) := Color;
            return True; -- Color added
         end if;
      end loop Insert_Loop;
      Put_Line("This should neven happen!");
      return False;
   end Put_Color_In_Array;

   Rules : Input_Data;

   Answer_Colors : Color_Array := (others => To_Unbounded_String(""));
   Answer : Integer := 0;
   New_Color_Added : Boolean := True;
begin
   Load_File(Rules);

   Answer_Colors(1) := To_Unbounded_String("shiny gold");

   Infinite_Loop:
   while New_Color_Added loop
      New_Color_Added := False;
      Rule_Loop:
      for R of Rules loop
         if R /= null then
            Content_Loop:
            for C of R.Contents loop
               if C.Amount = 0 then
                  exit Content_Loop;
               else
                  if Color_Already_Found(Answer_Colors, C.Bag_Color) then
                     New_Color_Added := New_Color_Added or
                       Put_Color_In_Array(Answer_Colors, R.Color);
                  end if;
               end if;
            end loop Content_Loop;
         end if;
      end loop Rule_Loop;
   end loop Infinite_Loop;

   -- Count the number of colors in Answer_Colors;
   Count_Loop:
   for A of Answer_Colors loop
      if To_String(A) /= "" then
         Answer := Answer + 1;
      end if;
   end loop Count_Loop;

   Answer := Answer - 1; -- The shiny gold bag is in the Answer_Colors, but it does not contain shiny gold bags.
   Put_line("A total of " & Integer'Image(Answer)
            & " can eventually contain a shiny gold bag");
end Day07_1;
