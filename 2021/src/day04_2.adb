with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day04_2 is
   -- ### CONSTANTS ### --
   Number_Of_Numbers : constant Integer := 100;
   Card_Dimension : constant Integer := 5;
   Number_Of_Players : constant Integer := 100;

   -- ### TYPE DEFINITIONS ### --
   type Bingo_Number is new Natural range 0 .. Number_Of_Numbers - 1;
   type Card_Value is record
      Value  : Bingo_Number;
      Marked : Boolean;
   end record;
   type Card_Row is array (1 .. Card_Dimension) of Card_Value;
   type Card is array (1 .. Card_Dimension) of Card_Row;

   type Drawing is array (1 .. Number_Of_Numbers) of Bingo_Number;

   type Card_Outcome is record
      Player_Card : Card;
      Finished    : Boolean;
   end record;

   type Play_Field is array (1 .. Number_Of_Players) of Card_Outcome;

   function Get_Numbers(Input : in Unbounded_String) return Drawing is
      Current_Char   : Character;
      Current_Number : Unbounded_String := To_Unbounded_String("");
      Drawing_Index  : Integer := 1;
      Result : Drawing := (others => 0);
   begin
      Parser_Loop:
      for I in 1 .. Length(Input) loop
         Current_Char := Element(Input, I);
         if (Current_Char = ',') then
            Result(Drawing_Index) := Bingo_Number'Value(To_String(Current_Number));
            Current_Number := To_Unbounded_String("");
            Drawing_Index := Drawing_Index + 1;
         else
            Current_Number := Current_Number & Current_Char;
         end if;
      end loop Parser_Loop;
      -- Don't forget the last number
      Result(Drawing_Index) := Bingo_Number'Value(To_String(Current_Number));

      return Result;
   end Get_Numbers;

   function To_Card_Row(Input : in Unbounded_String) return Card_Row is
      Result         : Card_Row := (others => (0, False));
   begin
      Result(1).Value := Bingo_Number'Value(Slice(Input, 1, 2));
      Result(2).Value := Bingo_Number'Value(Slice(Input, 4, 5));
      Result(3).Value := Bingo_Number'Value(Slice(Input, 7, 8));
      Result(4).Value := Bingo_Number'Value(Slice(Input, 10, 11));
      Result(5).Value := Bingo_Number'Value(Slice(Input, 13, 14));

      return Result;
   end To_Card_Row;

   procedure Load_File (Numbers : out Drawing;
                        Cards   : out Play_Field) is
      Input        : File_Type;
      Current_Line : Unbounded_String;

      Player_Index     : Integer := 0;
      Current_Card     : Card;
   begin
      Cards := (others =>
                  ((others =>
                     (others =>
                          (0, False)
                     )
                  ), False)
               );
      Open (File => Input, Mode => In_File, Name => "data/day04.input");
      -- First line contains the numbers that are drawn
      Numbers := Get_Numbers(To_Unbounded_String(Get_Line(Input)));
      while not End_Of_File (Input) loop
         Player_Index := Player_Index + 1;
         -- Each card starts with an empty line
         Current_Line := To_Unbounded_String(Get_Line(Input));
         -- First row
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Current_Card(1) := To_Card_Row(Current_Line);
         -- Second row
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Current_Card(2) := To_Card_Row(Current_Line);
         -- Third row
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Current_Card(3) := To_Card_Row(Current_Line);
         -- Fourth row
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Current_Card(4) := To_Card_Row(Current_Line);
         -- Fifth row
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Current_Card(5) := To_Card_Row(Current_Line);

         Cards(Player_Index).Player_Card := Current_Card;
         Cards(Player_Index).Finished := False;
      end loop;
      Close (File => Input);
   end Load_File;

   procedure Play(Input : in out Card;
                  Value : in Bingo_Number) is
   begin
      --Put_Line("Playing " & Bingo_Number'Image(Value));
      Row_Loop:
      for R in Input'Range loop
         Column_Loop:
         for C in Input(R)'Range loop
            if (Input(R)(C).Value = Value) then
               Input(R)(C).Marked := True;
            end if;
         end loop Column_Loop;
      end loop Row_Loop;
   end Play;

   function Check (Input : in out Card_Outcome) return Boolean is
      Player_Card : Card := Input.Player_Card;
   begin
      -- Check for rows
      Row_Loop:
      for R in Player_Card'Range loop
         if Player_Card(R)(1).Marked and then
           Player_Card(R)(2).Marked and then
           Player_Card(R)(3).Marked and then
           Player_Card(R)(4).Marked and then
           Player_Card(R)(5).Marked then
            Input.Finished := True;
            return True;
         end if;
      end loop Row_Loop;

      -- Check for columns
      Column_Loop:
      for C in Player_Card(1)'Range loop
         if Player_Card(1)(C).Marked and then
           Player_Card(2)(C).Marked and then
           Player_Card(3)(C).Marked and then
           Player_Card(4)(C).Marked and then
           Player_Card(5)(C).Marked then
            Input.Finished := True;
            return True;
         end if;
      end loop Column_Loop;

      return False;
   end Check;

   function Draw (Numbers    : in     Drawing;
                  Cards      : in out Play_Field;
                  Draw_Index : in out Integer) return Integer is
      Current_Draw : Bingo_Number;
      Winner       : Integer := -1;
      Loser        : Integer := -1;
      Players_Left  : Integer := 0;
   begin
      Draw_Index := Draw_Index + 1;
      Current_Draw := Numbers(Draw_Index);

      Play_Loop:
      for I in Cards'Range loop
         Play(Cards(I).Player_Card, Current_Draw);
      end loop Play_Loop;

      Check_Loop:
      for J in Cards'Range loop
         if not Cards(J).Finished and then Check(Cards(J)) then
            Winner := J;
         end if;
      end loop Check_Loop;

      if (Winner /= -1) then
         -- Check if there are players left
         Player_Loop:
         for N in Cards'Range loop
            if not Cards(N).Finished then
               Players_Left := Players_Left + 1;
            end if;
         end loop Player_Loop;
         if Players_Left = 0 then
            Loser := Winner;
         end if;
      end if;
      return Loser;
   end Draw;

   function Calculate_Sum(Winner : in Card) return Integer is
      Sum : Integer := 0;
   begin
      Row_Loop:
      for R in Winner'Range loop
         Column_Loop:
         for C in Winner(R)'Range loop
            if not Winner(R)(C).Marked then
               Sum := Sum + Integer(Winner(R)(C).Value);
            end if;
         end loop Column_Loop;
      end loop Row_Loop;
      return Sum;
   end Calculate_Sum;

   Player_Cards   : Play_Field;
   Drawn_Numbers  : Drawing;
   Draw_Index     : Integer := 0;
   -- Player number of the winner. -1 if there is no winner.
   Winner : Integer := -1;

   Sum_Of_Unmarked_Numbers : Integer := 0;
   Last_Drawn_Number       : Bingo_Number := 0;

   Answer : Integer;
begin
   Load_File(Drawn_Numbers, Player_Cards);

   Draw_Loop:
   while (Winner = -1) loop
      Winner := Draw(Drawn_Numbers, Player_Cards, Draw_Index);
   end loop Draw_Loop;

   -- Calculate the answer
   Sum_Of_Unmarked_Numbers := Calculate_Sum(Player_Cards(Winner).Player_Card);
   Last_Drawn_Number := Drawn_Numbers(Draw_Index);

   Answer := Sum_Of_Unmarked_Numbers * Integer(Last_Drawn_Number);

   Put_line("The answer is " & Integer'Image(Answer)
            & ", calculated by multiplying sum of unmarked numbers "
            & Integer'Image(Sum_Of_Unmarked_Numbers)
            & " and last drawn number "
            & Bingo_Number'Image(Last_Drawn_Number) & ".");
end Day04_2;
