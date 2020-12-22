with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day22_1 is
   -- ### CONSTANTS ### --
   Enable_Debug     : constant Boolean := False;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   -- A vector of different ingredients
   package Card_Vector is new Vectors (Natural, Natural);
   use     Card_Vector;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   procedure Load_File (Player_1 : out Card_Vector.Vector;
                        Player_2 : out Card_Vector.Vector) is
      Input        : File_Type;
      Current_Line : Data_Line;

      Current_Player : Natural;
      Current_Value  : Integer;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day22.input");
      Read_Loop:
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         if Current_Line = To_Unbounded_String("Player 1:") then
            Current_Player := 1;
         elsif Current_Line = To_Unbounded_String("Player 2:") then
            Current_Player := 2;
         elsif Current_Line /= To_Unbounded_String("") then
            Current_Value := Integer'Value(To_String(Current_Line));
            case Current_Player is
               when 1 => Player_1.Append(Current_Value);
               when 2 => Player_2.Append(Current_Value);
               when others => Put_Line("ERROR: Invalid player number: "
                                       & Natural'Image(Current_Player));
            end case;
         end if;
      end loop Read_Loop;
   end Load_File;

   function Calculate_Score(Cards : in Card_Vector.Vector) return Integer is
      Result : Integer := 0;
      Index : Natural := 1;
   begin
      for Card in reverse Cards.Iterate loop
         Result := Result + (Index * Cards(Card));
         Index := Index + 1;
      end loop;
      return Result;
   end Calculate_Score;

   procedure Play(Player_1 : in out Card_Vector.Vector;
                  Player_2 : in out Card_Vector.Vector) is
      Card_1 : Integer;
      Card_2 : Integer;
   begin
      Card_1 := Player_1(0);
      Card_2 := Player_2(0);
      Player_1.Delete_First;
      Player_2.Delete_First;

      if Card_1 > Card_2 then
         Player_1.Append(Card_1);
         Player_1.Append(Card_2);
      elsif Card_2 > Card_1 then
         Player_2.Append(Card_2);
         Player_2.Append(Card_1);
      end if;
   end Play;

   Player_1 : Card_Vector.Vector;
   Player_2 : Card_Vector.Vector;

   Answer   : Integer := 0;
begin
   Load_File(Player_1, Player_2);

   Play_Loop:
   while Player_1.Length /= 0 and then Player_2.Length /= 0 loop
      Play(Player_1, Player_2);
   end loop Play_Loop;

   if Player_1.Length = 0 then
      Log_Debug("Player 2 wins!");
      Answer := Calculate_Score(Player_2);
   else
      Log_Debug("Player 1 wins!");
      Answer := Calculate_Score(Player_1);
   end if;

   Put_line("The winning players score is "
            & Integer'Image(Answer));
end Day22_1;
