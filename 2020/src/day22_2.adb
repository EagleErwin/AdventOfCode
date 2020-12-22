with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Containers.Vectors;     use Ada.Containers;
with Ada.Containers.Hashed_Sets; use Ada.Containers;

procedure Day22_2 is
   -- ### CONSTANTS ### --
   Enable_Debug     : constant Boolean := False;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   subtype Game_Result is Natural range 1 .. 2;

   package Card_Vector is new Vectors (Natural, Natural);
   use     Card_Vector;

   function Unbounded_String_Hash (Game : Unbounded_String) return Hash_Type is
   begin
      return Hash(To_String(Game));
   end Unbounded_String_Hash;

   package Administration_Set is new Hashed_Sets(Element_Type        => Unbounded_String,
                                                 Hash                => Unbounded_String_Hash,
                                                 Equivalent_Elements => "=");
   use Administration_Set;


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

   function Cards_To_Unbounded_String(Cards : in Card_Vector.Vector)
                                      return Unbounded_String is
      Result : Unbounded_String := To_Unbounded_String("");
   begin
      Card_Loop:
      for C of Cards loop
         Result := Result & Integer'Image(C);
      end loop Card_Loop;
      return Result;
   end Cards_To_Unbounded_String;

   -- Detects a loop. If no loop is detected, this game is added to the set.
   function Is_Loop(Player_1 : in out Card_Vector.Vector;
                    Player_2 : in out Card_Vector.Vector;
                    Played_Games : in out Administration_Set.Set) return Boolean is
      This_Game : Unbounded_String;
   begin
      This_Game := Cards_To_Unbounded_String(Player_1) & "-"
        & Cards_To_Unbounded_String(Player_2);
      if Played_Games.Contains(This_Game) then
         Log_Debug("Loop detected!");
         return True;
      else
         Played_Games.Insert(This_Game);
         return False;
      end if;
   end Is_Loop;

   -- Returns a sub-vector of the given Source with size Size
   function Sub_Vector(Source : in Card_Vector.Vector;
                       Size   : in Natural) return Card_Vector.Vector is
      Result : Card_Vector.Vector;
   begin
      Fill_Loop:
      for I in 0 .. Size - 1 loop
         Result.Append(Source(I));
      end loop Fill_Loop;
      return Result;
   end Sub_Vector;

   -- Returns the number of the player that won the round.
   function Play(Player_1     : in out Card_Vector.Vector;
                 Player_2     : in out Card_Vector.Vector;
                 Played_Games : in out Administration_Set.Set) return Game_Result is
      Card_1 : Integer;
      Card_2 : Integer;

      Player_1_Subgame       : Card_Vector.Vector;
      Player_2_Subgame       : Card_Vector.Vector;
      Subgame_Administration : Administration_Set.Set := Administration_Set.Empty_Set;
      Subgame_Result         : Game_Result;
   begin
      Play_Loop:
      while Player_1.Length /= 0 and then Player_2.Length /= 0 loop
         if Is_Loop(Player_1, Player_2, Played_Games) then
            -- Player 1 wins due to loop
            return 1;
         end if;

         Card_1 := Player_1(0);
         Card_2 := Player_2(0);

         Log_Debug("Cards: " & Integer'Image(Card_1) & " vs " & Integer'Image(Card_2));
         Player_1.Delete_First;
         Player_2.Delete_First;
         if Card_1 <= Integer(Player_1.Length) and then
           Card_2 <= Integer(Player_2.Length) then
            -- Play a subgame!
            Player_1_Subgame := Sub_Vector(Player_1, Card_1);
            Player_2_Subgame := Sub_Vector(Player_2, Card_2);
            Log_Debug("Playing subgame!");
            Subgame_Result := Play(Player_1_Subgame, Player_2_Subgame, Subgame_Administration);
            Log_Debug("Subgame finished! Player" & Game_Result'Image(Subgame_Result) & " won.");

            if Subgame_Result = 1 then
               Player_1.Append(Card_1);
               Player_1.Append(Card_2);
            else
               Player_2.Append(Card_2);
               Player_2.Append(Card_1);
            end if;
         else
            -- Play a regular game
            if Card_1 > Card_2 then
               Player_1.Append(Card_1);
               Player_1.Append(Card_2);
            elsif Card_2 > Card_1 then
               Player_2.Append(Card_2);
               Player_2.Append(Card_1);
            else
               Put_Line("ERROR: Cards are the same. This can not happen");
               return 1;
            end if;
         end if;
      end loop Play_Loop;

      if Player_1.Length = 0 then
         return 2;
      elsif Player_2.Length = 0 then
         return 1;
      else
         Put_Line("ERROR: Draw detected");
         return 1;
      end if;
   end Play;

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

   Player_1 : Card_Vector.Vector;
   Player_2 : Card_Vector.Vector;

   Played_Games : Administration_Set.Set := Administration_Set.Empty_Set;

   Winner : Game_Result;

   Answer   : Integer := 0;
begin
   Load_File(Player_1, Player_2);

   Winner := Play(Player_1, Player_2, Played_Games);

   if Player_1.Length = 0 then
      Log_Debug("Player 2 wins!");
      Answer := Calculate_Score(Player_2);
   else
      Log_Debug("Player 1 wins!");
      Answer := Calculate_Score(Player_1);
   end if;

   Put_line("The winning players score is " & Integer'Image(Answer));
end Day22_2;
