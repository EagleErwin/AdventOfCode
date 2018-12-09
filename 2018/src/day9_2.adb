with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
procedure Day9_2 is
   Magic_Number : constant Integer := 23;
   Removal_Constant : constant Integer := 7;
   type Marble;
   type Marble_Access is access all Marble;
   type Marble is record
      Value : Integer;
      Pred : Marble_Access;
      Succ : Marble_Access;
   end record;

   type Score_Array is array (Natural range <>) of Long_Integer;
   
   function Get_Number_Of_Players (Input_String : Unbounded_String) return Integer is
      Current_Idx : Integer := 1;
      Buffer : Unbounded_String;
   begin
      Get_Character_Loop:
      while Element(Input_String, Current_Idx) /= ' ' loop
         Buffer := Buffer & Element(Input_String, Current_Idx);
         Current_Idx := Current_Idx + 1;
      end loop Get_Character_Loop;
      return Integer'Value(To_String(Buffer));
   end Get_Number_Of_Players;

   function Get_Value_Of_Last_Marble (Input_String : Unbounded_String) return Integer is
      Current_Idx : Integer;
      Buffer : Unbounded_String;
   begin
      Current_Idx := Index(Input_String, "h ") + 2; -- the 'h' in 'worth '
      Get_Character_Loop:
      while Element(Input_String, Current_Idx) /= ' ' loop
         Buffer := Buffer & Element(Input_String, Current_Idx);
         Current_Idx := Current_Idx + 1;
      end loop Get_Character_Loop;
      return Integer'Value(To_String(Buffer));
   end Get_Value_Of_Last_Marble;
   
   procedure Load_File (Nr_Of_Players : out Integer; Last_Marble : out Integer) is
      Input : File_Type;
      Data  : Unbounded_String;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day9.input");
      Data := To_Unbounded_String (Get_Line (Input));
      Close (File => Input);
      
      Nr_Of_Players := Get_Number_Of_Players(Data);
      Last_Marble := Get_Value_Of_Last_Marble(Data);
   end Load_File;
   
   procedure Print_Marbles (Current_Marble : in Marble_Access;
                            Last_Marble_Value : in Integer) is
      Printed_Marble : Marble_Access := Current_Marble;
   begin
      Put_Line("All marbles: ");
      for I in Integer range 1 .. Last_Marble_Value loop
         Put_Line(Integer'Image(Printed_Marble.Value));
         Printed_Marble := Printed_Marble.Succ;
      end loop;
      Put_Line("End");
   end Print_Marbles;
   
   -- Insert the marble with the given value in the correct position and return the score.
   function Insert_Marble (Value : in Integer; Current_Marble : in out Marble_Access) return Integer is
      New_Marble : Marble_Access := new Marble'(Value => Value, Pred => null, Succ => null);
   begin
      -- Check if we already have a circle. If not, create one.
      if Current_Marble.Pred = null and Current_Marble.Succ = null then
         New_Marble.Pred := Current_Marble;
         New_Marble.Succ := Current_Marble;
         
         Current_Marble.Pred := New_Marble;
         Current_Marble.Succ := New_Marble;
      else
         -- Fill the new Marble
         New_Marble.Pred := Current_Marble.Succ;
         New_Marble.Succ := Current_Marble.Succ.Succ;
      
         -- Update the predecessor
         New_Marble.Pred.Succ := New_Marble;
         -- Update the successor
         New_Marble.Succ.Pred := New_Marble;
      end if;
      
      -- Update the Current_Marble
      Current_Marble := New_Marble;
      
      return 0;
   end Insert_Marble;
   
   -- Insert the marble 7 positions back  from Current_Marble and return the score.
   function Remove_Marble (Current_Marble : in out Marble_Access) return Integer is
      Score : Integer;
   begin
      Find_Removal_Candidate:
      for I in Integer range 1 .. Removal_Constant loop
         Current_Marble := Current_Marble.Pred;
      end loop Find_Removal_Candidate;
      
      Score := Current_Marble.Value;
      -- Remove the Marble
      Current_Marble.Pred.Succ := Current_Marble.Succ;
      Current_Marble.Succ.Pred := Current_Marble.Pred;
      
      Current_Marble := Current_Marble.Succ;
      
      return Score;
   end Remove_Marble;
   
   -- Play the marble with the given value and return the score.
   function Play (Value : in Integer; Current_Marble : in out Marble_Access) return Integer is
   begin
      if Value mod Magic_Number = 0 then
         return Value + Remove_Marble(Current_Marble => Current_Marble);
      else
         return Insert_Marble(Value => Value, Current_Marble => Current_Marble);
      end if;
   end Play;
   
   Nr_Of_Players, Last_Marble_Value : Integer;
   Current_Marble : Marble_Access;
   
   Answer : Long_Integer := 0;
begin
   Load_File(Nr_Of_Players => Nr_Of_Players, Last_Marble => Last_Marble_Value);

   -- FOR TESTING
   --Nr_Of_Players := 30;
   --Last_Marble_Value := 5807;
   -- END FOR TESTING
   
   Last_Marble_Value := 100 * Last_Marble_Value;
   
   Current_Marble := new Marble'(Value => 0,
                                 Pred => null,
                                 Succ => null);
   
   The_Game:
   declare
      Scores : Score_Array(1 .. Nr_Of_Players) := (others => 0);
      Current_Player : Integer;
      Current_Score : Integer;
      
      Best_Score : Long_Integer := 0;
   begin
      for Turn in Integer range 1 .. Last_Marble_Value loop
         Current_Player := ((Turn - 1) mod Nr_Of_Players) + 1;

         Current_Score := Play (Value          => Turn,
                                Current_Marble => Current_Marble);
         Scores(Current_Player) := Scores(Current_Player) + Long_Integer(Current_Score);
      end loop;

      Determine_Winner_Score_Loop:
      for I in Scores'Range loop
         if  Scores(I) > Best_Score then
            Best_Score := Scores(I);
         end if;
      end loop Determine_Winner_Score_Loop;
      
      Answer := Best_Score;

      --Print_Marbles(Current_Marble, Last_Marble_Value);
   end The_Game;
   
   Put_Line("Final result:" & Long_Integer'Image(Answer));
end Day9_2;
