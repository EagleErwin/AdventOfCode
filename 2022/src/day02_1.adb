with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day02_1 is
   Input_Size : constant Integer := 2500;

   type His_Pick is (A, B, C);
   type My_Pick is (X, Y, Z);

   type Round is record
      His: His_Pick;
      Mine: My_Pick;
   end record;

   type Game is array (1 .. Input_Size) of Round;

   procedure Load_File (Turns : out Game) is
      Input          : File_Type;
      Current_Line   : Unbounded_String;
      Current_Round  : Round;
      He             : His_Pick;
      Me             : My_Pick;
      Turn_Index     : Integer := 0;

   begin
      Turns := (others => (A, X));
      Open (File => Input, Mode => In_File, Name => "data/day02.input");
      while not End_Of_File (Input) loop
         Turn_Index := Turn_Index + 1;
         Current_Line := To_Unbounded_String(Get_Line(Input));

         He := His_Pick'Value((1 => Element(Current_Line, 1)));
         Me := My_Pick'Value((1 => Element(Current_Line, 3)));

         Current_Round := (His => He, Mine => Me);
         Turns(Turn_Index) := Current_Round;
      end loop;
      Close (File => Input);
   end Load_File;

   function Play_Round (Turn : in Round) return Integer is
      Score : Integer := 0;
   begin
      case Turn.His is
         when A => --Rock
            case Turn.Mine is
               when X => Score := Score + 1 + 3;
               when Y => Score := Score + 2 + 6;
               when Z => Score := Score + 3 + 0;
            end case;
         when B => --Paper
            case Turn.Mine is
               when X => Score := Score + 1 + 0;
               when Y => Score := Score + 2 + 3;
               when Z => Score := Score + 3 + 6;
            end case;
         when C => --Scissors
            case Turn.Mine is
               when X => Score := Score + 1 + 6;
               when Y => Score := Score + 2 + 0;
               when Z => Score := Score + 3 + 3;
            end case;
      end case;
      return Score;
   end Play_Round;

   function Play (Turns : in Game) return Integer is
      Score : Integer := 0;
   begin
      Play_Loop:
      for I in Turns'Range loop
         Score := Score + Play_Round(Turns(I));
      end loop Play_Loop;
      return Score;
   end Play;

   Turns : Game;
   Total_Score : Integer := 0;
begin
   Load_File(Turns);

   Total_Score := Play(Turns);

   Put_Line("My total score is " & Integer'Image(Total_Score));
end Day02_1;
