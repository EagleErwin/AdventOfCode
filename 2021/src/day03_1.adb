with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day03_1 is
   -- ### CONSTANTS ### --
   Input_Length : constant Integer := 12;

   -- ### TYPE DEFINITIONS ### --
   type Administration is array (1 .. Input_Length) of Integer;
   type Bitmask is mod (2**Input_Length);

   procedure Process_Line(Input  : in     Unbounded_String;
                          Output : in out Administration) is
      Current_Char : Character;
      Current_Index : Integer := 0;
   begin
      for I in 1 .. Length(Input) loop
         Current_Index := Current_Index + 1;
         Current_Char := Element(Input, I);
         if (Current_Char = '1') then
            Output(Current_Index) := Output(Current_Index) + 1;
         elsif (Current_Char = '0') then
            Output(Current_Index) := Output(Current_Index) - 1;
         else
            Put_Line("Error! Invalid character found: " & Current_Char);
         end if;
      end loop;
   end Process_Line;

   Input          : File_Type;
   Current_Line   : Unbounded_String;
   Calculations   : Administration := (others => 0);

   Current_Val    : Integer;
   Result         : String := "............";

   Gamma          : Bitmask := 0;
   Epsilon        : Bitmask := 0;
   Answer         : Integer;
begin
   Open (File => Input, Mode => In_File, Name => "data/day03.input");
   while not End_Of_File (Input) loop
      Current_Line := To_Unbounded_String(Get_Line(Input));
      Process_Line(Current_Line, Calculations);
   end loop;
   Close (File => Input);

   for Idx in Calculations'Range loop
         Current_Val := Calculations(Idx);
         if (Current_Val > 0) then
            Result(Idx) := '1';
         elsif (Current_Val < 0) then
            Result(Idx) := '0';
         else
            Put_Line("Error! Equal number of occurences of 1 and 0");
         end if;
      end loop;

   Gamma := Bitmask'Value("2#" & Result & "#");
   Epsilon := not Gamma;

   Answer := Integer(Gamma) * Integer(Epsilon);

   Put_line("The answer is " & Integer'Image(Answer)
            & ", calculated by multiplying gamma "
            & Bitmask'Image(Gamma) & " and epsilon " & Bitmask'Image(Epsilon) & ".");
end Day03_1;
