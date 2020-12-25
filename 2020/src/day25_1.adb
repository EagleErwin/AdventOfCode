with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;     use Ada.Containers;
with Ada.Containers.Hashed_Sets; use Ada.Containers;
with Interfaces;                 use Interfaces;

procedure Day25_1 is
   -- ### CONSTANTS ### --
   Enable_Debug     : constant Boolean := False;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   procedure Load_File (Pubkey_Card : out Integer;
                        Pubkey_Door : out Integer) is
      Input        : File_Type;
      Current_Line : Data_Line;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day25.input");

      Pubkey_Card := Integer'Value(Get_Line(Input));
      Pubkey_Door := Integer'Value(Get_Line(Input));

      Close (File => Input);
   end Load_File;

   function Calculate_Value (Start_Value : in Integer;
                             Subject : in Integer) return Integer is
      Step_1 : Long_Integer;
   begin
      Step_1 := Long_Integer(Start_Value) * Long_Integer(Subject);
      return Integer(Step_1 mod 20201227);
   end Calculate_Value;

   function Find_Loop_Size(Pubkey : in Integer) return Integer is
      Start_Value : Integer := 1;
      Subject_Number : Integer := 7;
      Count : Integer := 0;
   begin
      Find_Loop:
      while Pubkey /= Start_Value loop
         Count := Count + 1;
         Start_Value := Calculate_Value(Start_Value, Subject_Number);
         --Log_Debug(Integer'Image(Start_Value));
      end loop Find_Loop;

      return Count;
   end Find_Loop_Size;

   function Transform(Subject_Number : in Integer;
                      Loop_Size : in Integer) return integer is
      Result : Integer := 1;
   begin
      Calculate_Loop:
      for I in 1 .. Loop_Size loop
         Result := Calculate_Value(Result, Subject_Number);
      end loop Calculate_Loop;

      return Result;
   end Transform;

   function Calculate_Encryption_Key(Loop_Size : Integer;
                                     Pubkey : Integer) return Integer is
   begin
      return Transform(Pubkey, Loop_Size);
   end Calculate_Encryption_Key;

   Pubkey_Card : Integer;
   Pubkey_Door : Integer;

   Loop_Size_Card : Integer;
   Loop_Size_Door : Integer;

   Encryption_Key_Card : Integer;
   Encryption_Key_Door : Integer;

   Answer : Integer := 0;
begin
   Load_File(Pubkey_Card, Pubkey_Door);

   Log_Debug("Pubkey Card: " & Integer'Image(Pubkey_Card));
   Log_Debug("Pubkey Door: " & Integer'Image(Pubkey_Door));

   Loop_Size_Card := Find_Loop_Size(Pubkey_Card);
   Loop_Size_Door := Find_Loop_Size(Pubkey_Door);

   Log_Debug("Loop Size Card: " & Integer'Image(Loop_Size_Card));
   Log_Debug("Loop Size Door: " & Integer'Image(Loop_Size_Door));

   Encryption_Key_Card := Calculate_Encryption_Key(Loop_Size_Door, Pubkey_Card);
   Encryption_Key_Door := Calculate_Encryption_Key(Loop_Size_Card, Pubkey_Door);
   Log_Debug("Card: " & Integer'Image(Encryption_Key_Card));
   Log_Debug("Door: " & Integer'Image(Encryption_Key_Door));

   if Encryption_Key_Card /= Encryption_Key_Door then
      Put_Line("ERROR: Encryption keys do not match");
   end if;

   Answer := Encryption_Key_Card;

   Put_line("The encryption key is " & Integer'Image(Answer));
end Day25_1;
