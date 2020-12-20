with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Vectors;     use Ada.Containers;
with GNAT.Regexp;                use GNAT.Regexp;

procedure Day20_1 is
   -- ### CONSTANTS ### --
   Enable_Debug    : constant Boolean := False;
   -- 12 * 12

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   function Natural_Hash (Id : Natural) return Hash_Type is
   begin
      -- I don't know how to calculate a Hash that makes more sense.
      return Hash(Integer'Image(id));
   end Natural_Hash;

   -- Used to store the mapping between border and tile ID
   package Border_Map is new Hashed_Maps (Key_Type        => Integer,
                                          Element_Type    => Natural,
                                          Hash            => Natural_Hash,
                                          Equivalent_Keys => "=");
   use     Border_Map;

   -- Used to store every possible border value
   package Border_Vector is new Vectors (Index_Type   => Natural,
                                         Element_Type => Integer);
   use     Border_Vector;

      -- Used to store the ID of every corner tile
   package Id_Vector is new Vectors (Index_Type   => Natural,
                                     Element_Type => Natural);
   use     Id_Vector;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   function Get_Value(Input : in Unbounded_String;
                      Rev   : in Boolean) return Integer is
      Parseable_String : Unbounded_String := To_Unbounded_String("2#");
      Curr_Char : Character;
   begin
      Replace_Characters_Loop:
      for I in 1 .. Length(Input) loop
         if Rev then
            Curr_Char := Element(Input, (Length(Input) + 1) - I);
         else
            Curr_Char := Element(Input, I);
         end if;

         if Curr_Char = '#' then
            Parseable_String := Parseable_String & '1';
         elsif Curr_Char = '.' then
            Parseable_String := Parseable_String & '0';
         else
            Put_Line("ERROR: Invalid character found");
         end if;
      end loop Replace_Characters_Loop;
      Parseable_String := Parseable_String & "#";

      return Integer'Value(To_String(Parseable_String));
   end Get_Value;

   procedure Add_To_Vector(Input : in Unbounded_String;
                           Borders : in out Border_Vector.Vector;
                           Id : in Natural;
                           Administration : in out Border_Map.Map) is
      Normal_Value  : Integer;
      Reverse_Value : Integer;
   begin
      -- Convert line to decimal (twice!)
      Normal_Value := Get_Value(Input, False);
      Reverse_Value := Get_Value(Input, True);

      -- Add number to Borders
      Borders.Append(Normal_Value);
      Borders.Append(Reverse_Value);

      -- Add records to Administration
      if not Administration.Contains(Normal_Value) then
         Administration.Insert(Normal_Value, Id);
      end if;

      if not Administration.Contains(Reverse_Value) then
         Administration.Insert(Reverse_Value, Id);
      end if;
   end Add_To_Vector;

   procedure Load_File (Borders : out Border_Vector.Vector;
                        Administration : out Border_Map.Map) is
      Input        : File_Type;
      Current_Line : Data_Line;

      Current_Id   : Natural := 0;
      Last_Line    : Unbounded_String;
      Left_Buffer  : Unbounded_String := To_Unbounded_String("");
      Right_Buffer : Unbounded_String := To_Unbounded_String("");
   begin
      Open (File => Input, Mode => In_File, Name => "data/day20.input");
      Read_Loop:
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         --Log_Debug("Reading line: " & To_String(Current_Line));
         if Current_Line = To_Unbounded_String("") then
            Add_To_Vector(Last_Line, Borders, Current_Id, Administration);
            Add_To_Vector(Left_Buffer, Borders, Current_Id, Administration);
            Add_To_Vector(Right_Buffer, Borders, Current_Id, Administration);

            Left_Buffer := To_Unbounded_String("");
            Right_Buffer := To_Unbounded_String("");
         elsif Element(Current_Line, 1) = 'T' then
            Current_Id := Natural'Value(Slice(Current_Line, 6, 9));
            -- Note: Extra Get_Line here!
            Current_Line := To_Unbounded_String(Get_Line(Input));
            Add_To_Vector(Current_Line, Borders, Current_Id, Administration);
            Left_Buffer := Left_Buffer & Element(Current_Line, 1);
            Right_Buffer := Right_Buffer & Element(Current_Line, Length(Current_Line));
         else
            Left_Buffer := Left_Buffer & Element(Current_Line, 1);
            Right_Buffer := Right_Buffer & Element(Current_Line, Length(Current_Line));
            Last_Line := Current_Line;
         end if;
      end loop Read_Loop;
      -- Don't forget the last one
      Add_To_Vector(Last_Line, Borders, Current_Id, Administration);
      Add_To_Vector(Left_Buffer, Borders, Current_Id, Administration);
      Add_To_Vector(Right_Buffer, Borders, Current_Id, Administration);

      Close (File => Input);
   end Load_File;

   function Occurs_Once(Borders : in Border_Vector.Vector;
                        Value   : in Integer) return Boolean is
      Num_Occurs : Integer := 0;
   begin
      Search_Loop:
      for B of Borders loop
         if B = Value then
            Num_Occurs := Num_Occurs + 1;
            if Num_Occurs > 1 then
               return False;
            end if;
         end if;
      end loop Search_Loop;
      return Num_Occurs = 1;
   end Occurs_Once;

   function Is_Corner(Tiles : in Id_Vector.Vector;
                      Value : in Natural) return Boolean is
      Num_Occurs : Integer := 0;
   begin
      Search_Loop:
      for T of Tiles loop
         if T = Value then
            Num_Occurs := Num_Occurs + 1;
         end if;
      end loop Search_Loop;
      return Num_Occurs > 2;
   end Is_Corner;

   function Extract_Corners(Borders : in Border_Vector.Vector;
                            Administration : in Border_Map.Map)
                            return Id_Vector.Vector is
      Side_Tiles : Id_Vector.Vector;
      Result     : Id_Vector.Vector;
   begin
      Find_Side_Tiles_Loop:
      for B of Borders loop
         if Occurs_Once(Borders, B) then
            Log_Debug(Integer'Image(B) & " occurs only once");
            Log_Debug(Natural'Image(Administration(B)) & " is an edge");
            Side_Tiles.Append(Administration(B));
         end if;
      end loop Find_Side_Tiles_Loop;

      Find_Corners_Loop:
      for S of Side_Tiles loop
         if Is_Corner(Side_Tiles, S) then
            Log_Debug(Natural'Image(S) & " is a corner");
            if not Result.Contains(S) then
               Result.Append(S);
            end if;
         end if;
      end loop Find_Corners_Loop;
      return Result;
   end Extract_Corners;

   Borders        : Border_Vector.Vector;
   Administration : Border_Map.Map;

   Corners        : Id_Vector.Vector;

   Answer         : Long_Integer := 1;
begin
   Load_File(Borders, Administration);

   Corners := Extract_Corners(Borders, Administration);

   Product_Loop:
   for C of Corners loop
      Answer := Answer * Long_Integer(C);
   end loop Product_Loop;

   Put_line("The product of the corder IDs " & Long_Integer'Image(Answer));
end Day20_1;
