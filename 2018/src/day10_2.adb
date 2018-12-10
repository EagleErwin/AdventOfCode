with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
procedure Day10_2 is
   Number_Of_Input_Lines : constant Integer := 362;
   Max_Pixel_Coordinate : constant Integer := 60000;

   Max_Printable_Width : constant Integer := 64;
   Max_Printable_Height : constant Integer := 10;
   subtype Data_Line is Unbounded_String;

   type Pixel;
   type Access_Pixel is access all Pixel;

   type Pixel_Array is array (1 .. Number_Of_Input_Lines) of Access_Pixel;
   subtype Coordinate is Integer range -Max_Pixel_Coordinate .. Max_Pixel_Coordinate;
   type Canvas_Row is array (1 .. Max_Printable_Width) of Character;
   type Canvas is array (1 .. Max_Printable_Height) of Canvas_Row;

   type Pixel is record
      X   : Coordinate;
      Y   : Coordinate;
      V_X : Integer;
      V_Y : Integer;
   end record;

   function Get_Substring_As_Integer (Input : Unbounded_String;
      Start_Pos : Integer; Length : Integer) return Integer
   is
   begin
      return Integer'Value
          (Slice (Input, Start_Pos, (Start_Pos + Length - 1)));
   end Get_Substring_As_Integer;

   function Get_X (Input : Unbounded_String) return Coordinate is
      Start_Char_Pos : constant Integer := 11;
      Number_Length  : constant Integer := 6;
   begin
      return Get_Substring_As_Integer
        (Input  => Input,
         Start_Pos => Start_Char_Pos,
           Length => Number_Length);
   end Get_X;

   function Get_Y (Input : Unbounded_String) return Coordinate is
      Start_Char_Pos : constant Integer := 19;
      Number_Length  : constant Integer := 6;
   begin
      return Get_Substring_As_Integer
        (Input  => Input,
         Start_Pos => Start_Char_Pos,
           Length => Number_Length);
   end Get_Y;

   function Get_V_X (Input : Unbounded_String) return Integer is
      Start_Char_Pos : constant Integer := 37;
      Number_Length  : constant Integer := 2;
   begin
      return Get_Substring_As_Integer
        (Input  => Input,
         Start_Pos => Start_Char_Pos,
           Length => Number_Length);
   end Get_V_X;

   function Get_V_Y (Input : Unbounded_String) return Integer is
      Start_Char_Pos : constant Integer := 41;
      Number_Length  : constant Integer := 2;
   begin
      return Get_Substring_As_Integer
        (Input  => Input,
         Start_Pos => Start_Char_Pos,
           Length => Number_Length);
   end Get_V_Y;

   procedure Load_File (Data : out Pixel_Array) is
      Input         : File_Type;
      Current_Line  : Data_Line;
      Current_Pixel : Access_Pixel;
      Row_Idx       : Integer := 0;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day10.input");
      while not End_Of_File (Input) loop
         Current_Line  := To_Unbounded_String (Get_Line (Input));
         Current_Pixel :=
           new Pixel'
             (X   => Get_X (Current_Line), Y => Get_Y (Current_Line),
              V_X => Get_V_X (Current_Line), V_Y => Get_V_Y (Current_Line));
         Row_Idx        := Row_Idx + 1;
         Data (Row_Idx) := Current_Pixel;
      end loop;

      Close (File => Input);
   end Load_File;

   -- Return true if the positions are printable
   function Calculate_Positions (Input : in Pixel_Array;
                                 Output : out Pixel_Array;
                                 Timestamp : in Integer;
                                 Min_Height : in out Integer;
                                 Min_X : out Coordinate;
                                 Min_Y : out Coordinate) return Boolean is
      Current_X : Coordinate;
      Current_Y : Coordinate;
      Current_V_X : Integer;
      Current_V_Y : Integer;

      New_X : Coordinate;
      New_Y : Coordinate;

      Max_X : Coordinate := Coordinate'First;
      Max_Y : Coordinate := Coordinate'First;
   begin
      Min_X := Coordinate'Last;
      Min_Y := Coordinate'Last;

      for I in Input'Range loop
         Current_X := Input(I).X;
         Current_Y := Input(I).Y;
         Current_V_X := Input(I).V_X;
         Current_V_Y := Input(I).V_Y;
         New_X := Timestamp * Current_V_X + Current_X;
         New_Y := Timestamp * Current_V_Y + Current_Y;
         Output(I) := new Pixel'(X   => New_X,
                                 Y   => New_Y,
                                 V_X => Current_V_X,
                                 V_Y => Current_V_Y);
         if New_X < Min_X then
            Min_X := New_X;
         end if;
         if New_X > Max_X then
            Max_X := New_X;
         end if;
         if New_Y < Min_Y then
            Min_Y := New_Y;
         end if;
         if New_Y > Max_Y then
            Max_Y := New_Y;
         end if;
      end loop;

      if Min_Height < Max_Y - Min_Y then
         -- We are part the minimum, the correct one was the previous.
         Put_Line("Timestamp:" & Integer'Image(Timestamp - 1));
         return True;
      end if;
      Min_Height := Max_Y - Min_Y;
      return False;
   end Calculate_Positions;

   procedure Print_Output (Input : in Pixel_Array;
                           Min_X : in Coordinate;
                           Min_Y : in Coordinate) is
      Output : Canvas := (others => (others => ' '));
      Current_X : Coordinate;
      Current_Y : Coordinate;
   begin
      for I in Input'Range loop
         Current_X := Input(I).X - Min_X;
         Current_Y := Input(I).Y - Min_Y;
         Output(Current_Y + 1)(Current_X + 1) := '#';
      end loop;

      for R in Output'Range loop
         for C in Output(R)'Range loop
            Put(Output(R)(C));
         end loop;
         Put_Line("");
      end loop;

   end Print_Output;

   Input_Data : Pixel_Array;
   Output_Data : Pixel_Array;
   Current_Time : Integer := 1;

   Min_X, Min_Y : Coordinate;
   Min_Height : Integer := 2 * Max_Pixel_Coordinate;
begin
   Load_File (Data => Input_Data);

   while not Calculate_Positions(Input => Input_Data,
                                 Output => Output_Data,
                                 Timestamp => Current_Time,
                                 Min_Height => Min_Height,
                                 Min_X => Min_X,
                                 Min_Y => Min_Y) loop
      Current_Time := Current_Time + 1;
   end loop;
end Day10_2;
