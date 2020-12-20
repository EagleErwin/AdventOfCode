with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Vectors;     use Ada.Containers;
with GNAT.Regexp;                use GNAT.Regexp;

procedure Day20_2 is
   -- ### CONSTANTS ### --
   Enable_Debug    : constant Boolean := False;
   Tile_Dim        : constant Integer := 10;
   Mosaic_Dim      : constant Integer := 12; -- 3 for example, 12 for input
   Photo_Dim       : constant Integer := (Tile_Dim - 2) * Mosaic_Dim;
   Monster_Heigth  : constant Integer := 3;
   Monster_Length  : constant Integer := 20;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Pixel is (Black, White, Monster);
   type Tile_Row is array (1 .. Tile_Dim) of Pixel;
   type Tile_Grid is array (1 .. Tile_Dim) of Tile_Row;

   type Coordinate is record
      X : Natural;
      Y : Natural;
   end record;

   type Tile;
   type Tile_Access is access Tile;
   type Tile is record
      Id      : Natural;
      Image   : Tile_Grid;
      Placed  : Boolean;
      Above   : Tile_Access;
      Below   : Tile_Access;
      Left    : Tile_Access;
      Right   : Tile_Access;
   end record;

   type Mosaic_Row is array (1 .. Mosaic_Dim) of Tile_Access;
   type Mosaic is array (1 .. Mosaic_Dim) of Mosaic_Row;

   type Photo_Row is array ( 1 .. Photo_Dim ) of Pixel;
   type Photograph is array ( 1 .. Photo_Dim ) of Photo_Row;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   function Row_To_String(Input : in Tile_Row) return String is
      Result : Unbounded_String := To_Unbounded_String("");
   begin
      for P of Input loop
         case P is
            when Black => Result := Result & '.';
            when White => Result := Result & '#';
            when Monster => Result := Result & 'O';
         end case;
      end loop;
      return To_String(Result);
   end Row_To_String;

   function To_Tile_Row(Input : in Unbounded_String) return Tile_Row is
      Curr_Char : Character;
      Result : Tile_Row;
   begin
      --Log_Debug(To_String(Input));
      Parser_Loop:
      for I in 1 .. Length(Input) loop
         Curr_Char := Element(Input, I);
         case Curr_Char is
            when '.' => Result(I) := Black;
            when '#' => Result(I) := White;
            when others => Put_Line("ERROR: Unable to map pixel");
         end case;
      end loop Parser_Loop;

      return Result;
   end To_Tile_Row;


   -- Returns a Mosaic with tiles in wrong order
   procedure Load_File (Picture : out Mosaic) is
      Input        : File_Type;
      Current_Line : Data_Line;

      Tile_Row_Idx : Natural := 1;

      Mosaic_X : Natural := 1;
      Mosaic_Y : Natural := 1;

      Current_Tile : Tile_Access := new Tile'(Id      => 0,
                                              Image   => (others => (others => Black)),
                                              Placed  => False,
                                              Above   => null,
                                              Below   => null,
                                              Left    => null,
                                              Right   => null);
   begin
      Open (File => Input, Mode => In_File, Name => "data/day20.input");
      Read_Loop:
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         --Log_Debug("Reading line: " & To_String(Current_Line));
         if Current_Line = To_Unbounded_String("") then
            Picture(Mosaic_Y)(Mosaic_X) := Current_Tile;
            Current_Tile := new Tile'(Id      => 0,
                                      Image   => (others => (others => Black)),
                                      Placed  => False,
                                      Above   => null,
                                      Below   => null,
                                      Left    => null,
                                      Right   => null);
            Tile_Row_Idx := 1;
            if Mosaic_X = Mosaic_Dim then
               Mosaic_X := 1;
               Mosaic_Y := Mosaic_Y + 1;
            else
               Mosaic_X := Mosaic_X + 1;
            end if;
         elsif Element(Current_Line, 1) = 'T' then
            Current_Tile.Id := Natural'Value(Slice(Current_Line, 6, 9));
         else
            Current_Tile.Image(Tile_Row_Idx) := To_Tile_Row(Current_Line);
            Tile_Row_Idx := Tile_Row_Idx + 1;
         end if;
      end loop Read_Loop;
      -- Don't forget the last one
      Picture(Mosaic_Y)(Mosaic_X) := Current_Tile;

      Close (File => Input);
   end Load_File;

   -- Returns an Integer representation of the given row.
   -- When `Rev` is True, the row is reversed.
   function Row_To_Value(Row : in Tile_Row;
                         Rev : in Boolean) return Integer is
      Buffer : Unbounded_String := To_Unbounded_String("2#");
      Px : Pixel;
   begin
      Parser_Loop:
      for I in Row'Range loop
         if Rev then
            Px := Row((Row'Length + 1) - I);
         else
            Px := Row(I);
         end if;
         case Px is
            when Black => Buffer := Buffer & '0';
            when White => Buffer := Buffer & '1';
            when Monster => Buffer := Buffer & '1';
         end case;
      end loop Parser_Loop;
      Buffer := Buffer & '#';
      return Integer'Value(To_String(Buffer));
   end Row_To_Value;

   -- Returns a Column from Subject as a row.
   -- When `Is_First_Column` is False, the last column
   function Col_To_Row(Subject : in Tile_Access;
                       Is_First_Column : in Boolean) return Tile_Row is
      Col_Idx : Natural;
      Result : Tile_Row;
      Curr_Idx : Natural := 1;
   begin
      if Is_First_Column then
         Col_Idx := 1;
      else
         Col_Idx := Subject.Image(1)'Length;
      end if;

      for R of Subject.Image loop
         Result(Curr_Idx) := R(Col_Idx);
         Curr_Idx := Curr_Idx + 1;
      end loop;

      return Result;
   end Col_To_Row;

   -- Return the edge values of the given tile
   procedure Fill_Values(Reference    : in Tile_Access;
                         Top_Value    : out Integer;
                         Bottom_Value : out Integer;
                         Left_Value   : out Integer;
                         Right_Value  : out Integer) is
   begin
      Top_Value    := Row_To_Value(Reference.Image(1), False);
      Bottom_Value := Row_To_Value(Reference.Image(Tile_Dim), False);
      Left_Value   := Row_To_Value(Col_To_Row(Reference, True), False);
      Right_Value  := Row_To_Value(Col_To_Row(Reference, False), False);
   end Fill_Values;

   procedure Link_Placed_Tiles(Reference : in out Tile_Access;
                               Placement : in out Tile_Access) is
      Reference_Top_Value    : Integer;
      Reference_Bottom_Value : Integer;
      Reference_Left_Value   : Integer;
      Reference_Right_Value  : Integer;

      Placement_Top_Value    : Integer;
      Placement_Bottom_Value : Integer;
      Placement_Left_Value   : Integer;
      Placement_Right_Value  : Integer;
   begin
      Fill_Values(Reference, Reference_Top_Value, Reference_Bottom_Value,
                  Reference_Left_Value, Reference_Right_Value);
      Fill_Values(Placement, Placement_Top_Value, Placement_Bottom_Value,
                  Placement_Left_Value, Placement_Right_Value);

      Log_Debug("Reference " & Natural'Image(Reference.Id) & " top is " & Integer'Image(Reference_Top_Value));
      Log_Debug("Placement " & Natural'Image(Placement.Id) & " top is " & Integer'Image(Placement_Top_Value));
      Log_Debug("Reference " & Natural'Image(Reference.Id) & " bottom is " & Integer'Image(Reference_Bottom_Value));
      Log_Debug("Placement " & Natural'Image(Placement.Id) & " bottom is " & Integer'Image(Placement_Bottom_Value));

      if Reference_Top_Value = Placement_Bottom_Value then
      Log_Debug("Linking placed " & Natural'Image(Placement.Id)
                & " to reference " & Natural'Image(Reference.Id));
         Reference.Above := Placement;
         Placement.Below := Reference;
      elsif Reference_Bottom_Value = Placement_Top_Value then
      Log_Debug("Linking placed " & Natural'Image(Placement.Id)
                & " to reference " & Natural'Image(Reference.Id));
         Reference.Below := Placement;
         Placement.Above := Reference;
      elsif Reference_Left_Value = Placement_Right_Value then
      Log_Debug("Linking placed " & Natural'Image(Placement.Id)
                & " to reference " & Natural'Image(Reference.Id));
         Reference.Left := Placement;
         Placement.Right := Reference;
      elsif Reference_Right_Value = Placement_Left_Value then
      Log_Debug("Linking placed " & Natural'Image(Placement.Id)
                & " to reference " & Natural'Image(Reference.Id));
         Reference.Right := Placement;
         Placement.Left := Reference;
      end if;
   end Link_Placed_Tiles;

   -- Rotate the image in a tile 90 degrees clockwise
   procedure Rotate(A_Tile : in out Tile_Access) is
      New_Image : Tile_Grid := (others => (others => Black));
      New_X : Natural;
      New_Y : Natural;
   begin
      Row_Loop:
      for Y in A_Tile.Image'Range loop
         Pixel_Loop:
         for X in A_Tile.Image(Y)'Range loop
            New_X := (Tile_Dim + 1) - Y;
            New_Y := X;
            New_Image(New_Y)(New_X) := A_Tile.Image(Y)(X);
         end loop Pixel_Loop;
      end loop Row_Loop;
      A_Tile.Image := New_Image;
   end Rotate;

   -- Flip the image in a tile over the vertical axis
   procedure Flip(A_Tile : in out Tile_Access) is
      New_Image : Tile_Grid := (others => (others => Black));
   begin
      Row_Loop:
      for Y in A_Tile.Image'Range loop
         Pixel_Loop:
         for X in A_Tile.Image(Y)'Range loop
            New_Image(Y)((Tile_Dim + 1) - X) := A_Tile.Image(Y)(X);
         end loop Pixel_Loop;
      end loop Row_Loop;
      A_Tile.Image := New_Image;
   end Flip;

   -- Place all tiles against the reference
   procedure Place(Reference : in out Tile_Access;
                   Picture   : in out Mosaic) is
      Top_Value    : Integer;
      Bottom_Value : Integer;
      Left_Value   : Integer;
      Right_Value  : Integer;

      Top_Row      : Tile_Row;
      Bottom_Row   : Tile_Row;
      First_Column : Tile_Row;
      Last_Column  : Tile_Row;
   begin
      Fill_Values(Reference, Top_Value, Bottom_Value, Left_Value, Right_Value);

      Mosaic_Row_Loop:
      for R of Picture loop
         Mosaic_Tile_Loop:
         for T of R loop
            if T.Id /= Reference.Id then
               Top_Row      := T.Image(1);
               Bottom_Row   := T.Image(Tile_Dim);
               First_Column := Col_To_Row(T, True);
               Last_Column  := Col_To_Row(T, False);

               if T.Placed then
                  Log_Debug(Natural'Image(T.Id) & " is already placed, linking to " & Natural'Image(Reference.Id));
                  Link_Placed_Tiles(Reference, T);
               -------------
               -- Try top --
               -------------
               elsif Row_To_Value(Top_Row, False) = Top_Value then
                  Rotate(T);
                  Rotate(T);
                  Flip(T);
                  T.Below := Reference;
                  T.Placed := True;
                  Reference.Above := T;
                  Log_Debug("T1: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Top_Row, True) = Top_Value then
                  Rotate(T);
                  Rotate(T);
                  T.Below := Reference;
                  T.Placed := True;
                  Reference.Above := T;
                  Log_Debug("T2: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Bottom_Row, False) = Top_Value then
                  T.Below := Reference;
                  T.Placed := True;
                  Reference.Above := T;
                  Log_Debug("T3: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Bottom_Row, True) = Top_Value then
                  Flip(T);
                  T.Below := Reference;
                  T.Placed := True;
                  Reference.Above := T;
                  Log_Debug("T4: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(First_Column, False) = Top_Value then
                  Rotate(T);
                  Rotate(T);
                  Rotate(T);
                  T.Below := Reference;
                  T.Placed := True;
                  Reference.Above := T;
                  Log_Debug("T5: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(First_Column, True) = Top_Value then
                  Rotate(T);
                  Rotate(T);
                  Rotate(T);
                  Flip(T);
                  T.Below := Reference;
                  T.Placed := True;
                  Reference.Above := T;
                  Log_Debug("T6: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Last_Column, False) = Top_Value then
                  Rotate(T);
                  Flip(T);
                  T.Below := Reference;
                  T.Placed := True;
                  Reference.Above := T;
                  Log_Debug("T7: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Last_Column, True) = Top_Value then
                  Rotate(T);
                  T.Below := Reference;
                  T.Placed := True;
                  Reference.Above := T;
                  Log_Debug("T8: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               ----------------
               -- Try bottom --
               ----------------
               elsif Row_To_Value(Top_Row, False) = Bottom_Value then
                  T.Above := Reference;
                  T.Placed := True;
                  Reference.Below := T;
                  Log_Debug("B1: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Top_Row, True) = Bottom_Value then
                  Flip(T);
                  T.Above := Reference;
                  T.Placed := True;
                  Reference.Below := T;
                  Log_Debug("B2: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Bottom_Row, False) = Bottom_Value then
                  Rotate(T);
                  Rotate(T);
                  Flip(T);
                  T.Above := Reference;
                  T.Placed := True;
                  Reference.Below := T;
                  Log_Debug("B3: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Bottom_Row, True) = Bottom_Value then
                  Rotate(T);
                  Rotate(T);
                  T.Above := Reference;
                  T.Placed := True;
                  Reference.Below := T;
                  Log_Debug("B4: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(First_Column, False) = Bottom_Value then
                  Rotate(T);
                  Flip(T);
                  T.Above := Reference;
                  T.Placed := True;
                  Reference.Below := T;
                  Log_Debug("B5: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(First_Column, True) = Bottom_Value then
                  Rotate(T);
                  T.Above := Reference;
                  T.Placed := True;
                  Reference.Below := T;
                  Log_Debug("B6: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Last_Column, False) = Bottom_Value then
                  Rotate(T);
                  Rotate(T);
                  Rotate(T);
                  T.Above := Reference;
                  T.Placed := True;
                  Reference.Below := T;
                  Log_Debug("B7: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Last_Column, True) = Bottom_Value then
                  Rotate(T);
                  Rotate(T);
                  Rotate(T);
                  Flip(T);
                  T.Above := Reference;
                  T.Placed := True;
                  Reference.Below := T;
                  Log_Debug("B8: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               --------------
               -- Try left --
               --------------
               elsif Row_To_Value(Top_Row, False) = Left_Value then
                  Rotate(T);
                  T.Right := Reference;
                  T.Placed := True;
                  Reference.Left := T;
                  Log_Debug("L1: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Top_Row, True) = Left_Value then
                  Flip(T);
                  Rotate(T);
                  T.Right := Reference;
                  T.Placed := True;
                  Reference.Left := T;
                  Log_Debug("L2: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Bottom_Row, False) = Left_Value then
                  Flip(T);
                  Rotate(T);
                  Rotate(T);
                  Rotate(T);
                  T.Right := Reference;
                  T.Placed := True;
                  Reference.Left := T;
                  Log_Debug("L3: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Bottom_Row, True) = Left_Value then
                  Rotate(T);
                  Rotate(T);
                  Rotate(T);
                  T.Right := Reference;
                  T.Placed := True;
                  Reference.Left := T;
                  Log_Debug("L4: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(First_Column, False) = Left_Value then
                  Flip(T);
                  T.Right := Reference;
                  T.Placed := True;
                  Reference.Left := T;
                  Log_Debug("L5: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(First_Column, True) = Left_Value then
                  Rotate(T);
                  Rotate(T);
                  T.Right := Reference;
                  T.Placed := True;
                  Reference.Left := T;
                  Log_Debug("L6: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Last_Column, False) = Left_Value then
                  T.Right := Reference;
                  T.Placed := True;
                  Reference.Left := T;
                  Log_Debug("L7: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Last_Column, True) = Left_Value then
                  Rotate(T);
                  Rotate(T);
                  Flip(T);
                  T.Right := Reference;
                  T.Placed := True;
                  Reference.Left := T;
                  Log_Debug("L8: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               ---------------
               -- Try right --
               ---------------
               elsif Row_To_Value(Top_Row, False) = Right_Value then
                  Rotate(T);
                  Flip(T);
                  T.Left := Reference;
                  T.Placed := True;
                  Reference.Right := T;
                  Log_Debug("R1: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Top_Row, True) = Right_Value then
                  Rotate(T);
                  Rotate(T);
                  Rotate(T);
                  T.Left := Reference;
                  T.Placed := True;
                  Reference.Right := T;
                  Log_Debug("R2: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Bottom_Row, False) = Right_Value then
                  Rotate(T);
                  T.Left := Reference;
                  T.Placed := True;
                  Reference.Right := T;
                  Log_Debug("R3: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Bottom_Row, True) = Right_Value then
                  Flip(T);
                  Rotate(T);
                  T.Left := Reference;
                  T.Placed := True;
                  Reference.Right := T;
                  Log_Debug("R4: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(First_Column, False) = Right_Value then
                  T.Left := Reference;
                  T.Placed := True;
                  Reference.Right := T;
                  Log_Debug("R5: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(First_Column, True) = Right_Value then
                  Flip(T);
                  Rotate(T);
                  Rotate(T);
                  T.Left := Reference;
                  T.Placed := True;
                  Reference.Right := T;
                  Log_Debug("R6: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Last_Column, False) = Right_Value then
                  Flip(T);
                  T.Left := Reference;
                  T.Placed := True;
                  Reference.Right := T;
                  Log_Debug("R7: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               elsif Row_To_Value(Last_Column, True) = Right_Value then
                  Rotate(T);
                  Rotate(T);
                  T.Left := Reference;
                  T.Placed := True;
                  Reference.Right := T;
                  Log_Debug("R8: Linked " & Natural'Image(T.Id) & " to " & Natural'Image(Reference.Id));
               end if;
            end if;
         end loop Mosaic_Tile_Loop;
      end loop Mosaic_Row_Loop;
   end Place;

   function Is_All_Placed(Tiles : in out Mosaic) return Boolean is
   begin
      Mosaic_Row_Loop:
      for R of Tiles loop
         Mosaic_Tile_Loop:
         for T of R loop
            if not T.Placed then
               return False;
            end if;
         end loop Mosaic_Tile_Loop;
      end loop Mosaic_Row_Loop;
      return True;
   end Is_All_Placed;

   procedure Reorder(Picture : in out Mosaic) is
      All_Placed : Boolean := False;
   begin
      -- Put the first tile somewhere in the bottom right corner
      Picture(1)(1).Placed := True;

      Placement_Loop:
      while not All_Placed loop
         Mosaic_Row_Loop:
         for R of Picture loop
            Mosaic_Tile_Loop:
            for T of R loop
               if T.Placed then
                  Log_Debug("Using reference " & Natural'Image(T.Id));
                  Log_Debug("Top row: " & Row_To_String(T.Image(Tile_Dim)));
                  -- Only connect tiles to other placed tiles
                  Place(T, Picture);
               end if;
            end loop Mosaic_Tile_Loop;
         end loop Mosaic_Row_Loop;

         All_Placed := Is_All_Placed(Picture);
      end loop Placement_Loop;
   end Reorder;

   function Get_Top_Left_Tile(Picture : in Mosaic) return Tile_Access is
      Current_Tile : Tile_Access;
      Next_Tile : Tile_Access;
   begin
      Current_Tile := Picture(1)(1);
      Next_Tile := Current_Tile.Above;
      Walk_Up_Loop:
      while Next_Tile /= null loop
         Current_Tile := Next_Tile;
         Next_Tile := Current_Tile.Above;
      end loop Walk_Up_Loop;

      Next_Tile := Current_Tile.Left;
      Walk_Left_Loop:
      while Next_Tile /= null loop
         Current_Tile := Next_Tile;
         Next_Tile := Next_Tile.Left;
      end loop Walk_Left_Loop;

      return Current_Tile;
   end Get_Top_Left_Tile;

   -- Puts the Part with tile index X, Y into the Full image.
   procedure Fill_Photo(Full_Image : in out Photograph;
                        Part       : in Tile_Access;
                        X : in Natural;
                        Y : in Natural) is
      Current_X : Natural;
      Current_Y : Natural := (Y-1)*(Tile_Dim - 2);
   begin
      Log_Debug("Id: " & Integer'Image(Part.Id) & " Y: " & Natural'Image(Y));
      Row_Loop:
      for Image_Row in 2 .. (Tile_Dim - 1) loop
         Current_Y := Current_Y + 1;
         Pixel_Loop:
         for Image_Pixel in 2 .. (Tile_Dim - 1) loop
            Current_X := (X - 1)*(Tile_Dim-2) + (Image_Pixel-1);
            Full_Image(Current_Y)(Current_X) := Part.Image(Image_Row)(Image_Pixel);
         end loop Pixel_Loop;
      end loop Row_Loop;

   end Fill_Photo;

   procedure Stitch_Horizontal(Start_Tile : in Tile_Access;
                               Row : in Natural;
                               Full_Image : in out Photograph) is
      Current_Tile : Tile_Access := Start_Tile;
      Current_X : Natural := 1;
   begin
      Log_Debug("Stitching row " & Natural'Image(Row));
      while Current_Tile /= null loop
         Fill_Photo(Full_Image, Current_Tile, Current_X, Row);
         Current_X := Current_X + 1;
         Current_Tile := Current_Tile.Right;
      end loop;
      Log_Debug("Finished stitching row " & Natural'Image(Row));
   end Stitch_Horizontal;

   function Stitch(Picture : in out Mosaic) return Photograph is
      Current_Tile : Tile_Access := Get_Top_Left_Tile(Picture);
      Result : Photograph := (others => (others => Black));
   begin
      Row_Loop:
      for Current_Y in 1 .. Mosaic_Dim loop
         Stitch_Horizontal(Current_Tile, Current_Y, Result);
         Current_Tile := Current_Tile.Below;
      end loop Row_Loop;

      return Result;
   end Stitch;

   function Rotate_Photo(Full_Image : in Photograph) return Photograph is
      New_Image : Photograph := (others => (others => Black));
      New_X : Natural;
      New_Y : Natural;
   begin
      Row_Loop:
      for Y in Full_Image'Range loop
         Pixel_Loop:
         for X in Full_Image(Y)'Range loop
            New_X := (Photo_Dim + 1) - Y;
            New_Y := X;
            New_Image(New_Y)(New_X) := Full_Image(Y)(X);
         end loop Pixel_Loop;
      end loop Row_Loop;
      return New_Image;
   end Rotate_Photo;

   function Flip_Photo(Full_Image : in Photograph) return Photograph is
      New_Image : Photograph := (others => (others => Black));
   begin
      Row_Loop:
      for Y in Full_Image'Range loop
         Pixel_Loop:
         for X in Full_Image(Y)'Range loop
            New_Image(Y)((Photo_Dim + 1) - X) := Full_Image(Y)(X);
         end loop Pixel_Loop;
      end loop Row_Loop;
      return New_Image;
   end Flip_Photo;

   -- Returns whether the Full_Image contains a monster.
   -- Also updates the Full_Image, so it outlines the monster
   function Contains_Monster(Full_Image : in out Photograph) return Boolean is
      Result : Boolean := False;
   begin
      Row_Loop:
      for Y in 1 .. (Photo_Dim - Monster_Heigth) loop
         Col_Loop:
         for X in 1 .. (Photo_Dim - Monster_Length) loop
            if Full_Image(Y+1)(X+19) = White and then

              Full_Image(Y+2)(X+1) = White and then
              Full_Image(Y+2)(X+6) = White and then
              Full_Image(Y+2)(X+7) = White and then
              Full_Image(Y+2)(X+12) = White and then
              Full_Image(Y+2)(X+13) = White and then
              Full_Image(Y+2)(X+18) = White and then
              Full_Image(Y+2)(X+19) = White and then
              Full_Image(Y+2)(X+20) = White and then

              Full_Image(Y+3)(X+2) = White and then
              Full_Image(Y+3)(X+5) = White and then
              Full_Image(Y+3)(X+8) = White and then
              Full_Image(Y+3)(X+11) = White and then
              Full_Image(Y+3)(X+14) = White and then
              Full_Image(Y+3)(X+17) = White then
               Log_Debug("Monster found!");
               Full_Image(Y+1)(X+19) := Monster;

               Full_Image(Y+2)(X+1) := Monster;
               Full_Image(Y+2)(X+6) := Monster;
               Full_Image(Y+2)(X+7) := Monster;
               Full_Image(Y+2)(X+12) := Monster;
               Full_Image(Y+2)(X+13) := Monster;
               Full_Image(Y+2)(X+18) := Monster;
               Full_Image(Y+2)(X+19) := Monster;
               Full_Image(Y+2)(X+20) := Monster;

               Full_Image(Y+3)(X+2) := Monster;
               Full_Image(Y+3)(X+5) := Monster;
               Full_Image(Y+3)(X+8) := Monster;
               Full_Image(Y+3)(X+11) := Monster;
               Full_Image(Y+3)(X+14) := Monster;
               Full_Image(Y+3)(X+17) := Monster;
               Result := True;
            end if;
         end loop Col_Loop;
      end loop Row_Loop;
      return Result;
   end Contains_Monster;

   procedure Show_Photograph(Full_Image : in Photograph) is
   begin
      Put_Line("");
      Row_Loop:
      for R of Full_Image loop
         Col_Loop:
         for C of R loop
            case C is
               when Black => Put(".");
               when White => Put("#");
               when Monster => Put("O");
            end case;
         end loop Col_Loop;
         Put_Line("");
      end loop Row_Loop;
   end Show_Photograph;

   function Find_Monster(Full_Image : in Photograph) return Photograph is
      Monster_Found : Boolean := False;
      Result : Photograph := Full_Image;
   begin
      Rotate_Loop:
      for I in 1 .. 4 loop
         Result := Rotate_Photo(Result);
         Row_Loop:
         for R of Full_Image loop
            if Contains_Monster(Result) then
               return Result;
            end if;
         end loop Row_Loop;
         Log_Debug("Monster not found, flipping");

         Result := Flip_Photo(Result);
         Row_Loop2:
         for R of Full_Image loop
            if Contains_Monster(Result) then
               return Result;
            end if;
         end loop Row_Loop2;
      end loop Rotate_Loop;
      Put_Line("ERROR: Monster not found!");
      return Result;
   end Find_Monster;

   function Count_Water (Full_image : in out Photograph) return Long_Integer is
      Result : Long_Integer := 0;
   begin
      Row_Loop:
      for R of Full_Image loop
         Col_Loop:
         for C of R loop
            case C is
               when White => Result := Result + 1;
               when others => Result := Result + 0;
            end case;
         end loop Col_Loop;
      end loop Row_Loop;
      return Result;
   end Count_Water;

   Picture        : Mosaic;
   Full_Image     : Photograph;
   Answer         : Long_Integer := 1;
begin
   Load_File(Picture);

   Reorder(Picture);

   if Enable_Debug then
      for A_Row of Picture loop
         for A_Tile of A_Row loop
            Put_Line(Natural'Image(A_Tile.Id));
            Put("Above: ");
            if A_Tile.Above /= null then
               Put_Line(Natural'Image(A_Tile.Above.Id));
            else
               Put_Line("-");
            end if;

            Put("Below: ");
            if A_Tile.Below /= null then
               Put_Line(Natural'Image(A_Tile.Below.Id));
            else
               Put_Line("-");
            end if;

            Put("Left: ");
            if A_Tile.Left /= null then
               Put_Line(Natural'Image(A_Tile.Left.Id));
            else
               Put_Line("-");
            end if;

            Put("Right: ");
            if A_Tile.Right /= null then
               Put_Line(Natural'Image(A_Tile.Right.Id));
            else
               Put_Line("-");
            end if;
            Put_Line("");
         end loop;
      end loop;
   end if;

   Full_Image := Stitch(Picture);
   Full_Image := Find_Monster(Full_Image);

   if Enable_Debug then
      Show_Photograph(Full_Image);
   end if;

   Answer := Count_Water(Full_Image);

   Put_line("The number of water pixels is " & Long_Integer'Image(Answer));
end Day20_2;
