with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;

procedure Day08_1 is
   -- ### CONSTANTS ### --
   Number_Of_Data_Records : constant Integer := 15_000; -- Number of characters in the input
   Image_Width  : constant Integer := 25;
   Image_Height : constant Integer := 6;
   PPL : constant := Image_Width * Image_Height; -- Pixels per Layer
   Number_Of_Layers : constant Integer := Number_Of_Data_Records / (Image_Width * Image_Height);

   -- ### TYPE DEFINITIONS ### --
   subtype Pixel is Integer range 0 .. 2;
   type Pixel_Access is access Pixel;
   type Image_Row is array (1 .. Image_Width) of Pixel_Access;
   type Image_Layer is array (1 .. Image_Height) of Image_Row;
   type Image is array (1 .. Number_Of_Layers) of Image_Layer;

   procedure Load_File (Puzzle_Input : out Image) is
      Input             : File_Type;
      Current_Character : Character;
      Current_Pixel     : Pixel_Access;
      Current_X         : Integer;
      Current_Y         : Integer;
      Current_Z         : Integer;
      Current_Idx       : Integer := 0;
   begin
      -- Initial data
      Puzzle_Input := (others => (others => (others => null)));

      Open (File => Input, Mode => In_File, Name => "data/day08.input");
      Read_File_Loop:
      while not End_Of_File (Input) loop
         Get(Input, Current_Character);
         Current_Pixel := new Pixel'(Integer'Value((1 => Current_Character)));

         Current_Idx := Current_Idx + 1;
         Current_Z   := ((Current_Idx - 1) / (Image_Width * Image_Height)) + 1;
         Current_Y   := (((Current_Idx - 1) mod PPL) / Image_Width) + 1;
         Current_X   := ((Current_Idx - 1) mod Image_Width) + 1;

         Puzzle_Input(Current_Z)(Current_Y)(Current_X) := Current_Pixel;
      end loop Read_File_Loop;
      Close (File => Input);

   end Load_File;

   function Solve (The_Image : in Image) return Integer is
      Curr_Num_Of_0 : Integer;
      Curr_Num_Of_1 : Integer;
      Curr_Num_Of_2 : Integer;

      Fewest_0 : Integer := Number_Of_Layers; -- Contains least amount of 0 seen in a layer
      Best_Layer : Integer := 0;
      Answer : Integer;
   begin
      LayerLoop:
      for L in 1 .. The_Image'Length loop
         Curr_Num_Of_0 := 0;
         Curr_Num_Of_1 := 0;
         Curr_Num_Of_2 := 0;
         RowLoop:
         for R in 1 .. The_Image(L)'Length loop
            ColLoop:
            for C in 1 .. The_Image(L)(R)'Length loop
               if The_Image(L)(R)(C).all = 0 then
                  Curr_Num_Of_0 := Curr_Num_Of_0 + 1;
               elsif The_Image(L)(R)(C).all = 1 then
                  Curr_Num_Of_1 := Curr_Num_Of_1 + 1;
	       elsif The_Image(L)(R)(C).all = 2 then
                  Curr_Num_Of_2 := Curr_Num_Of_2 + 1;
               end if;
            end loop ColLoop;
         end loop RowLoop;
         if Curr_Num_Of_0 < Fewest_0 then
            Best_Layer := L;
            Fewest_0 := Curr_Num_Of_0;
            Answer := Curr_Num_Of_1 * Curr_Num_Of_2;
         end if;
      end loop LayerLoop;

      return Answer;
   end Solve;

   BIOS_Password : Image;
   Answer   : Integer := 0;
begin
   Load_File (Puzzle_Input => BIOS_Password);

   Answer := Solve (The_Image => BIOS_Password);

   Put_Line("Answer:" & Integer'Image(Answer));
end Day08_1;
