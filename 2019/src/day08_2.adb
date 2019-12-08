with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;

procedure Day08_2 is
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

   procedure Render (The_Image : in Image) is
      Result : Image_Layer := (others => (others => new Pixel'(2)));
   begin
      LayerLoop:
      for L in 1 .. The_Image'Length loop
         RowLoop:
         for R in 1 .. The_Image(L)'Length loop
            ColLoop:
            for C in 1 .. The_Image(L)(R)'Length loop
               if Result(R)(C).all = 2 then
                  Result(R)(C) := The_Image(L)(R)(C);
               end if;
            end loop ColLoop;
         end loop RowLoop;
      end loop LayerLoop;

      PrintRowLoop:
      for R in 1 .. Result'Length loop
         PrintColLoop:
         for C in 1 .. Result(R)'Length loop
            if Result(R)(C).all = 0 then
               Put('#');
            elsif Result(R)(C).all = 1 then
               Put('.');
            else
               Put('?');
            end if;
         end loop PrintColLoop;
         Put_Line("");
      end loop PrintRowLoop;
   end Render;

   BIOS_Password : Image;
begin
   Load_File (Puzzle_Input => BIOS_Password);
   Render (The_Image => BIOS_Password);
end Day08_2;
