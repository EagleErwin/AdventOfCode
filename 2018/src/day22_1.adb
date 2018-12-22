with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
procedure Day22_1 is
   -- This dimension might raise a stack overflow. To execute the program,
   -- increase the stack size: `ulimit -s 90000`
   Cave_Dimension : constant Integer := 1_000; -- Estimated max X,Y coordinate
   X_Factor       : constant Integer := 16_807;
   Y_Factor       : constant Integer := 48_271;
   Erosion_Modulo : constant Integer := 20_183;

   subtype Data_Line is Unbounded_String;

   subtype Coordinate is Integer range 0 .. (Cave_Dimension - 1);

   subtype Risk_Integer is Integer range 1 .. 3;

   type Region_Type is (Rock, Wet, Narrow);

   type Region is record
      --X, Y : Coordinate;
      Geologic_Index : Integer;
      Erosion_Level  : Integer;
      Material       : Region_Type;
   end record;

   type Region_Access is access all Region;

   type Cave_Row is array (0 .. (Cave_Dimension - 1)) of Region_Access;
   type Cave is array (0 .. (Cave_Dimension - 1)) of Cave_Row;

   function Type_To_Char (Current_Type : in Region_Type) return Character is
   begin
      case Current_Type is
         when Rock => return '.';
         when Wet => return '=';
         when Narrow => return '|';
      end case;
   end Type_To_Char;

   procedure Load_File (Cave_Depth         : out Integer;
                        Target_X, Target_Y : out Coordinate) is
      Depth_Index   : constant Integer := 8; -- String index of the first digit of the depth
      Target_Index  : constant Integer := 9; -- String index of the first digit of the target
      Input         : File_Type;
      Current_Line  : Data_Line;
      Comma_Index   : Integer;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day22.input");
      -- Depth
      Current_Line := To_Unbounded_String(Get_Line(File => Input));
      Cave_Depth := Integer'Value(Slice(Source => Current_Line,
                                        Low    => Depth_Index,
                                        High   => Length(Current_Line)));

      -- Target
      Current_Line := To_Unbounded_String(Get_Line(File => Input));
      Comma_Index := Index(Source  => Current_Line,
                           Pattern => ",");
      Target_X := Integer'Value(Slice(Source => Current_Line,
                                      Low    => Target_Index,
                                      High   => Comma_Index - 1));
      Target_Y := Integer'Value(Slice(Source => Current_Line,
                                      Low    => Comma_Index + 1,
                                      High   => Length(Current_Line)));

      Close (File => Input);
   end Load_File;

   function Erosion_To_Type (Erosion_Level : in Integer) return Region_Type is
      Erosion_Mod_3 : Integer;
   begin
      Erosion_Mod_3 := Erosion_Level mod 3;
      if Erosion_Mod_3 = 0 then
         return Rock;
      elsif Erosion_Mod_3 = 1 then
         return Wet;
      elsif Erosion_Mod_3 = 2 then
         return Narrow;
      else
         Put_Line("How can the mod 3 of an integer be more than 2? :S");
         return Rock; -- Something must be returned...
      end if;
   end Erosion_To_Type;

   function Create_Cave (Depth : in Integer;
                         Target_X : in Coordinate;
                         Target_Y : in Coordinate) return Cave is
      Result : Cave := (others => (others => null));
      Curr_Geo_Index   : Integer;
      Curr_Erosion_Lvl : Integer;
      Curr_Type        : Region_Type;
   begin
      Row_Loop:
      for R in Result'Range loop
         Column_Loop:
         for C in Result(R)'Range loop
            if R = Target_Y and C = Target_X then
               Curr_Geo_Index := 0;
            elsif R = 0 then -- Y coordinate is 0
               Curr_Geo_Index := C * X_Factor;
            elsif C = 0 then -- X coordinate is 0
               Curr_Geo_Index := R * Y_Factor;
            else
               Curr_Geo_Index := Result(R)(C-1).Erosion_Level * Result(R-1)(C).Erosion_Level;
            end if;
            Curr_Erosion_Lvl := (Curr_Geo_Index + Depth) mod Erosion_Modulo;
            Curr_Type := Erosion_To_Type(Curr_Erosion_Lvl);

            Result(R)(C) := new Region'(Geologic_Index => Curr_Geo_Index,
                                        Erosion_Level  => Curr_Erosion_Lvl,
                                        Material       => Curr_Type);
         end loop Column_Loop;
      end loop Row_Loop;
      return Result;
   end Create_Cave;

   function Calculate_Risk(The_Cave : in Cave;
                           Target_X, Target_Y : in Coordinate) return Integer is
      Result : Integer := 0;
      Current_Type : Region_Type;
   begin
      Row_Loop:
      for R in Coordinate'First .. Target_Y loop
         Column_Loop:
         for C in Coordinate'First .. Target_X loop
            Current_Type := The_Cave(R)(C).Material;
            case Current_Type is
               when Rock => Result := Result + 0;
               when Wet => Result := Result + 1;
               when Narrow => Result := Result + 2;
            end case;
         end loop Column_Loop;
      end loop Row_Loop;
      return Result;
   end Calculate_Risk;

   procedure Print_Cave(Cave_To_Print : in Cave) is
   begin
      Row_Loop:
      for R in Cave_To_Print'Range loop
         Column_Loop:
         for C in Cave_To_Print(R)'Range loop
            Put(Type_To_Char(Cave_To_Print(R)(C).Material));
         end loop Column_Loop;
         Put_Line("");
      end loop Row_Loop;
      Put_Line("");
   end Print_Cave;

   Search_Area : Cave;
   Cave_Depth  : Integer;
   Target_X, Target_Y : Coordinate;

   Answer : Integer;
begin
   Load_File(Cave_Depth => Cave_Depth,
             Target_X   => Target_X,
             Target_Y   => Target_Y);

   Search_Area := Create_Cave(Depth => Cave_Depth,
                              Target_X => Target_X,
                              Target_Y => Target_Y);

   --Print_Cave(Cave_To_Print => Search_Area);

   Answer := Calculate_Risk(The_Cave => Search_Area,
                            Target_X => Target_X,
                            Target_Y => Target_Y);

   Put_Line("Answer:" & Integer'Image(Answer));
end Day22_1;
