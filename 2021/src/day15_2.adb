with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day15_2 is
   -- ### CONSTANTS ### --
   Max_Cost : constant Integer := (Integer'Last - 9);

   -- ### TYPE DEFINITIONS ### --
   type Coordinate is record
      X : Natural;
      Y : Natural;
   end record;

   subtype Coordinate_Index is Natural range 1 .. Natural'Last;

   type Risk is mod 10;

   type Position is record
      Enter_Cost    : Risk;
      Cost_To_Reach : Integer;
   end record;

   function Natural_Hash (id : Natural) return Hash_Type is
   begin
      -- I don't know how to calculate a Hash that makes more sense.
      return Hash(Integer'Image(id));
   end Natural_Hash;

   package Row is new Vectors(Index_Type   => Coordinate_Index,
                              Element_Type => Position);
   use Row;

   package Grid is new Vectors(Index_Type   => Coordinate_Index,
                               Element_Type => Row.Vector);
   use Grid;

   function To_Row (Input : in Unbounded_String) return Row.Vector is
      Result       : Row.Vector := Row.Empty_Vector;
      New_Position : Position;
   begin
      Character_Loop:
      for I in 1 .. Length(Input) loop
         New_Position := (Risk'Value((1 => Element(Input, I))), Max_Cost);
         Result.Append(New_Position);
      end loop Character_Loop;
      return Result;
   end To_Row;

   procedure Enlarge (Cave : in out Grid.Vector) is
      Original_First_Row_Index : Natural := Cave.First_Index;
      Original_Last_Row_Index  : Natural := Cave.Last_Index;
      Original_First_Col_Index : Natural := Cave(Original_First_Row_Index).First_Index;
      Original_Last_Col_Index  : Natural := Cave(Original_Last_Row_Index).Last_Index;

      New_Position   : Position;
      New_Enter_Cost : Risk;

      New_Row : Row.Vector;
   begin
      Row_Loop:
      for R in Original_First_Row_Index .. Original_Last_Row_Index loop
         Enlarge_Horizontal_Loop:
         for C_I in (Original_Last_Col_Index + 1) .. (5 * Original_Last_Col_Index) loop
            New_Enter_Cost := Cave(R)(C_I - Original_Last_Col_Index).Enter_Cost + 1;
            if New_Enter_Cost = 0 then
               New_Enter_Cost := 1;
            end if;
            New_Position := (New_Enter_Cost, Max_Cost);
            Cave(R).Append(New_Position);
         end loop Enlarge_Horizontal_Loop;
      end loop Row_Loop;

      Enlarge_Vertical_Loop:
      for R_I in (Original_Last_Row_Index + 1) .. (5 * Original_Last_Row_Index) loop
         New_Row := Row.Empty_Vector;
         Large_Column_Index_Loop:
         for L_C_I in 1 .. Cave(1).Last_Index loop --TODO
            New_Enter_Cost := Cave(R_I - Original_Last_Row_Index)(L_C_I).Enter_Cost + 1;
            if New_Enter_Cost = 0 then
               New_Enter_Cost := 1;
            end if;
            New_Position := (New_Enter_Cost, Max_Cost);
            New_Row.Append(New_Position);
         end loop Large_Column_Index_Loop;
         Cave.Append(New_Row);
      end loop Enlarge_Vertical_Loop;
   end Enlarge;

   procedure Load_File (Cave  : out Grid.Vector) is
      Input        : File_Type;
      Current_Line : Unbounded_String;

      Current_Row  : Row.Vector;
   begin
      Cave := Grid.Empty_Vector;
      Open (File => Input, Mode => In_File, Name => "data/day15.input");
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Current_Row := To_Row(Current_Line);
         Cave.Append(Current_Row);
      end loop;
      Close (File => Input);

      Enlarge(Cave);

      Cave(1)(1).Cost_To_Reach := 0;
   end Load_File;

   function Update_Neighbours(Row_Idx : in Natural;
                              Col_Idx : in Natural;
                              Cave    : in out Grid.Vector) return Boolean is
      North : Coordinate := (Col_Idx, Row_Idx - 1);
      East  : Coordinate := (Col_Idx + 1, Row_Idx);
      South : Coordinate := (Col_Idx, Row_Idx + 1);
      West  : Coordinate := (Col_Idx - 1, Row_Idx);

      Current_Pos : Position := Cave(Row_Idx)(Col_Idx);
      Next_Pos    : Position;
      Total_Costs : Integer;

      Updated : Boolean := False;
   begin
      -- Update North
      if North.Y > 0 then
         Next_Pos := Cave(North.Y)(North.X);
         Total_Costs := Current_Pos.Cost_To_Reach + Integer(Next_Pos.Enter_Cost);
         if Next_Pos.Cost_To_Reach > Total_Costs then
            Cave(North.Y)(North.X).Cost_To_Reach := Total_Costs;
            Updated := True;
         end if;
      end if;
      -- Update East
      if East.X <= Cave(Row_Idx).Last_Index then
         Next_Pos := Cave(East.Y)(East.X);
         Total_Costs := Current_Pos.Cost_To_Reach + Integer(Next_Pos.Enter_Cost);
         if Next_Pos.Cost_To_Reach > Total_Costs then
            Cave(East.Y)(East.X).Cost_To_Reach := Total_Costs;
            Updated := True;
         end if;
      end if;
      -- Update South
      if South.Y <= Cave.Last_Index then
         Next_Pos := Cave(South.Y)(South.X);
         Total_Costs := Current_Pos.Cost_To_Reach + Integer(Next_Pos.Enter_Cost);
         if Next_Pos.Cost_To_Reach > Total_Costs then
            Cave(South.Y)(South.X).Cost_To_Reach := Total_Costs;
            Updated := True;
         end if;
      end if;
      -- Update West
      if West.X > 0 then
         Next_Pos := Cave(West.Y)(West.X);
         Total_Costs := Current_Pos.Cost_To_Reach + Integer(Next_Pos.Enter_Cost);
         if Next_Pos.Cost_To_Reach > Total_Costs then
            Cave(West.Y)(West.X).Cost_To_Reach := Total_Costs;
            Updated := True;
         end if;
      end if;

      return Updated;
   end Update_Neighbours;

   -- Returns wheter one of the elements in the cave was updated.
   function Calculate_Costs (Cave : in out Grid.Vector) return Boolean is
      Updated : Boolean := False;
   begin
      Row_Loop:
      for R in Cave.Iterate loop
         Col_Loop:
         for C in Cave(R).Iterate loop
            Updated := Updated or Update_Neighbours(To_Index(R), To_Index(C), Cave);
         end loop Col_Loop;
      end loop Row_Loop;
      return Updated;
   end Calculate_Costs;

   Cave    : Grid.Vector;
   Updated : Boolean := True;

   Max_Row : Coordinate_Index;
   Max_Col : Coordinate_Index;

   Answer  : Integer := 0;
begin
   Load_File(Cave);

   Calculate_Costs_Loop:
   while Updated loop
      Updated := Calculate_Costs(Cave);
   end loop Calculate_Costs_Loop;

   Max_Row := Cave.Last_Index;
   Max_Col := Cave(Max_Row).Last_Index;
   Answer := Cave(Max_Row)(Max_Col).Cost_To_Reach;

   Put_line("The lowest total risk of any path from the top left to the bottom "
            & "right is " & Integer'Image(Answer) & ".");
end Day15_2;
