with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day08_2 is

   -- ## CONSTANTS ## --
   Grid_Size : constant Integer := 99;

   type Tree is record
      Height       : Integer;
      Scenic_Score : Integer;
   end record;

   type Tree_Row is array (1 .. Grid_Size) of Tree;
   type Tree_Grid is array (1 .. Grid_Size) of Tree_Row;


   procedure Load_File (Trees : out Tree_Grid) is
      Input          : File_Type;
      Current_Line   : Unbounded_String;
      Current_Height : Integer;

      Row_Index : Integer := 0;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day08.input");
      Read_Line_Loop:
      while not End_Of_File (Input) loop
         Row_Index := Row_Index + 1;
         Trees(Row_Index) := (others => (0, 0));
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Read_Character_Loop:
         for I in 1 .. Length(Current_Line) loop
            Current_Height := Integer'Value("" & Element(Current_Line, I));
            Trees(Row_Index)(I).Height := Current_Height;
         end loop Read_Character_Loop;

      end loop Read_Line_Loop;
      Close (File => Input);
   end Load_File;

   function Calculate_Scenic_Score (Trees        : in Tree_Grid;
                                    Row_Index    : in Integer;
                                    Column_Index : in Integer) return Integer is
      Current_Tree_Height : Integer := Trees(Row_Index)(Column_Index).Height;
      North_Score : Integer := 0;
      South_Score : Integer := 0;
      West_Score  : Integer := 0;
      East_Score  : Integer := 0;

   begin
      Watch_North_Loop:
      for I in reverse 1 .. (Row_Index - 1) loop
         North_Score := North_Score + 1;
         if Current_Tree_Height <= Trees(I)(Column_Index).Height then
            exit Watch_North_Loop;
         end if;
      end loop Watch_North_Loop;

      Watch_South_Loop:
      for I in (Row_Index + 1) .. Grid_Size loop
         South_Score := South_Score + 1;
         if Current_Tree_Height <= Trees(I)(Column_Index).Height then
            exit Watch_South_Loop;
         end if;
      end loop Watch_South_Loop;

      Watch_West_Loop:
      for J in reverse 1 .. (Column_Index - 1) loop
         West_Score := West_Score + 1;
         if Current_Tree_Height <= Trees(Row_Index)(J).Height then
            exit Watch_West_Loop;
         end if;
      end loop Watch_West_Loop;

      Watch_East_Loop:
      for J in (Column_Index + 1) .. Grid_Size loop
         East_Score := East_Score + 1;
         if Current_Tree_Height <= Trees(Row_Index)(J).Height then
            exit Watch_East_Loop;
         end if;
      end loop Watch_East_Loop;

      return North_Score * South_Score * West_Score * East_Score;
   end Calculate_Scenic_Score;

   procedure Watch_Trees (Trees : in out Tree_Grid) is
   begin
      Row_Loop:
      for I in Tree_Grid'Range loop
         Column_Loop:
         for J in Trees(I)'Range loop
            Trees(I)(J).Scenic_Score := Calculate_Scenic_Score(Trees, I, J);
         end loop Column_Loop;
      end loop Row_Loop;

   end Watch_Trees;

   procedure Print_Forest (Trees : in Tree_Grid) is
   begin
      Row_Loop:
      for I in Trees'Range loop
         Column_Loop:
         for J in Trees(I)'Range loop
            Put(Integer'Image(Trees(I)(J).Scenic_Score) & "");
         end loop Column_Loop;
         Put_Line("");
      end loop Row_Loop;
   end Print_Forest;

   function Find_Max_Score (Trees : in Tree_Grid) return Integer is
      Result : Integer := 0;
   begin
      Row_Loop:
      for I in Trees'Range loop
         Column_Loop:
         for J in Trees(I)'Range loop
            if (Trees(I)(J).Scenic_Score > Result) then
               Result := Trees(I)(J).Scenic_Score;
            end if;
         end loop Column_Loop;
      end loop Row_Loop;

      return Result;
   end Find_Max_Score;

   Forest : Tree_Grid;
   Answer : Integer := 0;
begin
   Load_File(Forest);

   Watch_Trees(Forest);

   -- Print_Forest(Forest);

   Answer := Find_Max_Score(Forest);

   Put_Line("The highest scenic score possible for any tree is "
           & Integer'Image(Answer));
end Day08_2;
