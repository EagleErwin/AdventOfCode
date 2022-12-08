with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day08_1 is

   -- ## CONSTANTS ## --
   Grid_Size : constant Integer := 99;

   type Tree is record
      Height  : Integer;
      Visible : Boolean;
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
         Trees(Row_Index) := (others => (0, False));
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Read_Character_Loop:
         for I in 1 .. Length(Current_Line) loop
            Current_Height := Integer'Value("" & Element(Current_Line, I));
            Trees(Row_Index)(I).Height := Current_Height;
         end loop Read_Character_Loop;

      end loop Read_Line_Loop;
      Close (File => Input);
   end Load_File;

   procedure Watch_Trees (Trees : in out Tree_Grid) is
      Current_Height : Integer;
      Max_Height     : Integer;
   begin
      Check_Left_To_Right_Loop:
      for I in Tree_Grid'Range loop
         Max_Height := -1;
         for J in Trees(I)'Range loop
            Current_Height := Trees(I)(J).Height;
            if (Current_Height > Max_Height) then
               Trees(I)(J).Visible := True;
               Max_Height := Current_Height;
            end if;
         end loop;
      end loop Check_Left_To_Right_Loop;

      Check_Right_To_Left_Loop:
      for I in Tree_Grid'Range loop
         Max_Height := -1;
         for J in reverse Trees(I)'Range loop
            Current_Height := Trees(I)(J).Height;
            if (Current_Height > Max_Height) then
               Trees(I)(J).Visible := True;
               Max_Height := Current_Height;
            end if;
         end loop;
      end loop Check_Right_To_Left_Loop;

      Check_Top_To_Bottom_Loop:
      for J in Trees(1)'Range loop
         Max_Height := -1;
         for I in Tree_Grid'Range loop
            Current_Height := Trees(I)(J).Height;
            if (Current_Height > Max_Height) then
               Trees(I)(J).Visible := True;
               Max_Height := Current_Height;
            end if;
         end loop;
      end loop Check_Top_To_Bottom_Loop;

      Check_Bottom_To_Top_Loop:
      for J in Trees(1)'Range loop
         Max_Height := -1;
         for I in reverse Tree_Grid'Range loop
            Current_Height := Trees(I)(J).Height;
            if (Current_Height > Max_Height) then
               Trees(I)(J).Visible := True;
               Max_Height := Current_Height;
            end if;
         end loop;
      end loop Check_Bottom_To_Top_Loop;
   end Watch_Trees;

   procedure Print_Forest (Trees : in Tree_Grid) is
   begin
      Row_Loop:
      for I in Trees'Range loop
         Column_Loop:
         for J in Trees(I)'Range loop
            if (Trees(I)(J).Visible) then
               Put(Integer'Image(Trees(I)(J).Height) & "*");
            else
               Put(Integer'Image(Trees(I)(J).Height) & " ");
            end if;
         end loop Column_Loop;
         Put_Line("");
      end loop Row_Loop;
   end Print_Forest;

   function Count_Visible_Trees (Trees : in Tree_Grid) return Integer is
      Result : Integer := 0;
   begin
      Row_Loop:
      for I in Trees'Range loop
         Column_Loop:
         for J in Trees(I)'Range loop
            if (Trees(I)(J).Visible) then
               Result := Result + 1;
            end if;
         end loop Column_Loop;
      end loop Row_Loop;

      return Result;
   end Count_Visible_Trees;

   Forest : Tree_Grid;
   Answer : Integer := 0;
begin
   Load_File(Forest);

   Watch_Trees(Forest);

   --Print_Forest(Forest);

   Answer := Count_Visible_Trees(Forest);

   Put_Line("The number of trees that are visible from outside the grid is "
           & Integer'Image(Answer));
end Day08_1;
