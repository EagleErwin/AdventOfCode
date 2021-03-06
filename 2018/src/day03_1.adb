with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
procedure Day03_1 is
   Data_Line_Amount : constant Integer := 1357; -- Number of lines in the dataset
   Max_Fabric_Width : constant Integer := 1000; -- Can be less
   Max_Fabric_Height : constant Integer := 1000; -- Can be less

   subtype Data_Line is Unbounded_String;
   
   type Data_Record is record
      Claim_Id : Integer;
      Start_X : Integer;
      Start_Y : Integer;
      Width : Integer;
      Height : Integer;
   end record;

   type Data_Array is array (1 .. Data_Line_Amount) of Data_Record;
   
   type Fabric_Claim is (Free, Once, Many);
   type Fabric_Row is array (0 .. Max_Fabric_Width) of Fabric_Claim;
   type Fabric is array (0 .. Max_Fabric_Height) of Fabric_Row;

   function Get_Claim_Id (Line : in Data_Line) return Integer is
      Started : Boolean := False;
      Finished : Boolean := False;
      Buffer : Unbounded_String;
      Result : Integer;
   begin
      for C in 1 .. Length(Line) loop
         exit when Finished;
         if Started then
            if Element(Line, C) = ' ' then
               Finished := True;
            else
               Buffer := Buffer & Element(Line, C);
            end if; 
         elsif Element(Line, C) = '#' then
             Started := True;
         end if;
      end loop;
      Result := Integer'Value (To_String (Buffer));
      return Result;
   end Get_Claim_Id;

   function Get_Start_X (Line : in Data_Line) return Integer is
      Started : Boolean := False;
      Finished : Boolean := False;
      Buffer : Unbounded_String;
      Result : Integer;
   begin
      for C in 1 .. Length(Line) loop
         exit when Finished;
         if Started then
            if Element(Line, C) = ',' then
               Finished := True;
            elsif Element(Line, C) /= ' ' then
               Buffer := Buffer & Element(Line, C);
            end if; 
         elsif Element(Line, C) = '@' then
             Started := True;
         end if;
      end loop;
      Result := Integer'Value (To_String (Buffer));
      return Result;
   end Get_Start_X;

   function Get_Start_Y (Line : in Data_Line) return Integer is
      Started : Boolean := False;
      Finished : Boolean := False;
      Buffer : Unbounded_String;
      Result : Integer;
   begin
      for C in 1 .. Length(Line) loop
         exit when Finished;
         if Started then
            if Element(Line, C) = ':' then
               Finished := True;
            else
               Buffer := Buffer & Element(Line, C);
            end if;
         elsif Element(Line, C) = ',' then
             Started := True;
         end if;
      end loop;
      Result := Integer'Value (To_String (Buffer));
      return Result;
   end Get_Start_Y;

   function Get_Width (Line : in Data_Line) return Integer is
      Started : Boolean := False;
      Finished : Boolean := False;
      Buffer : Unbounded_String;
      Result : Integer;
   begin
      for C in 1 .. Length(Line) loop
         exit when Finished;
         if Started then
            if Element(Line, C) = 'x' then
               Finished := True;
            elsif Element(Line, C) /= ' ' then
               Buffer := Buffer & Element(Line, C);
            end if;
         elsif Element(Line, C) = ':' then
             Started := True;
         end if;
      end loop;
      Result := Integer'Value (To_String (Buffer));
      return Result;
   end Get_Width;
   
   function Get_Height (Line : in Data_Line) return Integer is
      Started : Boolean := False;
      Finished : Boolean := False;
      Buffer : Unbounded_String;
      Result : Integer;
   begin
      for C in 1 .. Length(Line) loop
         if Started then
             Buffer := Buffer & Element(Line, C);
         elsif Element(Line, C) = 'x' then
             Started := True;
         end if;
      end loop;
      Result := Integer'Value (To_String (Buffer));
      return Result;
   end Get_Height;

   procedure Load_File (Data : out Data_Array) is
      Current_Line   : Data_Line;
      Current_Record : Data_Record;
      Input          : File_Type;
      Row_Idx        : Integer := 0;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day03.input");
      while not End_Of_File (Input) loop
         Current_Line   := To_Unbounded_String (Get_Line (Input));
         Current_Record := (Claim_Id => Get_Claim_Id(Line => Current_Line),
                            Start_X => Get_Start_X(Line => Current_Line),
                            Start_Y => Get_Start_Y(Line => Current_Line),
                            Width => Get_Width(Line => Current_Line),
                            Height => Get_Height(Line => Current_Line));
         Row_Idx        := Row_Idx + 1;
         Data (Row_Idx) := Current_Record;
      end loop;
      Close (File => Input);
   end Load_File;

   procedure Claim (Cell : in out Fabric_Claim) is
   begin
      case Cell is
         when Free => Cell := Once;
         when Once => Cell := Many;
         when Many => Cell := Many;
      end case;
   end Claim;
   
   function Print_Claim (Cell : in Fabric_Claim) return Character is
   begin
      case Cell is
         when Free => return ' ';
         when Once => return '.';
         when Many => return '#';
      end case;
   end Print_Claim;
   
   Input_Data   : Data_Array;
   Suit_Fabric : Fabric := (others => (others => Free));
   X : Integer;
   Y : Integer;
   W : Integer;
   H : Integer;
   
   Answer : Integer := 0;
begin
   Load_File (Data => Input_Data);
   for L in Input_Data'Range loop
      X := Input_Data(L).Start_X;
      Y := Input_Data(L).Start_Y;
      W := Input_Data(L).Width;
      H := Input_Data(L).Height;
      for Row in Y .. Y+H-1 loop
         for Column in X .. X+W-1 loop
            Claim(Suit_Fabric(Row)(Column));
         end loop;
      end loop;
   end loop;
   
   for Fabric_Cell_Row in Suit_Fabric'Range loop
      for Fabric_Cell in Suit_Fabric(Fabric_Cell_Row)'Range loop
         --Put(Print_Claim(Suit_Fabric(Fabric_Cell_Row)(Fabric_Cell)));
         if Suit_Fabric(Fabric_Cell_Row)(Fabric_Cell) = Many then
            Answer := Answer + 1;
         end if;
      end loop;
      --Put_Line("");
   end loop;
   
   Put_Line(Integer'Image(Answer));
end Day03_1;
