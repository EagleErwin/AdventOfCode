with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
procedure Day6_1 is
   Data_Line_Amount : constant Integer := 50; -- Number of lines in the dataset
   City_Dimension : constant Integer := 350;
         
   subtype Data_Line is Unbounded_String;
   subtype Building_Id is Integer range 0 .. Data_Line_Amount; -- Identification of a building. Id 0 is 'Not initialized'
   
   type Building is record
      Id : Building_Id;
      X : Integer range 0 .. City_Dimension - 1;
      Y : Integer range 0 .. City_Dimension - 1;
   end record;
   
   type Data_Array is array (1 .. Data_Line_Amount) of Building;
   type Blacklist_Array is array (1 .. Data_Line_Amount) of Boolean;
   type Admin is array (1 .. Data_Line_Amount) of Integer;
   type City_Row is array (1 .. City_Dimension) of Building_Id; 
   type City is array(1 .. City_Dimension) of City_Row; --Contains the city, with on each cell the closest building
   
   procedure Get_Coordinates(Input : in Unbounded_String; X : out Integer; Y : out Integer) is
      X_Length : Integer := 1;
      X_String : Unbounded_String;
      Y_String : Unbounded_String;
   begin
      if Element(Input, 2) = ',' then
         X_Length := 1;
      elsif Element(Input, 3) = ',' then
         X_Length := 2;
      elsif Element(Input, 4) = ',' then
         X_Length := 3;
      end if;

      X_String := To_Unbounded_String(Slice(Input, 1, X_Length));
      Y_String := To_Unbounded_String(Slice(Input, 3 + X_Length, Length(Input)));
      
      X := Integer'Value(To_String(X_String));
      Y := Integer'Value(To_String(Y_String));
   end Get_Coordinates;     
   
   procedure Load_File (Data : out Data_Array) is
      Current_Line   : Data_Line;
      Input          : File_Type;
      Row_Idx        : Integer := 0;
      Input_Building : Building;
      X_Idx, Y_Idx   : Integer;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day6.input");
      while not End_Of_File (Input) loop
         Current_Line   := To_Unbounded_String (Get_Line (Input));
         Row_Idx        := Row_Idx + 1;
         Get_Coordinates(Current_Line, X_Idx, Y_Idx);
         Input_Building := (Id => Row_Idx,
                            X => X_Idx,
                            Y => Y_Idx);
         Data (Row_Idx) := Input_Building;
      end loop;
      Close (File => Input);
   end Load_File;
   
   -- Find the closest building for the given coordinate. Returns the Building_Id
   function Find_Closest_Building (X, Y : Integer; Buildings : in Data_Array) return Building_Id is
      type Administration is array (Buildings'Range) of Integer;
      Ranges : Administration;
      Current_Building : Building;
      Distance : Integer;
      
      Smallest_Distance : Integer := City_Dimension;
      Valid : Boolean := False;
      Answer : Integer := 0;
   begin
      for I in Buildings'Range loop
         Current_Building := Buildings(I);
         Distance := abs (X - Current_Building.X) + abs (Y - Current_Building.Y);
         Ranges(Current_Building.Id) := Distance;
      end loop;
      
      for Idx in Ranges'Range loop
         if Ranges(Idx) < Smallest_Distance then
            Answer := Idx;
            Smallest_Distance := Ranges(Idx);
            Valid := True;
         elsif Ranges(Idx) = Smallest_Distance then
            Answer := 0;
            Valid := False;
         end if;
      end loop;
      
      return Answer;
   end Find_Closest_Building;
   
   -- Fill the city with the provided buildings
   procedure Fill_City (Data : in out City; Buildings : in Data_Array) is
      Current_Building : Building_Id;
   begin
      for R in 1 .. City_Dimension loop
         for C in 1 .. City_Dimension loop
            Current_Building := Find_Closest_Building(X => C, Y => R, Buildings => Buildings);
            Data(R)(C) := Current_Building;
         end loop;
      end loop;
   end Fill_City;
   
   Input_Data : Data_Array;
   Input_Area : City := (others => (others => 0));
   Closest_Building : Integer;
   Administration : Admin := (others => 0); -- Administration of how many times a Building_Id is the closest.
   Blacklist : Blacklist_Array := (others => False);
   
   Answer : Integer := 0;
begin
   Load_File(Data => Input_Data);
   Fill_City(Data => Input_Area, Buildings => Input_Data);

   for R in Input_Area'Range loop
      for C in Input_Area(R)'Range loop
         Closest_Building := Input_Area(R)(C);
         if Closest_Building /= 0 then -- Zero means multiple closest buildings.
            -- Border locations indicate infinite area. Blacklist the closest buildings there.
            if R = 0 or R = Data_Line_Amount or C = 0 or C = Data_Line_Amount then
               Blacklist(Closest_Building) := True;
            end if;
            Administration(Input_Area(R)(C)) := Administration(Input_Area(R)(C)) + 1;            
         end if;
         
      end loop;
   end loop;
   
   for I in Administration'Range loop
      if not Blacklist(I) then
         if Answer < Administration(I) then
            Answer := Administration(I);
         end if;
      end if;
   end loop;
   
   Put_Line(Integer'Image(Answer));
end Day6_1;
