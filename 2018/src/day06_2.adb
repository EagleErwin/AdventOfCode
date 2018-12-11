with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
procedure Day06_2 is
   Data_Line_Amount : constant Integer := 50; -- Number of lines in the dataset
   City_Dimension : constant Integer := 350;
   Maximum_Distance : constant Integer := 10000;
         
   subtype Data_Line is Unbounded_String;
   
   type Building is record
      X : Integer range 0 .. City_Dimension - 1;
      Y : Integer range 0 .. City_Dimension - 1;
   end record;
   
   type Data_Array is array (0 .. Data_Line_Amount - 1) of Building;
   type City_Row is array (0 .. City_Dimension - 1) of Integer; 
   type City is array(0 .. City_Dimension - 1) of City_Row; --Contains the city, with on each cell the total distance to all buildings
   
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
      Open (File => Input, Mode => In_File, Name => "data/day06.input");
      while not End_Of_File (Input) loop
         Current_Line   := To_Unbounded_String (Get_Line (Input));
         Get_Coordinates(Current_Line, X_Idx, Y_Idx);
         Input_Building := (X => X_Idx,
                            Y => Y_Idx);
         Data (Row_Idx) := Input_Building;
         Row_Idx        := Row_Idx + 1;
      end loop;
      Close (File => Input);
   end Load_File;
   
   -- Find the total distance to all buildings. Returns the distance
   function Find_Total_Distance (X, Y : Integer; Buildings : in Data_Array) return Integer is
      Current_Building : Building;
      Distance : Integer := 0;
   begin
      for I in Buildings'Range loop
         Current_Building := Buildings(I);
         Distance := Distance + abs (X - Current_Building.X) + abs (Y - Current_Building.Y);
      end loop;
      return Distance;
   end Find_Total_Distance;
   
   -- Fill the city with the total distances
   procedure Fill_City (Data : in out City; Buildings : in Data_Array) is
      Distance : Integer;
   begin
      for R in 0 .. City_Dimension-1 loop
         for C in 0 .. City_Dimension-1 loop
            Distance := Find_Total_Distance(X => C, Y => R, Buildings => Buildings);
            Data(R)(C) := Distance;
         end loop;
      end loop;
   end Fill_City;
   
   Input_Data : Data_Array;
   Input_Area : City := (others => (others => 0));
   Distance : Integer;
   
   Answer : Integer := 0;
begin
   Load_File(Data => Input_Data);
   Fill_City(Data => Input_Area, Buildings => Input_Data);

   for R in 0 .. City_Dimension-1 loop
      for C in 0 .. City_Dimension-1 loop
         Distance := Input_Area(R)(C);
         if Distance < Maximum_Distance then
--            Put("*");
            Answer := Answer + 1;
--         else
--            Put(" ");
         end if;
      end loop;
--      Put_Line("");
   end loop;
   
   Put_Line(Integer'Image(Answer));
end Day06_2;
