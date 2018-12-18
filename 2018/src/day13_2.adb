with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
procedure Day13_2 is
   Maze_Dimension : constant Integer := 150;
   Number_Of_Carts : constant Integer := 17;

   subtype Data_Line is String (1 .. Maze_Dimension);

   subtype Coordinate is Integer range 0 .. Maze_Dimension - 1;

   type Direction is (Up, Down, Left, Right);
   type Cell_State is (Empty, Vertical, Horizontal, Crossing, Slash, Backslash);

   type Choice is (Left, Straight, Right);

   type Cart is record
      X_Pos : Coordinate;
      Y_Pos : Coordinate;
      Orientation : Direction;
      Next_Choice : Choice;
      Crashed : Boolean;
   end record;

   type Cart_Access is access all Cart;

   type Cart_Array is array (1 .. Number_Of_Carts) of Cart_Access;

   type Cell is record
      State : Cell_State;
      Occupation : Cart_Access;
   end record;

   type Maze_Row is array(0 .. Maze_Dimension - 1) of Cell;
   type Maze is array (0 .. Maze_Dimension - 1) of Maze_Row;

   function Create_Cart (Input_Character : in Character) return Cart_Access is
      Current_Direction : Direction;
   begin
      if Input_Character = '^' then
         Current_Direction := Up;
      elsif Input_Character = 'v' then
         Current_Direction := Down;
      elsif Input_Character = '<' then
         Current_Direction := Left;
      elsif Input_Character = '>' then
         Current_Direction := Right;
      else
         Put_Line("[ERROR] Invalid direction "
                  & Character'Image(Input_Character));
      end if;
      -- Position will be filled in later
      return new Cart'(X_Pos       => 0,
                       Y_Pos       => 0,
                       Orientation => Current_Direction,
                       Next_Choice => Choice'First,
                       Crashed     => False);
   end Create_Cart;

   function Char_To_State (Input_Character : in Character;
                           Output_State : out Cell_State) return Cart_Access is
      Result : Cart_Access := null;
   begin
      if Input_Character = ' ' then
         Output_State := Empty;
      elsif Input_Character = '|' then
         Output_State := Vertical;
      elsif Input_Character = '-' then
         Output_State := Horizontal;
      elsif Input_Character = '+' then
         Output_State := Crossing;
      elsif Input_Character = '/' then
         Output_State := Slash;
      elsif Input_Character = '\' then
         Output_State := Backslash;
      else
         Result := Create_Cart(Input_Character => Input_Character);
         if Input_Character = 'v' or Input_Character = '^' then
            Output_State := Vertical;
         elsif Input_Character = '<' or Input_Character = '>' then
            Output_State := Horizontal;
         end if;
      end if;
      return Result;
   end Char_To_State;

   procedure Fill_Maze_Cell (Input_Maze : in out Maze;
                             Row : in Coordinate;
                             Column : in Coordinate;
                             Cell_Char : in Character) is
      Current_Cart : Cart_Access;
      Current_Cell_State : Cell_State;
   begin
      Current_Cart := Char_To_State(Input_Character => Cell_Char,
                                    Output_State    => Current_Cell_State);
      if Current_Cart /= null then
         Current_Cart.X_Pos := Column;
         Current_Cart.Y_Pos := Row;
      end if;

      Input_Maze(Row)(Column) := Cell'(State      => Current_Cell_State,
                                       Occupation => Current_Cart);
   end Fill_Maze_Cell;

   procedure Load_File (Input_Maze : out Maze) is
      Input         : File_Type;
      Current_Line  : Data_Line;
      Current_Line_Number : Integer := 0;
      Current_Cell_Char : Character;
   begin
      Input_Maze := (others => (others => Cell'(State      => Empty,
                                                Occupation => null)));
      Open (File => Input, Mode => In_File, Name => "data/day13.input");

      Row_Loop:
      while not End_Of_File (Input) loop
         Current_Line  := Get_Line(File => Input);
         Cell_Loop:
         for I in Current_Line'Range loop
            Current_Cell_Char := Current_Line(I);
            Fill_Maze_Cell(Input_Maze => Input_Maze,
                           Row        => Current_Line_Number,
                           Column     => I - 1,
                           Cell_Char  => Current_Cell_Char);
         end loop Cell_Loop;
         Current_Line_Number := Current_Line_Number + 1;
      end loop Row_Loop;

      Close (File => Input);
   end Load_File;

   function Cell_To_Char(Cell_Char : in Cell_State) return Character is
   begin
      case Cell_Char is
         when Empty => return ' ';
         when Vertical => return '|';
         when Horizontal => return '-';
         when Crossing => return '+';
         when Slash => return '/';
         when Backslash => return '\';
         when others => return '?';
      end case;
   end Cell_To_Char;

   procedure Print_Maze(Maze_To_Print : in Maze) is
   begin
      --Put(ASCII.ESC & "[2J");
      for R in Maze_To_Print'Range loop
         for C in Maze_To_Print(R)'Range loop
            if Maze_To_Print(R)(C).Occupation /= null then
               Put("#");
            else
               Put(Cell_To_Char(Maze_To_Print(R)(C).State));
            end if;
         end loop;
         Put_Line("");
      end loop;
      Put_Line("");
   end Print_Maze;

   procedure Check_For_Collisions (Input_Maze : in out Maze;
                                   Carts : in Cart_Array;
                                   Carts_Left : in Integer) is
      type Id_Array is array (1 .. Carts'Length) of Integer;
      Cell_Id_Administration : Id_Array := (others => 150*150);
      Cell_Id : Integer;
      Collision_X : Coordinate;
      Collision_Y : Coordinate;
   begin
      Cart_Loop:
      for C in 1 .. Carts_Left loop
         if not Carts(C).Crashed then
            Cell_Id := Carts(C).X_Pos + Carts(C).Y_Pos * Maze_Dimension;
            Admin_Loop:
            for I in 1 .. Carts_Left loop
               if Cell_Id_Administration(I) = Cell_Id then
                  Collision_Y := Cell_Id / Maze_Dimension;
                  Collision_X := Cell_Id - (Collision_Y * Maze_Dimension);
                  Carts(C).Crashed := True;
                  Carts(I).Crashed := True;
               end if;
            end loop Admin_Loop;
            Cell_Id_Administration(C) := Cell_Id;
         end if;
      end loop Cart_Loop;
   end Check_For_Collisions;

   function Get_Cart_Order (Input_Maze : in Maze;
                            Carts_Left : out Integer) return Cart_Array is
      Carts : Cart_Array := (others => null);
      Current_Cart_Index : Integer := 1;
   begin
      -- Determine movement order of the carts
      Row_Loop:
      for R in Input_Maze'Range loop
         Col_Loop:
         for C in Input_Maze(R)'Range loop
            if Input_Maze(R)(C).Occupation /= null and then
              not Input_Maze(R)(C).Occupation.Crashed then
               Carts(Current_Cart_Index) := Input_Maze(R)(C).Occupation;
               Current_Cart_Index := Current_Cart_Index + 1;
            end if;
         end loop Col_Loop;
      end loop Row_Loop;
      Carts_Left := Current_Cart_Index - 1;
      return Carts;
   end Get_Cart_Order;

   procedure Rotate (Subject : in out Cart_Access; Clockwise : in Boolean) is
   begin
      if Clockwise then
         case Subject.Orientation is
            when Up => Subject.Orientation := Right;
            when Left => Subject.Orientation := Up;
            when Right => Subject.Orientation := Down;
            when Down => Subject.Orientation := Left;
         end case;
      else
         case Subject.Orientation is
            when Up => Subject.Orientation := Left;
            when Left => Subject.Orientation := Down;
            when Right => Subject.Orientation := Up;
            when Down => Subject.Orientation := Right;
         end case;
      end if;
   end Rotate;

   procedure Update_Next_Choice (Subject : in out Cart_Access) is
   begin
      case Subject.Next_Choice is
         when Left => Subject.Next_Choice := Straight;
         when Straight => Subject.Next_Choice := Right;
         when Right => Subject.Next_Choice := Left;
      end case;
   end Update_Next_Choice;

   procedure Update_For_Crossing (Subject : in out Cart_Access) is
   begin
      case Subject.Next_Choice is
         when Right => Rotate(Subject => Subject, Clockwise => True);
         when Left => Rotate(Subject => Subject, Clockwise => False);
         when others => Subject.Orientation := Subject.Orientation;
      end case;
      Update_Next_Choice(Subject => Subject);
   end Update_For_Crossing;

   procedure Move_Up (Input_Maze : in out Maze;
                      Moving_Cart : in out Cart_Access) is
      Current_X : Coordinate;
      Current_Y : Coordinate;
      Next_Y : Coordinate;
      Next_Cell_State : Cell_State;
   begin
      Current_X := Moving_Cart.X_Pos;
      Current_Y := Moving_Cart.Y_Pos;
      Next_Y := Current_Y - 1;
      -- Update the maze
      Input_Maze(Current_Y)(Current_X).Occupation := null;
      Input_Maze(Next_Y)(Current_X).Occupation := Moving_Cart;

      Moving_Cart.Y_Pos := Next_Y; -- Update the cart position

      Next_Cell_State := Input_Maze(Next_Y)(Current_X).State;
      -- Do something at +, / or \, ignore |, error for - and empty
      if Next_Cell_State = Crossing then
         Update_For_Crossing(Moving_Cart);
      elsif Next_Cell_State = Slash then
         Moving_Cart.Orientation := Right;
      elsif Next_Cell_State = Backslash then
         Moving_Cart.Orientation := Left;
      elsif Next_Cell_State = Empty or Next_Cell_State = Horizontal then
         Put_Line("[ERROR] Unexpected cell type in Move_Up: "
                  & Cell_State'Image(Next_Cell_State));
      end if;

      --Put_Line("Moving up");
   end Move_Up;

   procedure Move_Down (Input_Maze : in out Maze;
                        Moving_Cart : in out Cart_Access) is
      Current_X : Coordinate;
      Current_Y : Coordinate;
      Next_Y : Coordinate;
      Next_Cell_State : Cell_State;
   begin
      Current_X := Moving_Cart.X_Pos;
      Current_Y := Moving_Cart.Y_Pos;
      Next_Y := Current_Y + 1;
      -- Update the maze
      Input_Maze(Current_Y)(Current_X).Occupation := null;
      Input_Maze(Next_Y)(Current_X).Occupation := Moving_Cart;

      Moving_Cart.Y_Pos := Next_Y; -- Update the cart position

      Next_Cell_State := Input_Maze(Next_Y)(Current_X).State;
      -- Do something at +, / or \, ignore |, error for - and empty
      if Next_Cell_State = Crossing then
         Update_For_Crossing(Moving_Cart);
      elsif Next_Cell_State = Slash then
         Moving_Cart.Orientation := Left;
      elsif Next_Cell_State = Backslash then
         Moving_Cart.Orientation := Right;
      elsif Next_Cell_State = Empty or Next_Cell_State = Horizontal then
         Put_Line("[ERROR] Unexpected cell type in Move_Down "
                  & Cell_State'Image(Next_Cell_State));
         end if;

      --Put_Line("Moving down");
   end Move_Down;

   procedure Move_Left (Input_Maze : in out Maze;
                        Moving_Cart : in out Cart_Access) is
      Current_X : Coordinate;
      Current_Y : Coordinate;
      Next_X : Coordinate;
      Next_Cell_State : Cell_State;
   begin
      Current_X := Moving_Cart.X_Pos;
      Current_Y := Moving_Cart.Y_Pos;
      Next_X := Current_X - 1;
      -- Update the maze
      Input_Maze(Current_Y)(Current_X).Occupation := null;
      Input_Maze(Current_Y)(Next_X).Occupation := Moving_Cart;

      Moving_Cart.X_Pos := Next_X; -- Update the cart position

      Next_Cell_State := Input_Maze(Current_Y)(Next_X).State;
      -- Do something at +, / or \, ignore -, error for | and empty
      if Next_Cell_State = Crossing then
         Update_For_Crossing(Moving_Cart);
      elsif Next_Cell_State = Slash then
         Moving_Cart.Orientation := Down;
      elsif Next_Cell_State = Backslash then
         Moving_Cart.Orientation := Up;
      elsif Next_Cell_State = Empty or Next_Cell_State = Vertical then
         Put_Line("[ERROR] Unexpected cell type in Move_Left: "
                  & Cell_State'Image(Next_Cell_State));
      end if;

      --Put_Line("Moving left");
   end Move_Left;

   procedure Move_Right (Input_Maze : in out Maze;
                         Moving_Cart : in out Cart_Access) is
      Current_X : Coordinate;
      Current_Y : Coordinate;
      Next_X : Coordinate;
      Next_Cell_State : Cell_State;
   begin
      Current_X := Moving_Cart.X_Pos;
      Current_Y := Moving_Cart.Y_Pos;
      Next_X := Current_X + 1;
      -- Update the maze
      Input_Maze(Current_Y)(Current_X).Occupation := null;
      Input_Maze(Current_Y)(Next_X).Occupation := Moving_Cart;

      Moving_Cart.X_Pos := Next_X; -- Update the cart position

      Next_Cell_State := Input_Maze(Current_Y)(Next_X).State;
      -- Do something at +, / or \, ignore -, error for | and empty
      if Next_Cell_State = Crossing then
         Update_For_Crossing(Moving_Cart);
      elsif Next_Cell_State = Slash then
         Moving_Cart.Orientation := Up;
      elsif Next_Cell_State = Backslash then
         Moving_Cart.Orientation := Down;
      elsif Next_Cell_State = Empty or Next_Cell_State = Vertical then
         Put_Line("[ERROR] Unexpected cell type in Move_Right: "
                  & Cell_State'Image(Next_Cell_State));
      end if;

      --Put_Line("Moving right");
   end Move_Right;

   procedure Move (Input_Maze : in out Maze;
                   All_Carts : in out Cart_Array;
                   Carts_Left : in Integer;
                   Moving_Cart : in out Cart_Access) is
   begin
      case Moving_Cart.Orientation is
      when Up =>
         Move_Up(Input_Maze => Input_Maze, Moving_Cart => Moving_Cart);
      when Down =>
         Move_Down(Input_Maze => Input_Maze, Moving_Cart => Moving_Cart);
      when Left =>
         Move_Left(Input_Maze => Input_Maze, Moving_Cart => Moving_Cart);
      when Right =>
         Move_Right(Input_Maze => Input_Maze, Moving_Cart => Moving_Cart);
      end case;

      Check_For_Collisions(Input_Maze => Input_Maze,
                           Carts => All_Carts,
                           Carts_Left => Carts_Left);
   end Move;

   function Do_Tick (Input_Maze : in out Maze) return Boolean is
      Carts : Cart_Array;
      Carts_Left : Integer;
   begin
      Carts := Get_Cart_Order(Input_Maze => Input_Maze,
                              Carts_Left => Carts_Left);
      if Carts_Left = 1 then
         Put_Line("Only one cart left at location ("
                  & Integer'Image(Carts(1).X_Pos) & ","
                  & Integer'Image(Carts(1).Y_Pos) & ")");
         return True;
      end if;
      for I in 1 .. Carts_Left loop
         Move(Input_Maze  => Input_Maze,
              All_Carts => Carts,
              Carts_Left => Carts_Left,
              Moving_Cart => Carts(I));
      end loop;

      return False;
   end Do_Tick;

   Input_Maze : Maze;
   Current_Tick : Integer := 1;
begin
   Load_File (Input_Maze => Input_Maze);
   while not Do_Tick(Input_Maze) and Current_Tick < 1_000_000 loop
      Current_Tick := Current_Tick + 1;
   end loop;

   --Print_Maze(Maze_To_Print => Input_Maze);

   Put_Line("One cart left after" & Integer'Image(Current_Tick - 1) & " ticks.");
end Day13_2;
