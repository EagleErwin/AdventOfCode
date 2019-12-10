with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Numerics;             use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Day10_2 is
   -- ### CONSTANTS ### --
   DEBUG : constant Boolean := False; -- Set this to True to enable debug logging
   Grid_Width   : constant Integer := 30;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is String (1 .. Grid_Width);
   type Coordinate_Int is mod Grid_Width;

   type Asteroid;
   type Asteroid_Access is access all Asteroid;

   type Visibility is (Visible, Invisible, Unknown);

   type Map_Data is record
      Contents : Asteroid_Access;
      Visible  : Visibility;
   end record;
   type Map_Data_Access is access all Map_Data;

   type Map_Row is array (Coordinate_Int) of Map_Data_Access;
   type Map is array (Coordinate_Int) of Map_Row;
   type Map_Access is access all Map;
   type Asteroid is record
      Nr_Of_Visible_Asteroids : Integer range 0 .. (Grid_Width * Grid_Width);
      Asteroid_Map : Map_Access;
      X_Pos : Coordinate_Int;
      Y_Pos : Coordinate_Int;
   end record;

   procedure Log_Debug (Log_Line : in String) is
   begin
      if (DEBUG) then
         Put_Line(Log_Line);
      end if;
   end Log_Debug;

   procedure Load_File (Input_Map : out Map) is
      Input         : File_Type;
      Current_Line  : Data_Line;
      Current_Char  : Character;
      Col_Idx       : Coordinate_Int := 0;
      Row_Idx       : Coordinate_Int := 0;
   begin
      -- Initial data
      Input_Map := (others => (others => null));

      Open (File => Input, Mode => In_File, Name => "data/day10.input");
      Read_Line_Loop:
      while not End_Of_File (Input) loop
         Current_Line := Get_Line(Input);
         Read_Char_Loop:
         for C in Current_Line'Range loop
            Current_Char := Current_Line(C);
            if Current_Char = '#' then
               Input_Map(Row_Idx)(Col_Idx) := new Map_Data'(Contents => new Asteroid'(Nr_Of_Visible_Asteroids => 0,
                                                                                      Asteroid_Map => null,
                                                                                      X_Pos => Col_Idx,
                                                                                      Y_Pos => Row_Idx),
                                                            Visible  => Unknown);
            end if;
            Col_Idx := Col_Idx + 1;
         end loop Read_Char_Loop;
         Row_Idx := Row_Idx + 1;
      end loop Read_Line_Loop;
      Close (File => Input);
   end Load_File;

   procedure Print_Map (Map_To_Print : in Map) is
   begin
      if DEBUG then
         Put_Line("");
         for R in Map_To_Print'Range loop
            for C in Map_To_Print(R)'Range loop
               if Map_To_Print(R)(C) = null then
                  Put(".");
               else
                  case Map_To_Print(R)(C).Visible is
                     when Visible => Put("o");
                     when Invisible => Put("x");
                     when Unknown => Put("?");
                  end case;
               end if;
            end loop;
            Put_Line("");
         end loop;
      end if;
   end Print_Map;

   function In_Range (Coordinate_Nr : in Integer) return Boolean is
   begin
      return Coordinate_Nr >= Integer(Coordinate_Int'First)
        and Coordinate_Nr <= Integer(Coordinate_Int'Last);
   end In_Range;

   -- Return a clone of the Input_Map
   function Clone_Map (Input_Map : in Map) return Map is
      Clone : Map := (others => (others => null));
   begin
      Row_Count_Loop:
      for R in Input_Map'Range loop
         Column_Count_Loop:
         for C in Input_Map(R)'Range loop
            if Input_Map(R)(C) /= null then
               Clone(R)(C) := new Map_Data'(Input_Map(R)(C).all);
            end if;
         end loop Column_Count_Loop;
      end loop Row_Count_Loop;
      return Clone;
   end Clone_Map;

   function Euclides (A : in Integer; B: in Integer) return Integer is
      ABS_A    : Integer := abs A;
      ABS_B    : Integer := abs B;
      Largest  : Integer;
      Smallest : Integer;
      Modulo   : Integer;
      Result   : Integer;
   begin
      if A = 0 then
         Result := ABS_B;
      elsif B = 0 then
         Result := ABS_A;
      else
         Largest := Integer'Max(ABS_A, ABS_B);
         Smallest := Integer'Min(ABS_A, ABS_B);
         Modulo := Largest mod Smallest;
         if Modulo = 0 then
            Result := Smallest;
         else
            Result := Euclides(Smallest, Modulo);
         end if;
      end if;
      return abs Result;
   end Euclides;

   procedure Get_Nr_Of_Visible_Asteroids (Input_Map : in Map;
                                          Station   : in out Asteroid_Access) is
      Tmp_Map : Map := Clone_Map(Input_Map);
      Result  : Integer := 0;
      Delta_X : Integer := 0;
      Delta_Y : Integer := 0;
      GCD     : Integer;
      Tmp_X   : Integer := 0;
      Tmp_Y   : Integer := 0;
      Tmp_Map_Data : Map_Data_Access;
   begin
      Row_Loop:
      for RI in Coordinate_Int'Range loop
         Column_Loop:
         for CI in Coordinate_Int'Range loop
            if Tmp_Map(RI)(CI) /= null and then
              Tmp_Map(RI)(CI).Visible /= Invisible and then
              not (RI = Station.Y_Pos and CI = Station.X_Pos) then
               Log_Debug("Calculating for (" & Coordinate_Int'Image(RI) & "," & Coordinate_Int'Image(CI) & ")");
               Tmp_Map(RI)(CI).Visible := Visible;
               Delta_X := Integer(CI) - Integer(Station.X_Pos);
               Delta_Y := Integer(RI) - Integer(Station.Y_Pos);
               Log_Debug("Delta_X:" & Integer'Image(Delta_X) & " Delta_Y:" & Integer'Image(Delta_Y));
               GCD := Euclides(Delta_X, Delta_Y);
               Log_Debug("GCD:" & Integer'Image(GCD));
               Delta_X := Delta_X / GCD;
               Delta_Y := Delta_Y / GCD;
               Log_Debug("- Delta_X:" & Integer'Image(Delta_X) & " Delta_Y:" & Integer'Image(Delta_Y));

               Tmp_X := Integer(CI) + Delta_X;
               Tmp_Y := Integer(RI) + Delta_Y;
               Update_Visibility_Loop:
               while In_Range(Tmp_X) and then In_Range(Tmp_Y) loop
                  Tmp_Map_Data := Tmp_Map(Coordinate_Int(Tmp_Y))(Coordinate_Int(Tmp_X));
                  if Tmp_Map_Data /= null then
                     Tmp_Map_Data.Visible := Invisible;
                  end if;
                  Tmp_X := Tmp_X + Delta_X;
                  Tmp_Y := Tmp_Y + Delta_Y;
               end loop Update_Visibility_Loop;
            end if;
         end loop Column_Loop;
      end loop Row_Loop;

      Print_Map(Tmp_Map);

      Station.Asteroid_Map := new Map'(Tmp_Map);

      Column_Count_Loop:
      for CI in Coordinate_Int'Range loop
         Row_Count_Loop:
         for RI in Coordinate_Int'Range loop
            if Tmp_Map(RI)(CI) /= null and then
              Tmp_Map(RI)(CI).Visible = Visible then
               Result := Result + 1;
            end if;
         end loop Row_Count_Loop;
      end loop Column_Count_Loop;
      Station.Nr_Of_Visible_Asteroids := Result;
      Log_Debug("Number of visible asteroids from ("
                & Coordinate_Int'Image(Station.X_Pos) & ","
                & Coordinate_Int'Image(Station.Y_Pos) & "): "
                & Integer'Image(Result));
   end Get_Nr_Of_Visible_Asteroids;

   function Get_Best_Location (Input_Map : in Map;
                               Best_X : out Coordinate_Int;
                               Best_Y : out Coordinate_Int) return Integer is
      Result : Integer := 0;
      Current_Asteroid : Asteroid_Access;
   begin
      Row_Loop:
      for R in Input_Map'Range loop
         Col_Loop:
         for C in Input_Map(R)'Range loop
            if Input_Map(R)(C) /= null then
               Current_Asteroid := Input_Map(R)(C).Contents;
               Get_Nr_Of_Visible_Asteroids(Input_Map => Input_Map,
                                           Station   => Current_Asteroid);
               if Current_Asteroid.Nr_Of_Visible_Asteroids > Result then
                  Result := Current_Asteroid.Nr_Of_Visible_Asteroids;
                  Best_X := Current_Asteroid.X_Pos;
                  Best_Y := Current_Asteroid.Y_Pos;
               end if;
            end if;
         end loop Col_Loop;
      end loop Row_Loop;

      return Result;
   end Get_Best_Location;

   -- Only the fourth quadrant is implemented, because after the first three there are not yet 200 asteroids killed.
   function Get_Angle(Origin : in Map_Data_Access;
                      Target : in Map_Data_Access) return Float is
      Origin_X : Integer := Integer(Origin.Contents.X_Pos);
      Origin_Y : Integer := Integer(Origin.Contents.Y_Pos);
      Target_X : Integer := Integer(Target.Contents.X_Pos);
      Target_Y : Integer := Integer(Target.Contents.Y_Pos);

      Delta_X : Integer := Origin_X - Target_X;
      Delta_Y : Integer := Origin_Y - Target_Y;

      Result : Float := 0.0;
   begin
      if Delta_X <= 0 and Delta_Y >= 0 then
         -- First quadrant
         Result := 0.0;
      elsif Delta_X <= 0 and Delta_Y < 0 then
         -- Second quadrant
         Result := 0.5 * Pi;
      elsif Delta_X > 0 and Delta_Y < 0 then
         -- Third quadrant
         Result := Pi;
      elsif Delta_X > 0 and Delta_Y >= 0 then
         -- Fourth quadrant
         Result := 1.5 * Pi + Arctan(Float(abs(Delta_Y)), Float(abs(Delta_X)));
      else
         Put_Line("Failed to determine quadrant for (" & Integer'Image(Target_X) & "," & Integer'Image(Target_Y) & ")");
      end if;

      return Result;
   end Get_Angle;

   function Solve (Input_Map : in out Map;
                   Subject_X : in Coordinate_Int;
                   Subject_Y : in Coordinate_Int) return Integer is
      Subject        : Map_Data_Access := Input_Map(Subject_Y)(Subject_X);
      Visibility_Map : Map := Subject.Contents.Asteroid_Map.all;

      Asteroid_To_Destroy : Map_Data_Access;
      Destroyed_Asteroids : Integer := 0;

      Current_Angle : Float;
      Smallest_Angle : Float;
   begin
      Destruction:
      while Destroyed_Asteroids < 200 loop
         Smallest_Angle := 2.0 * Pi;
         Print_Map(Visibility_Map);
         Row_Loop:
         for R in Visibility_Map'Range loop
            Col_Loop:
            for C in Visibility_Map(R)'Range loop
               if Visibility_Map(R)(C) /= null and then
                 Visibility_Map(R)(C).Visible = Visible then
                  Current_Angle := Get_Angle(Subject, Visibility_Map(R)(C));
                  if Current_Angle < Smallest_Angle then
                     Asteroid_To_Destroy := Visibility_Map(R)(C);
                     Smallest_Angle := Current_Angle;
                  end if;
               end if;
            end loop Col_Loop;
         end loop Row_Loop;

         Asteroid_To_Destroy.Visible := Invisible;
         Print_Map(Visibility_Map);
         Destroyed_Asteroids := Destroyed_Asteroids + 1;
         Log_Debug(Integer'Image(Destroyed_Asteroids)
                  & " Destroyed (" & Coordinate_Int'Image(Asteroid_To_Destroy.Contents.X_Pos)
                  & "," & Coordinate_Int'Image(Asteroid_To_Destroy.Contents.Y_Pos) & ")" & Float'Image(Smallest_Angle));
         if Destroyed_Asteroids >= 200 then
            exit Destruction;
         end if;
      end loop Destruction;


      return Integer(Asteroid_To_Destroy.Contents.X_Pos) * 100 + Integer(Asteroid_To_Destroy.Contents.Y_Pos);
   end Solve;

   Space_Area : Map;
   Best_X     : Coordinate_Int;
   Best_Y     : Coordinate_Int;
   Strength   : Integer;

   Answer     : Integer;
begin
   Load_File (Input_Map => Space_Area);
   --Print_Map(Map_To_Print => Space_Area);
   Strength := Get_Best_Location(Input_Map => Space_Area,
                                 Best_X => Best_X,
                                 Best_Y => Best_Y);

   Log_Debug("Highest strength:" & Integer'Image(Strength) & " at location (" & Coordinate_Int'Image(Best_X) & "," & Coordinate_Int'Image(Best_Y) & ")");

   Answer := Solve(Input_Map => Space_Area,
                   Subject_X => Best_X,
                   Subject_Y => Best_Y);

   Put_Line("Answer: " & Integer'Image(Answer));
end Day10_2;
