with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;

procedure Day10_1 is
   -- ### CONSTANTS ### --
   DEBUG : constant Boolean := False; -- Set this to True to enable debug logging
   Grid_Width   : constant Integer := 30;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is String (1 .. Grid_Width);
   type Coordinate_Int is mod Grid_Width;

   type Asteroid is record
      Nr_Of_Visible_Asteroids : Integer range 0 .. (Grid_Width * Grid_Width);
      X_Pos : Coordinate_Int;
      Y_Pos : Coordinate_Int;
   end record;
   type Asteroid_Access is access all Asteroid;

   type Visibility is (Visible, Invisible, Unknown);

   type Map_Data is record
      Contents : Asteroid_Access;
      Visible  : Visibility;
   end record;
   type Map_Data_Access is access all Map_Data;

   type Map_Row is array (Coordinate_Int) of Map_Data_Access;
   type Map is array (Coordinate_Int) of Map_Row;

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

   function Solve (Input_Map : in Map) return Integer is
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
               end if;
            end if;
         end loop Col_Loop;
      end loop Row_Loop;

      return Result;
   end Solve;

   Space_Area : Map;
   Answer   : Integer := 0;
begin
   Load_File (Input_Map => Space_Area);
   Print_Map(Map_To_Print => Space_Area);
   Answer := Solve(Input_Map => Space_Area);

   Put_Line("Answer:" & Integer'Image(Answer));
end Day10_1;
