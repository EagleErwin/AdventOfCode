with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps; use Ada.Containers;

procedure Day05_2 is
   -- ### CONSTANTS ### --

   -- ### TYPE DEFINITIONS ### --
   function Natural_Hash (id : Integer) return Hash_Type is
   begin
      -- I don't know how to calculate a Hash that makes more sense.
      return Hash(Integer'Image(id));
   end Natural_Hash;

   -- In a row, the key is the X index, the value the number of thermal vents
   package Floor_Row is new Hashed_Maps(Key_Type        => Integer,
                                        Element_Type    => Integer,
                                        Hash            => Natural_Hash,
                                        Equivalent_Keys => "=" );
   use Floor_Row;

   -- On the floor, the key is the Y index, the value the row
   package Ocean_Floor is new Hashed_Maps(Key_Type        => Integer,
                                          Element_Type    => Floor_Row.Map,
                                          Hash            => Natural_Hash,
                                          Equivalent_Keys => "=" );
   use Ocean_Floor;

   procedure Read_Line(Input : in Unbounded_String;
                       Administration : in out Ocean_Floor.Map) is
      Begin_X : Integer;
      End_X   : Integer;
      Begin_Y : Integer;
      End_Y   : Integer;

      First_Comma_Pos  : Natural;
      Arrow_Pos        : Natural;
      Second_Comma_Pos : Natural;

      X1 : Integer;
      X2 : Integer;
      Y1 : Integer;
      Y2 : Integer;

      Diagonal_Diff : Integer;
      Curr_X        : Integer;
      Curr_Y        : Integer;
   begin
      First_Comma_Pos := Index(Input, ",");
      Arrow_Pos := Index(Input, " -> ");
      Second_Comma_Pos := Index(Input, ",", Backward);

      Begin_X := Integer'Value(Slice(Input, 1, First_Comma_Pos - 1));
      Begin_Y   := Integer'Value(Slice(Input, First_Comma_Pos + 1, Arrow_Pos - 1));
      End_X := Integer'Value(Slice(Input, Arrow_Pos + 4, Second_Comma_Pos - 1));
      End_Y   := Integer'Value(Slice(Input, Second_Comma_Pos + 1, Length(Input)));

      if (Begin_X /= End_X) and then (Begin_Y /= End_Y) then
         -- Top left to bottom right
         if (Begin_X > End_X and then Begin_Y > End_Y) or else
           (End_X > Begin_X and then End_Y > Begin_Y) then
            X1 := Integer'Min(Begin_X, End_X);
            Y1 := Integer'Min(Begin_Y, End_Y);
            Diagonal_Diff := Abs(End_X - Begin_X);

            Diagonal_Loop:
            for I in 0 .. Diagonal_Diff loop
               Curr_X := X1 + I;
               Curr_Y := Y1 + I;
               if not Contains(Administration, Curr_Y) then
                  Administration.Insert(Curr_Y, Floor_Row.Empty_Map);
               end if;
               if not Contains(Administration(Curr_Y), Curr_X) then
                  Administration(Curr_Y).Insert(Curr_X, 0);
               end if;
               Administration(Curr_Y)(Curr_X) := Administration(Curr_Y)(Curr_X) + 1;
            end loop Diagonal_Loop;
         else
            -- Bottom left to top right
            X1 := Integer'Min(Begin_X, End_X);
            Y1 := Integer'Max(Begin_Y, End_Y);
            Diagonal_Diff := Abs(End_X - Begin_X);

            Bottom_Left_To_Top_Right:
            for I in 0 .. Diagonal_Diff loop
               Curr_X := X1 + I;
               Curr_Y := Y1 - I;
               if not Contains(Administration, Curr_Y) then
                  Administration.Insert(Curr_Y, Floor_Row.Empty_Map);
               end if;
               if not Contains(Administration(Curr_Y), Curr_X) then
                  Administration(Curr_Y).Insert(Curr_X, 0);
               end if;
               Administration(Curr_Y)(Curr_X) := Administration(Curr_Y)(Curr_X) + 1;
            end loop Bottom_Left_To_Top_Right;
         end if;


      else
         -- Horizontal / Vertical

         if (Begin_X <= End_X) then
            X1 := Begin_X;
            X2 := End_X;
         else
            X1 := End_X;
            X2 := Begin_X;
         end if;
         if (Begin_Y <= End_Y) then
            Y1 := Begin_Y;
            Y2 := End_Y;
         else
            Y1 := End_Y;
            Y2 := Begin_Y;
         end if;

         Row_Loop:
         for Y in Y1 .. Y2 loop
            if not Contains(Administration, Y) then
               Administration.Insert(Y, Floor_Row.Empty_Map);
            end if;
            Col_Loop:
            for X in X1 .. X2 loop
               if not Contains(Administration(Y), X) then
                  Administration(Y).Insert(X, 0);
               end if;
               Administration(Y)(X) := Administration(Y)(X) + 1;
            end loop Col_Loop;
         end loop Row_Loop;
      end if;
   end Read_Line;

   Input         : File_Type;
   Current_Line  : Unbounded_String;
   Thermal_Field : Ocean_Floor.Map;
   Current_Row   : Floor_Row.Map;
   Answer        : Integer := 0;
begin
   Open (File => Input, Mode => In_File, Name => "data/day05.input");
   while not End_Of_File (Input) loop
      Current_Line := To_Unbounded_String(Get_Line(Input));
      Read_Line(Current_Line, Thermal_Field);
   end loop;
   Close (File => Input);

   Row_Loop:
   for R in Thermal_Field.Iterate loop
      Column_Loop:
      for C in Thermal_Field(Key(R)).Iterate loop
         if Element(C) >= 2 then
            Answer := Answer + 1;
         end if;
      end loop Column_Loop;
   end loop Row_Loop;

   Put_line("In a total of " & Integer'Image(Answer)
            & " points do at least two lines overlap.");
end Day05_2;
