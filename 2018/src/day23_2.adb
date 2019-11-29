with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
procedure Day23_2 is
   Number_Of_Bots  : constant Integer := 1_000;
   Coordinate_Size : constant Integer := 1_000_000_000; --Estimation

   subtype Data_Line is Unbounded_String;
   subtype Coordinate_Integer is Integer range -Coordinate_Size .. Coordinate_Size;

   type Bot;
   type Bot_Access is access Bot;

   type Bot is record
      X : Coordinate_Integer;
      Y : Coordinate_Integer;
      Z : Coordinate_Integer;
      R : Coordinate_Integer;
   end record;

   type Bot_Array is array (1 .. Number_Of_Bots) of Bot_Access;

   type Universe_Row is array (-Coordinate_Size .. Coordinate_Size) of Integer;
   type Universe_Row_Access is access all Universe_Row;
   type Universe_Plane is array (-Coordinate_Size .. Coordinate_Size) of Universe_Row_Access;
   type Universe_Plane_Access is access all Universe_Plane;
   type Universe is array (-Coordinate_Size .. Coordinate_Size) of Universe_Plane_Access;

   function Create_Bot (Bot_String : in Unbounded_String) return Bot_Access is
      X_Char_Pos : constant Integer := 6;
      String_Pointer : Integer;
      Current_Char : Character;
      Buffer       : Unbounded_String;
      X_Pos : Coordinate_Integer;
      Y_Pos : Coordinate_Integer;
      Z_Pos : Coordinate_Integer;
      R     : Coordinate_Integer;
   begin
      X_Loop:
      for I in X_Char_Pos .. Length(Bot_String) loop
         String_Pointer := I;
         Current_Char := Element(Bot_String, I);
         exit when Current_Char = ',';
         Buffer := Buffer & Current_Char;
      end loop X_Loop;
      X_Pos := Coordinate_Integer'Value(To_String(Buffer));
      Buffer := Null_Unbounded_String;
      String_Pointer := String_Pointer + 1;

      Y_Loop:
      for I in String_Pointer .. Length(Bot_String) loop
         String_Pointer := I;
         Current_Char := Element(Bot_String, I);
         exit when Current_Char = ',';
         Buffer := Buffer & Current_Char;
      end loop Y_Loop;
      Y_Pos := Coordinate_Integer'Value(To_String(Buffer));
      Buffer := Null_Unbounded_String;
      String_Pointer := String_Pointer + 1;

      Z_Loop:
      for I in String_Pointer .. Length(Bot_String) loop
         String_Pointer := I;
         Current_Char := Element(Bot_String, I);
         exit when Current_Char = '>';
         Buffer := Buffer & Current_Char;
      end loop Z_Loop;
      Z_Pos := Coordinate_Integer'Value(To_String(Buffer));
      Buffer := Null_Unbounded_String;
      String_Pointer := String_Pointer + 5;

      R_Loop:
      for I in String_Pointer .. Length(Bot_String) loop
         String_Pointer := I;
         Current_Char := Element(Bot_String, I);
         Buffer := Buffer & Current_Char;
      end loop R_Loop;
      R := Coordinate_Integer'Value(To_String(Buffer));

      return new Bot'(X => X_Pos,
                      Y => Y_Pos,
                      Z => Z_Pos,
                      R => R);
   end Create_Bot;

   procedure Load_File (Nanobots : out Bot_Array) is
      Input           : File_Type;
      Current_Line    : Data_Line;
      Current_Bot     : Bot_Access;
      Current_Bot_Idx : Integer := 0;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day23.input");
      Row_Loop:
      while not End_Of_File (Input) loop
         Current_Bot_Idx := Current_Bot_Idx + 1;
         Current_Line := To_Unbounded_String(Get_Line(File => Input));
         Current_Bot := Create_Bot(Bot_String => Current_Line);
         Nanobots(Current_Bot_Idx) := Current_Bot;
      end loop Row_Loop;

      Close (File => Input);
   end Load_File;

   function Is_In_Range (Master : in Bot_Access;
                         Slave : in Bot_Access) return Boolean is
      Distance : Coordinate_Integer;
   begin
      Distance := abs (Master.X - Slave.X) + abs (Master.Y - Slave.Y) + abs (Master.Z - Slave.Z);
      return Distance <= Master.R;
   end Is_In_Range;

   function Number_Of_Bots_In_Range (Nanobots : in Bot_Array;
                                     Subject : Bot_Access) return Integer is
      Result : Integer := 0;
   begin
      for I in Nanobots'Range loop
         if Is_In_Range (Master => Subject,
                         Slave  => Nanobots(I)) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Number_Of_Bots_In_Range;

   All_Bots      : Bot_Array;
   --The_Universe  : Universe := (others => null);
   Answer        : Integer;
begin
   Load_File (Nanobots      => All_Bots);



   Put_Line("Finished");
end Day23_2;
