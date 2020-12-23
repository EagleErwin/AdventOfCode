with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps; use Ada.Containers;

procedure Day23_2 is
   -- ### CONSTANTS ### --
   Enable_Debug     : constant Boolean := False;
   Number_Of_Cups   : constant Natural := 1_000_000;
   Number_Of_Rounds : constant Natural := 10_000_000;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Taken_Array is array (1 .. 3) of Integer;

   type Cup_Number;
   type Cup_Number_Access is access Cup_Number;
   type Cup_Number is record
      Value : Natural;
      Next  : Cup_Number_Access;
   end record;

   function Natural_Hash (Value : Natural) return Hash_Type is
   begin
      return Hash_Type(Value);
   end Natural_Hash;

   package Administration_Map is new Hashed_Maps(Key_Type        => Natural,
                                                 Element_Type    => Cup_Number_Access,
                                                 Hash            => Natural_Hash,
                                                 Equivalent_Keys => "=");
   use Administration_Map;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   procedure Print_List(Current : in Cup_Number_Access) is
      Tmp : Cup_Number_Access := Current;
   begin
      if Enable_Debug then
         for I in 1 .. Number_Of_Cups loop
            Put(Natural'Image(Tmp.Value));
            Tmp := Tmp.Next;
         end loop;
         Put_Line("");
      end if;
   end Print_List;

   procedure Load_File (Result : out Cup_Number_Access;
                        Administration : out Administration_Map.Map) is
      Input        : File_Type;
      Current_Line : Data_Line;

      Current_Value : Natural;
      Current_Cup   : Cup_Number_Access;
      Previous_Cup : Cup_Number_Access;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day23.input");
      Current_Line := To_Unbounded_String(Get_Line(Input));

      Result := null;

      Fill_From_File_Loop:
      for I in 1 .. Length(Current_Line) loop
         Previous_Cup := Current_Cup;
         Current_Value := Natural'Value((1 => Element(Current_Line, I)));
         Current_Cup := new Cup_Number'(Value => Current_Value,
                                        Next  => null);
         Administration.Insert(Current_Value, Current_Cup);
         if Result = null then
            Result := Current_Cup;
         else
            Previous_Cup.Next := Current_Cup;
         end if;
      end loop Fill_From_File_Loop;

      Fill_Rest_Loop:
      for Val in 10 .. Number_Of_Cups loop
         Previous_Cup := Current_Cup;
         Current_Value := Val;
         Current_Cup := new Cup_Number'(Value => Current_Value,
                                        Next  => null);
         Administration.Insert(Current_Value, Current_Cup);
         Previous_Cup.Next := Current_Cup;
      end loop Fill_Rest_Loop;

      while Current_Cup.Next /= null loop
         Current_Cup := Current_Cup.Next;
      end loop;

      Current_Cup.Next := Result;
   end Load_File;

   function Contains(Haystack : in Taken_Array; Needle : in Integer)
                     return Boolean is
   begin
      Search_Loop:
      for N in Haystack'Range loop
         if Haystack(N) = Needle then
            return True;
         end if;
      end loop Search_Loop;
      return False;
   end Contains;

   function Find_Destination(Current : in Cup_Number_Access;
                             Taken   : in Cup_Number_Access;
                             Administration : in Administration_Map.Map)
                             return Cup_Number_Access is
      Current_Value : Natural := Current.Value;
      Forbidden_Values : Taken_Array :=
        (Taken.Value, Taken.Next.Value, Taken.Next.Next.Value);
      Result : Cup_Number_Access := Current;
   begin
      First_Destination_Number_Loop:
      for I in reverse 1 .. (Current_Value - 1) loop
         if not Contains(Forbidden_Values, I) then
            Log_Debug("(1)Next destination is "
                      & Natural'Image(Administration(I).Value));
            return Administration(I);
         end if;
      end loop First_Destination_Number_Loop;

      -- Wrap around
      Second_Destination_Number_Loop:
      for I in reverse Current_Value .. Number_Of_Cups loop
         if not Contains(Forbidden_Values, I) then
            Log_Debug("(2)Next destination is "
                      & Natural'Image(Administration(I).Value));
            return Administration(I);
         end if;
      end loop Second_Destination_Number_Loop;

      Put_Line("ERROR: Destination not found");
      return Result;
   end Find_Destination;

   function Play (Current : in Cup_Number_Access;
                  Administration : in Administration_Map.Map)
                  return Cup_Number_Access is
      Taken         : Cup_Number_Access; -- The first of the 3 taken numbers

      Destination : Cup_Number_Access;
      Destination_Next : Cup_Number_Access;
   begin
      -- Take 3 cups out
      Taken := Current.Next;
      Current.Next := Taken.Next.Next.Next;

      Destination := Find_Destination(Current, Taken, Administration);
      Destination_Next := Destination.Next;
      Destination.Next := Taken;
      Taken.Next.Next.Next := Destination_Next;

      return Current.Next;
   end Play;

   function Get_Labels (Current : in Cup_Number_Access) return Long_Integer is
      Start : Cup_Number_Access := Current;
   begin
      Find_One_Loop:
      while Start.Value /= 1 loop
         Start := Start.Next;
      end loop Find_One_Loop;

      return Long_Integer(Start.Next.Value) * Long_Integer(Start.Next.Next.Value);
   end Get_Labels;

   Current        : Cup_Number_Access;
   Administration : Administration_Map.Map;
   Answer         : Long_Integer;
begin
   Load_File(Current, Administration);

   Play_Loop:
   for I in 1 .. Number_Of_Rounds loop
      --  Print_List(Current);
      Current := Play(Current, Administration);
   end loop Play_Loop;
   Answer := Get_Labels(Current);

   Put_line("The product of the two labels on the cups after cup 1 are:"
            & Long_Integer'Image(Answer));
end Day23_2;
