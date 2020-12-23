with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;

procedure Day23_1 is
   -- ### CONSTANTS ### --
   Enable_Debug     : constant Boolean := False;
   Number_Of_Cups   : constant Natural := 9;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   type Cup_Number;
   type Cup_Number_Access is access Cup_Number;
   type Cup_Number is record
      Value : Natural;
      Next  : Cup_Number_Access;
   end record;

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

   procedure Load_File (Result : out Cup_Number_Access) is
      Input        : File_Type;
      Current_Line : Data_Line;

      Current_Value : Natural;
      Current_Cup   : Cup_Number_Access;
      Previous_Cup : Cup_Number_Access;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day23.input");
      Current_Line := To_Unbounded_String(Get_Line(Input));

      Result := null;

      for I in 1 .. Length(Current_Line) loop
         Previous_Cup := Current_Cup;
         Current_Value := Natural'Value((1 => Element(Current_Line, I)));
      Current_Cup := new Cup_Number'(Value => Current_Value,
                                     Next  => null);

         if Result = null then
            Result := Current_Cup;
         else
            Previous_Cup.Next := Current_Cup;
         end if;
      end loop;

      while Current_Cup.Next /= null loop
         Current_Cup := Current_Cup.Next;
      end loop;

      Current_Cup.Next := Result;
   end Load_File;

   function Find_Destination(Current : in Cup_Number_Access)
                             return Cup_Number_Access is
      Current_Value : Natural := Current.Value;
      Result : Cup_Number_Access := Current;
   begin
      First_Destination_Number_Loop:
      for I in reverse 0 .. (Current_Value - 1) loop
         First_Find_Cup_Loop:
         for N in 1 .. Number_Of_Cups loop
            Result := Result.Next;
            if Result.Value = I then
               Log_Debug("(1)Next destination is " & Natural'Image(Result.Value));
               return Result;
            end if;
         end loop First_Find_Cup_Loop;
      end loop First_Destination_Number_Loop;

      -- Wrap around
      Second_Destination_Number_Loop:
      for I in reverse Current_Value .. 9 loop
         Second_Find_Cup_Loop:
         for N in 1 .. Number_Of_Cups loop
            Result := Result.Next;
            if Result.Value = I then
               Log_Debug("(2)Next destination is " & Natural'Image(Result.Value));
               return Result;
            end if;
         end loop Second_Find_Cup_Loop;
      end loop Second_Destination_Number_Loop;

      Put_Line("ERROR: Destination not found");
      return Result;
   end Find_Destination;

   function Play (Current : in Cup_Number_Access) return Cup_Number_Access is
      Taken         : Cup_Number_Access; -- The first of the 3 taken numbers

      Destination : Cup_Number_Access;
      Destination_Next : Cup_Number_Access;
   begin
      -- Take 3 cups out
      Taken := Current.Next;
      Current.Next := Taken.Next.Next.Next;

      Destination := Find_Destination(Current);
      Destination_Next := Destination.Next;
      Destination.Next := Taken;
      Taken.Next.Next.Next := Destination_Next;

      return Current.Next;
   end Play;

   function Get_Labels (Current : in Cup_Number_Access)
                        return Unbounded_String is
      Result : Unbounded_String := To_Unbounded_String("");

      Start : Cup_Number_Access := Current;
   begin
      Find_One_Loop:
      while Start.Value /= 1 loop
         Start := Start.Next;
      end loop Find_One_Loop;

      Append_String_Loop:
      for I in 1 .. Number_Of_Cups - 1 loop
         Result := Result & Natural'Image(Start.Next.Value)(2);
         Start := Start.Next;
      end loop Append_String_Loop;

      return Result;
   end Get_Labels;

   Current : Cup_Number_Access;
   Answer   : Unbounded_String := To_Unbounded_String("");
begin
   Load_File(Current);

   Play_Loop:
   for I in 1 .. 100 loop
      Print_List(Current);
      Current := Play(Current);
   end loop Play_Loop;

   Answer := Get_Labels(Current);

   Put_line("The labels on the cups after cup 1 are: " & To_String(Answer));
end Day23_1;
