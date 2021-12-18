with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day18_2 is
   -- ### CONSTANTS ### --
   Max_Nesting : constant Natural := 4;
   Max_Value   : constant Integer := 9;

   -- ### TYPE DEFINITIONS ### --
   type Snailfish_Number;
   type Snailfish_Number_Access is access Snailfish_Number;
   -- If Left or Right is null, Left_Value or Right_Value should be used.
   type Snailfish_Number is record
      Parent      : Snailfish_Number_Access;
      Left        : Snailfish_Number_Access;
      Left_Value  : Integer;
      Right       : Snailfish_Number_Access;
      Right_Value : Integer;
   end record;

   package Numbers_List is new Vectors(Index_Type   => Natural,
                                       Element_Type => Unbounded_String);
   use Numbers_List;

   type Side is (Left, Right);

   procedure Print (Input : in Snailfish_Number_Access) is
   begin
      Put("[");
      if Input.Left = null then
         Put(Integer'Image(Input.Left_Value));
      else
         Print(Input.Left);
      end if;
      Put(",");
      if Input.Right = null then
         Put(Integer'Image(Input.Right_Value));
      else
         Print(Input.Right);
      end if;
      Put("]");
   end Print;

   function Get_Snailfish_Number (Input  : in Unbounded_String)
                                  return Snailfish_Number_Access is
      Result : Snailfish_Number_Access;
      Current_Element : Snailfish_Number_Access := null;
      Current_Parent  : Snailfish_Number_Access;
      Curr_Char  : Character;
      Leaf       : Side := Left;
   begin
      Parser_Loop:
      for I in 1 .. Length(Input) loop
         Curr_Char := Element(Input, I);
         case Curr_Char is
            when '[' =>
               Current_Parent := Current_Element;
               Current_Element := new Snailfish_Number'(Parent      => Current_Parent,
                                                        Left        => null,
                                                        Left_Value  => -1,
                                                        Right       => null,
                                                        Right_Value => -1);
               if Current_Parent /= null then
                  case Leaf is
                     when Left  => Current_Parent.Left := Current_Element;
                     when Right => Current_Parent.Right := Current_Element;
                  end case;
               else
                  -- This is the root element
                  Result := Current_Element;
               end if;
               Leaf := Left;
            when ',' =>
               Leaf := Right;
            when ']' =>
               Current_Element := Current_Parent;
               if Current_Parent /= null then
                  Current_Parent := Current_Parent.Parent;
               end if;
               Leaf := Left;
            when others =>
               -- Should be a number here
               case Leaf is
                  when Left => Current_Element.Left_Value := Integer'Value((1 => Curr_Char));
                  when Right => Current_Element.Right_Value := Integer'Value((1 => Curr_Char));
               end case;
         end case;
      end loop Parser_Loop;
      return Result;
   end Get_Snailfish_Number;

   procedure Load_File (Homework : out Numbers_List.Vector) is
      Input        : File_Type;
      Current_Line : Unbounded_String;
   begin
      Homework := Numbers_List.Empty_Vector;
      Open (File => Input, Mode => In_File, Name => "data/day18.input");
      Read_File_Loop:
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Homework.Append(Current_Line);
      end loop Read_File_Loop;
      Close (File => Input);
   end Load_File;

   function Get_Depth (Value : in Snailfish_Number_Access) return Natural is
   begin
      if Value = null then
         return 0;
      else
         return 1 + Integer'Max(Get_Depth(Value.Left), Get_Depth(Value.Right));
      end if;
   end Get_Depth;

   function Get_Max_Value (Value : in Snailfish_Number_Access) return Integer is
   begin
      if Value.Left = null then
         if Value.Right = null then
            return Integer'Max(Value.Left_Value, Value.Right_Value);
         else
            return Integer'Max(Value.Left_Value, Get_Max_Value(Value.Right));
         end if;
      else
         if Value.Right = null then
            return Integer'Max(Get_Max_Value(Value.Left), Value.Right_Value);
         else
            return Integer'Max(Get_Max_Value(Value.Left), Get_Max_Value(Value.Right));
         end if;
      end if;
   end Get_Max_Value;

   -- Returns the leftmost pair of a Snailfish Number that is too deep.
   -- Returns null if there is pair too deep.
   function Get_Leftmost_Pair_That_Is_Too_Deep (Value : in Snailfish_Number_Access;
                                                Depth_Offset : in Natural := 0) return Snailfish_Number_Access is
      Candidate : Snailfish_Number_Access := Value;
   begin
      -- If there are no elements too deep, return null.
      if Get_Depth(Value) + Depth_Offset <= Max_Nesting then
         return null;
      end if;

      -- If this is a regular pair, this is the deep element.
      if Value.Left = null and then Value.Right = null then
         return Value;
      end if;

      -- If the Left of this element is too deep, find the deep element in the left branch
      if Value.Left /= null then
         if Get_Depth(Value.Left) + Depth_Offset > Max_Nesting - 1 then
            return Get_Leftmost_Pair_That_Is_Too_Deep(Value.Left, Depth_Offset + 1);
         end if;
      end if;

      -- If the Right of this element is too deep, find the deep element in the right branch.
      if Value.Right /= null then
         if Get_Depth(Value.Right) + Depth_Offset > Max_Nesting - 1 then
            return Get_Leftmost_Pair_That_Is_Too_Deep(Value.Right, Depth_Offset + 1);
         end if;
      end if;

      Put_Line("ERROR: Get_Leftmost_Pair_That_Is_Too_Deep got no result!");
      return null;
   end Get_Leftmost_Pair_That_Is_Too_Deep;

   -- Explode the leftmost pair in the number that is too deep.
   procedure Explode (Value : in out Snailfish_Number_Access) is
      Subject : Snailfish_Number_Access; -- The element to explode
      Parent  : Snailfish_Number_Access;
      Current_Elem : Snailfish_Number_Access;
   begin
      Subject := Get_Leftmost_Pair_That_Is_Too_Deep(Value);
      if Subject /= null then
         -- Find the first regular number to the left of subject
         Current_Elem := Subject;
         Parent := Current_Elem.Parent;

         -- Walk up to the tree until we can step left
         while Parent /= null and then Parent.Left = Current_Elem loop
            Current_Elem := Parent;
            Parent := Current_Elem.Parent;
         end loop;
         if Parent /= null then -- If Parent is null, there is no neighbour on the left
            Current_Elem := Parent;
            Parent := Current_Elem.Parent;
            if Current_Elem.Left = null then
               -- If there is no branch of the left, it's a regular number. Add to it.
               Current_Elem.Left_Value := Current_Elem.Left_Value + Subject.Left_Value;
            else
               -- Find the rightmost regular number in this branch and add to it.
               Current_Elem := Current_Elem.Left;
               while Current_Elem.Right /= null loop
                  Current_Elem := Current_Elem.Right;
               end loop;
               Current_Elem.Right_Value := Current_Elem.Right_Value + Subject.Left_Value;
            end if;
         end if;

         -- Find the first regular number to the right of subject
         Current_Elem := Subject;
         Parent := Current_Elem.Parent;
         -- Walk up to the tree until we can step right
         while Parent /= null and then Parent.Right = Current_Elem loop
            Current_Elem := Parent;
            Parent := Current_Elem.Parent;
         end loop;
         if Parent /= null then -- If Parent is null, there is no neighbour on the right
            Current_Elem := Parent;
            Parent := Current_Elem.Parent;
            if Current_Elem.Right = null then
               -- If there is no branch of the right, it's a regular number. Add to it.
               Current_Elem.Right_Value := Current_Elem.Right_Value + Subject.Right_Value;
            else
               -- Find the leftmost regular number in this branch and add to it.
               Current_Elem := Current_Elem.Right;
               while Current_Elem.Left /= null loop
                  Current_Elem := Current_Elem.Left;
               end loop;
               Current_Elem.Left_Value := Current_Elem.Left_Value + Subject.Right_Value;
            end if;
         end if;

         -- Remove the exploded pair.
         if Subject.Parent /= null then
            if Subject.Parent.Left = Subject then
               Subject.Parent.Left := null;
               Subject.Parent.Left_Value := 0;
            elsif Subject.Parent.Right = Subject then
               Subject.Parent.Right := null;
               Subject.Parent.Right_Value := 0;
            end if;
         end if;
      else
         Put_Line("No need to explode");
      end if;
   end Explode;

   -- Returns the leftmost pair of a Snailfish Number that has a value that is too high.
   -- Returns null if there is no value too high.
   function Get_Leftmost_Pair_With_High_Number (Value : in Snailfish_Number_Access) return Snailfish_Number_Access is
      Candidate : Snailfish_Number_Access := Value;
   begin
      -- If this number does not have a value that's too high, return null.
      if Get_Max_Value(Value) <= Max_Value then
         return null;
      end if;

      -- If the Left_Value of this element is too high, return this element.
      if Value.Left_Value > Max_Value then
         return Value;
      end if;

      -- If the Left of this element has a value that's too high, find the element in the left branch.
      if Value.Left /= null and then Get_Max_Value(Value.Left) > Max_Value then
         return Get_Leftmost_Pair_With_High_Number(Value.Left);
      end if;

      -- If the Right_Value of this element is too high, return this element.
      if Value.Right_Value > Max_Value then
         return Value;
      end if;

      -- If the Right of this element has a value that's too high, find the element in the right branch.
      if Value.Right /= null and then Get_Max_Value(Value.Right) > Max_Value then
         return Get_Leftmost_Pair_With_High_Number(Value.Right);
      end if;

      Put_Line("ERROR: Get_Leftmost_Pair_With_High_Number got no result!");
      return null;
   end Get_Leftmost_Pair_With_High_Number;

   -- Split the leftmost value in the number that is higher than 9.
   procedure Split (Value : in out Snailfish_Number_Access) is
      Subject  : Snailfish_Number_Access;
      New_Pair : Snailfish_Number_Access := new Snailfish_Number'(Parent      => null,
                                                                  Left        => null,
                                                                  Left_Value  => -1,
                                                                  Right       => null,
                                                                  Right_Value => -1);
   begin
      Subject := Get_Leftmost_Pair_With_High_Number(Value);
      if Subject /= null then
         New_Pair.Parent := Subject;
         if Subject.Left_Value > Max_Value then
            New_Pair.Left_Value := Subject.Left_Value / 2;
            New_Pair.Right_Value := Subject.Left_Value - New_Pair.Left_Value;
            Subject.Left := New_Pair;
            Subject.Left_Value := -1;
         elsif Subject.Right_Value > Max_Value then
            New_Pair.Left_Value := Subject.Right_Value / 2;
            New_Pair.Right_Value := Subject.Right_Value - New_Pair.Left_Value;
            Subject.Right := New_Pair;
            Subject.Right_Value := -1;
         end if;
      else
         Put_Line("No need to split");
      end if;
   end Split;

   procedure Reduce (Value : in out Snailfish_Number_Access) is
      Current_Depth     : Natural;
      Current_Max_Value : Integer;
   begin
      Current_Depth := Get_Depth(Value);
      if Current_Depth = 0 then
         -- We don't need to reduce an element that has no depth.
         return;
      end if;

      Current_Max_Value := Get_Max_Value(Value);
      Reduce_Loop:
      while Current_Depth > Max_Nesting or else Current_Max_Value > Max_Value loop
         Explode_Loop:
         while Current_Depth > Max_Nesting loop
            Explode(Value);
            Current_Depth := Get_Depth(Value);
            Current_Max_Value := Get_Max_Value(Value);
         end loop Explode_Loop;

         if Current_Max_Value > Max_Value then
            Split(Value);
         end if;
         Current_Depth := Get_Depth(Value);
         Current_Max_Value := Get_Max_Value(Value);
      end loop Reduce_Loop;
   end Reduce;

   -- Adds two Snailfish numbers to each other
   function Sum(First_Value  : in out Snailfish_Number_Access;
                Second_Value : in out Snailfish_Number_Access) return Snailfish_Number_Access is
      Result : Snailfish_Number_Access := new Snailfish_Number'(Parent      => null,
                                                                Left        => null,
                                                                Left_Value  => -1,
                                                                Right       => null,
                                                                Right_Value => -1);
   begin
      Result.Left := First_Value;
      Result.Right := Second_Value;
      First_Value.Parent := Result;
      Second_Value.Parent := Result;
      return Result;
   end Sum;

   function Calculate_Magnitude(Values : in Snailfish_Number_Access) return Integer is
      Left_Magnitude  : Integer := 0;
      Right_Magnitude : Integer := 0;
   begin
      if Values.Left = null then
         Left_Magnitude := Values.Left_Value;
      else
         Left_Magnitude := Calculate_Magnitude(Values.Left);
      end if;
      if Values.Right = null then
         Right_Magnitude := Values.Right_Value;
      else
         Right_Magnitude := Calculate_Magnitude(Values.Right);
      end if;

      return Left_Magnitude * 3 + Right_Magnitude* 2;
   end Calculate_Magnitude;

   function Find_Max_Magnitude (Input : in Numbers_List.Vector) return Integer is
      First_Value  : Snailfish_Number_Access;
      Second_Value : Snailfish_Number_Access;
      Pair_Sum  : Snailfish_Number_Access;
      Magnitude : Integer;

      Max_Magnitude  : Integer := 0;
   begin
      Left_Loop:
      for L of Input loop
         Right_Loop:
         for R of Input loop
            if L /= R then
               First_Value := Get_Snailfish_Number(L);
               Second_Value := Get_Snailfish_Number(R);
               Pair_Sum := Sum(First_Value, Second_Value);
               Reduce(Pair_Sum);
               Magnitude := Calculate_Magnitude(Pair_Sum);
               if Magnitude > Max_Magnitude then
                  Max_Magnitude := Magnitude;
               end if;
            end if;
         end loop Right_Loop;
      end loop Left_Loop;

      return Max_Magnitude;
   end Find_Max_Magnitude;

   Homework : Numbers_List.Vector;
   Answer   : Integer := 0;
begin
   Load_File(Homework);

   Answer := Find_Max_Magnitude(Homework);

   Put_line("The largest magnitude of any sum of two different snailfish "
            & "numbers is " & Integer'Image(Answer) & ".");
end Day18_2;
