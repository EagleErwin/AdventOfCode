with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day14_1 is
   -- ### CONSTANTS ### --
   Nr_Of_Steps : constant Natural := 10;

   -- ### TYPE DEFINITIONS ### --
   subtype Pair_String is String (1 .. 2);

   function Pair_Hash (Input : Pair_String) return Hash_Type is
   begin
      return Hash(Input);
   end Pair_Hash;

   function Char_Hash (Input : Character) return Hash_Type is
   begin
      return Hash((1 => Input));
   end Char_Hash;

   package Pair_Map is new Hashed_Maps(Key_Type        => Pair_String,
                                       Element_Type    => Character,
                                       Hash            => Pair_Hash,
                                       Equivalent_Keys => "=");
   use Pair_Map;

   package Element_Map is new Hashed_Maps(Key_Type        => Character,
                                              Element_Type    => Integer,
                                              Hash            => Char_Hash,
                                              Equivalent_Keys => "=");
   use Element_Map;

   type Polymer_Element;
   type Polymer_Element_Access is access Polymer_Element;
   type Polymer_Element is record
      Letter : Character;
      Next : Polymer_Element_Access;
   end record;

   function Create_Initial_Polymer(Input : in Unbounded_String)
                                   return Polymer_Element_Access is
      Previous_Element : Polymer_Element_Access;
      Current_Element : Polymer_Element_Access;
   begin
      Previous_Element := null;
      Read_Loop:
      for I in reverse 1 .. Length(Input) loop
         Current_Element := new Polymer_Element'(Letter => Element(Input, I),
                                                 Next   => Previous_Element);
         Previous_Element := Current_Element;
      end loop Read_Loop;

      return Current_Element;
   end Create_Initial_Polymer;

   procedure Parse_Rule(Input  : in Unbounded_String;
                        Output : in out Pair_Map.Map) is
      Current_Key : Pair_String;
      Current_Val : Character;
   begin
      Current_Key := Slice(Input, 1, 2);
      Current_Val := Element(Input, Length(Input));
      Output.Insert(Current_Key, Current_Val);
   end Parse_Rule;

   procedure Load_File (First_Element  : out Polymer_Element_Access;
                        Insertion_Rules : out Pair_Map.Map) is
      Input              : File_Type;
      Current_Line       : Unbounded_String;
   begin
      Insertion_Rules := Pair_Map.Empty_Map;
      Open (File => Input, Mode => In_File, Name => "data/day14.input");
      -- Read the line with the initial polymer
      Current_Line := To_Unbounded_String(Get_Line(Input));
      First_Element := Create_Initial_Polymer(Current_Line);

      -- Read the empty line.
      Current_Line := To_Unbounded_String(Get_Line(Input));
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Parse_Rule(Current_Line, Insertion_Rules);
      end loop;
      Close (File => Input);
   end Load_File;

   procedure Grow (First_Element   : in out Polymer_Element_Access;
                   Insertion_Rules : in Pair_Map.Map) is
      Current_Element : Polymer_Element_Access := First_Element;

      First_Char  : Character;
      Second_Char : Character;
      Pair        : Pair_String;

      New_Element : Polymer_Element_Access;
   begin
      Insert_Loop:
      while Current_Element.Next /= null loop
         First_Char := Current_Element.Letter;
         Second_Char := Current_Element.Next.Letter;
         Pair := (1 => First_Char, 2 => Second_Char);
         if Insertion_Rules.Contains(Pair) then
            New_Element := new Polymer_Element'(Letter => Insertion_Rules(Pair),
                                                Next => Current_Element.Next);
            Current_Element.Next := New_Element;
         else
            Put_Line("ERROR! Invalid pair " & Pair);
         end if;
         Current_Element := New_Element.Next;

      end loop Insert_Loop;
   end Grow;

   procedure Print_Polymer(First_Element : in Polymer_Element_Access) is
      Current_Element : Polymer_Element_Access := First_Element;
   begin
      Put(Current_Element.Letter);
      Print_Loop:
      while Current_Element.Next /= null loop
         Current_Element := Current_Element.Next;
         Put(Current_Element.Letter);
      end loop Print_Loop;
      Put_Line("");
   end Print_Polymer;

   procedure Find_Elements (First_Element : in Polymer_Element_Access;
                            Most_Common   : out Integer;
                            Least_Common  : out Integer) is
      Elements        : Element_Map.Map := Element_Map.Empty_Map;
      Current_Element : Polymer_Element_Access := First_Element;
   begin
      Most_Common  := 0;
      Least_Common := Integer'Last;

      Elements.Insert(Current_Element.Letter, 1);

      Count_Loop:
      while Current_Element.Next /= null loop
         Current_Element := Current_Element.Next;
         if not Elements.Contains(Current_Element.Letter) then
            Elements.Insert(Current_Element.Letter, 0);
         end if;
         Elements(Current_Element.Letter) := Elements(Current_Element.Letter) + 1;
      end loop Count_Loop;

      Find_Loop:
      for E in Elements.Iterate loop
         if Element(E) < Least_Common then
            Least_Common := Element(E);
         end if;
         if Element(E) > Most_Common then
            Most_Common := Element(E);
         end if;
      end loop Find_Loop;

   end Find_Elements;

   First_Element   : Polymer_Element_Access;
   Insertion_Rules : Pair_Map.Map;

   Q_Most_Common_Element  : Integer;
   Q_Least_Common_Element : Integer;
   Answer          : Integer;
begin
   Load_File(First_Element, Insertion_Rules);

   Grow_Loop:
   for I in 1 .. Nr_Of_Steps loop
      Grow(First_Element, Insertion_Rules);
   end loop Grow_Loop;

   --Print_Polymer(First_Element);

   Find_Elements(First_Element, Q_Most_Common_Element, Q_Least_Common_Element);

   Answer := Q_Most_Common_Element - Q_Least_Common_Element;

   Put_line("The quantity of the most common element ("
            & Integer'Image(Q_Most_Common_Element)
            & ") minus the quantity of the least common element ("
            & Integer'Image(Q_Least_Common_Element) & ") is "
            & Integer'Image(Answer) & ".");
end Day14_1;
