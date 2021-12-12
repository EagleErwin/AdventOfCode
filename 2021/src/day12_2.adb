with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;     use Ada.Containers;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Characters.Handling;    use Ada.Characters.Handling;

procedure Day12_2 is
   -- ### CONSTANTS ### --
   -- ### TYPE DEFINITIONS ###
   function Unbounded_String_Hash (Strng : Unbounded_String) return Hash_Type is
   begin
      return Hash(To_String(Strng));
   end Unbounded_String_Hash;

   type Cave_Size is (Small, Large);

   type Node;
   type Node_Access is access Node;

   package Node_List is new Vectors(Index_Type   => Natural,
                                    Element_Type => Node_Access);
   use Node_List;

   package Visits_List is new Hashed_Maps(Key_Type        => Unbounded_String,
                                          Element_Type    => Integer,
                                          Hash            => Unbounded_String_Hash,
                                          Equivalent_Keys => "=" );
   use Visits_List;

   type Node is record
      Name       : Unbounded_String;
      Size       : Cave_Size;
      Neighbours : Node_List.Vector;
   end record;

   function Get_Size(Node_Name : in Unbounded_String) return Cave_Size is
      First_Char : Character;
   begin
      First_Char := Element(Node_Name, 1);
      if First_Char /= To_Lower(First_Char) then
         return Large;
      else
         return Small;
      end if;
   end Get_Size;

   procedure Load_File (Start_Node : out Node_Access) is
      Input         : File_Type;
      Current_Line  : Unbounded_String;
      Nodes         : Node_List.Vector := Node_List.Empty_Vector;
      First_Name    : Unbounded_String;
      Second_Name   : Unbounded_String;
      Separator_Idx : Integer;

      First_Node  : Node_Access;
      Second_Node : Node_Access;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day12.input");
      while not End_Of_File (Input) loop
         First_Node := null;
         Second_Node := null;

         Current_Line := To_Unbounded_String(Get_Line(Input));
         Separator_Idx := Index(Current_Line, "-");
         First_Name := To_Unbounded_String(Slice(Current_Line, 1, Separator_Idx - 1));
         Second_Name := To_Unbounded_String(Slice(Current_Line, Separator_Idx + 1, Length(Current_Line)));

         Find_Node_Loop:
         for N of Nodes loop
            if N.Name = First_Name then
               First_Node := N;
            end if;
            if N.Name = Second_Name then
               Second_Node := N;
            end if;
         end loop Find_Node_Loop;

         if First_Node = null then
            First_Node := new Node'(Name       => First_Name,
                                    Size       => Get_Size(First_Name),
                                    Neighbours => Node_List.Empty_Vector);
            Nodes.Append(First_Node);
         end if;
         if Second_Node = null then
            Second_Node := new Node'(Name       => Second_Name,
                                     Size       => Get_Size(Second_Name),
                                     Neighbours => Node_List.Empty_Vector);
            Nodes.Append(Second_Node);
         end if;

         First_Node.Neighbours.Append(Second_Node);
         Second_Node.Neighbours.Append(First_Node);
      end loop;
      Close (File => Input);

      Find_Start_Loop:
      for M of Nodes loop
         if M.Name = "start" then
            Start_Node := M;
            exit Find_Start_Loop;
         end if;
      end loop Find_Start_Loop;
   end Load_File;

   -- Returns how many times the Subject is visited.
   function Get_Visits(Visited_Nodes : in Visits_List.Map;
                       Subject       : in Node_Access) return Integer is
   begin
      if Visited_Nodes.Contains(Subject.Name) then
         return Visited_Nodes(Subject.Name);
      else
         return 0;
      end if;
   end Get_Visits;

   -- Returns whether any small cave is visited at least twice.
   -- It does not take start or end into account.
   function Is_Any_Small_Cave_Visited_Twice(Visited_Nodes : in Visits_List.Map) return Boolean is
   begin
      Visits_Loop:
      for V in Visited_Nodes.Iterate loop
         if Get_Size(Key(V)) = Small and then
           Key(V) /= "start" and then
           Key(V) /= "end" and then
           Element(V) >= 2 then
            return True;
         end if;
      end loop Visits_Loop;
      return False;
   end Is_Any_Small_Cave_Visited_Twice;

   function Find_Paths(Start_Node    : in Node_Access;
                       Visited_Nodes : in Visits_List.Map;
                       History       : in Unbounded_String) return Integer is
      New_Visited_Nodes : Visits_List.Map := Visited_Nodes;
      Visits_Updated : Boolean := False;
      Do_Visit : Boolean := False;
      Result : Integer := 0;
   begin
      -- Check if we are the end node
      if Start_Node.Name = "end" then
         New_Visited_Nodes.Insert(Start_Node.Name, 2);
         return 1;
      else
         -- Update the number of visits for this node if it is a small node
         if Start_Node.Size = Small then
            if New_Visited_Nodes.Contains(Start_Node.Name) then
               New_Visited_Nodes(Start_Node.Name) := New_Visited_Nodes(Start_Node.Name) + 1;
            else
               New_Visited_Nodes.Insert(Start_Node.Name, 1);
            end if;
         end if;
      end if;

      -- Find the paths via each of the non-visited or large neighbours
      Neighbour_Loop:
      for N of Start_Node.Neighbours loop
         if N.Name /= "start" then
            Do_Visit := False;
            if N.Size = Large then
               -- Always visit a large neighbour
               Do_Visit := True;
            else
               if New_Visited_Nodes.Contains(N.Name) then
                  --  if New_Visited_Nodes(N.Name) < 2 then
                  if not Is_Any_Small_Cave_Visited_Twice(New_Visited_Nodes) then
                     -- Visit the small neighbour if it has visited max once before.
                     Do_Visit := True;
                  end if;
               else
                  -- Visit the small neighbour if it has never been visited before.
                  Do_Visit := True;
               end if;
            end if;
            if Do_Visit then
               Result := Result + Find_Paths(N, New_Visited_Nodes, History & "," & N.Name);
            end if;
         end if;
      end loop Neighbour_Loop;
      return Result;
   end Find_Paths;

   Start_Node : Node_Access;
   Visits     : Visits_List.Map;
   Answer     : Integer := 0;
begin
   Load_File(Start_Node);

   -- The start-node should not be visited again, so set number of visits to 1 already.
   Visits.Insert(Start_Node.Name, 1);
   Answer := Find_Paths(Start_Node, Visits, Start_Node.Name);

   Put_line("There are " & Integer'Image(Answer) & " paths trough this cave "
            & "system with the new rules.");
end Day12_2;
