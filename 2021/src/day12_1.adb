with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Hash;        use Ada.Strings;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;  use Ada.Containers;
with Ada.Characters.Handling; use Ada.Characters.Handling;

procedure Day12_1 is
   -- ### CONSTANTS ### --
   Number_Of_Steps : constant Integer := 100; --100

   -- ### TYPE DEFINITIONS ### --
   type Cave_Size is (Small, Large);

   type Node;
   type Node_Access is access Node;

   package Node_List is new Vectors(Index_Type   => Natural,
                                    Element_Type => Node_Access);
   use Node_List;

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
      Second_Node  : Node_Access;
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
                                     Size        => Get_Size(Second_Name),
                                     Neighbours  => Node_List.Empty_Vector);
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

   function Find_Paths(Start_Node    : in Node_Access;
                       Visited_Nodes : in Node_List.Vector) return Integer is
      New_Visited_Nodes : Node_List.Vector := Visited_Nodes;
      Result : Integer := 0;
   begin
      -- Check if we are the end node
      if Start_Node.Name = "end" then
         return 1;
      end if;
      -- Add this node to the visited nodes list
      if Start_Node.Size = Small then
         New_Visited_Nodes.Append(Start_Node);
      end if;

      -- Find the paths via each of the non-visited or large neighbours
      Neighbour_Loop:
      for N of Start_Node.Neighbours loop
         if N.Size = Large or else not Visited_Nodes.Contains(N) then
            Result := Result + Find_Paths(N, New_Visited_Nodes);
         end if;
      end loop Neighbour_Loop;
      return Result;
   end Find_Paths;

   Start_Node : Node_Access;
   Answer     : Integer := 0;
begin
   Load_File(Start_Node);

   Answer := Find_Paths(Start_Node, Node_List.Empty_Vector);

   Put_line("There are " & Integer'Image(Answer) & " paths trough this cave "
            & "system that visit small caves at most once.");
end Day12_1;
