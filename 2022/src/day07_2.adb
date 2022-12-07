with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings;              use Ada.Strings;

procedure Day07_2 is
   -- ### CONSTANTS ### --
   Max_Nr_Of_Files     : constant Integer := 100; -- Should be large enough.
   Size_Threshold      : constant Integer := 100_000;
   Filesystem_Capacity : constant Integer := 70_000_000;
   Required_Space      : constant Integer := 30_000_000;

   -- ### TYPE DEFINITIONS ### --
   type Data_Item_Type is (Directory, File);

   type Data_Item;
   type Data_Item_Access is access Data_Item;
   type Contents_Array is array (1 .. Max_Nr_Of_Files) of Data_Item_Access;

   type Data_Item is record
      Item_Type   : Data_Item_Type;
      Name        : Unbounded_String;
      Size        : Integer;
      Parent      : Data_Item_Access;
      Nr_Of_Items : Integer;
      Contents    : Contents_Array;
   end record;

   procedure Load_File (Root : out Data_Item_Access) is
      Input          : File_Type;
      Current_Line   : Unbounded_String;
      Current_Dir    : Data_Item_Access;
      New_Item       : Data_Item_Access;
      New_Item_Name  : Unbounded_String;
      New_Item_Size  : Integer;
      Space_Char_Pos : Integer;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day07.input");
      -- The first line is assumed to be the root.
      Current_Line := To_Unbounded_String(Get_Line(Input));
      Root := new Data_Item'(Directory, To_Unbounded_String("C:"), 0, null, 0, (others => null));
      Current_Dir := Root;

      Current_Line := To_Unbounded_String(Get_Line(Input));
      Parse_Input_File_Loop:
      while not End_Of_File (Input) loop
         if Element(Current_Line, 1) = '$' then
            -- Either cd or ls
            if (Element(Current_Line, 3) = 'l' and then
                Element(Current_Line, 4) = 's') then
               -- ls
               Current_Line := To_Unbounded_String(Get_Line(Input));
               List_Contents_Loop:
               while (Element(Current_Line, 1) /= '$') loop
                  if (Element(Current_Line, 1) = 'd' and then
                      Element(Current_Line, 2) = 'i' and then
                      Element(Current_Line, 3) = 'r') then
                     -- Directory.
                     New_Item_Name := To_Unbounded_String(Slice(Current_Line, 5, Length(Current_Line)));
                     New_Item := new Data_Item'(Directory, New_Item_Name, 0, Current_Dir, 0, (others => null));
                  else
                     -- File.
                     Space_Char_Pos := Index(Current_Line, " ");
                     New_Item_Size := Integer'Value(Slice(Current_Line, 1, Space_Char_Pos - 1));
                     New_Item_Name := To_Unbounded_String(Slice(Current_Line, Space_Char_Pos + 1, Length(Current_Line)));
                     New_Item := new Data_Item'(File, New_Item_Name, New_Item_Size, Current_Dir, 0, (others => null));
                  end if;
                  Current_Dir.Nr_Of_Items := Current_Dir.Nr_Of_Items + 1;
                  Current_Dir.Contents(Current_Dir.Nr_Of_Items) := New_Item;

                  if (not End_Of_File(Input)) then
                     Current_Line := To_Unbounded_String(Get_Line(Input));
                  else
                     exit List_Contents_Loop;
                  end if;
               end loop List_Contents_Loop;
            elsif (Element(Current_Line, 3) = 'c' and then
                   Element(Current_Line, 4) = 'd') then
               --cd
               if (Element(Current_Line, 6) = '.' and then
                   Element(Current_Line, 7) = '.') then
                  Current_Dir := Current_Dir.Parent;
               else
                  Space_Char_Pos := Index(Current_Line, " ", Backward); -- Assume no space in a folder name.
                  New_Item_Name := To_Unbounded_String(Slice(Current_Line, Space_Char_Pos + 1, Length(Current_Line)));
                  Find_Dir_Loop:
                  for I in 1 .. Current_Dir.Nr_Of_Items loop
                     if (Current_Dir.Contents(I).Name = New_Item_Name) then
                        Current_Dir := Current_Dir.Contents(I);
                        exit Find_Dir_Loop;
                     end if;
                  end loop Find_Dir_Loop;
               end if;
               Current_Line := To_Unbounded_String(Get_Line(Input));
            else
               Put_Line("This should not happen");
               Current_Line := To_Unbounded_String(Get_Line(Input));
            end if;
         end if;
      end loop Parse_Input_File_Loop;
      Close (File => Input);
   end Load_File;

   procedure Print_Filesystem(Root  : in Data_Item_Access;
                              Level : in Integer) is
   begin
      Indent_Loop:
      for I in 1 .. Level loop
         Put(' ');
      end loop Indent_Loop;
      Put("- " & To_String(Root.Name));

      case Root.Item_Type is
         when Directory => Put_Line(" (dir, size = " & Integer'Image(Root.Size) & ")");
         when File      => Put_Line(" (file, size = " & Integer'Image(Root.Size) & ")");
      end case;

      Leaves_Loop:
      for N in 1 .. Root.Nr_Of_Items loop
         Print_Filesystem(Root.Contents(N), Level + 1);
      end loop Leaves_Loop;
   end Print_Filesystem;

   procedure Fill_Dir_Sizes(Root : in out Data_Item_Access) is
      Size : Integer := 0;
   begin
      Leaves_Loop:
      for N in 1 .. Root.Nr_Of_Items loop
         if (Root.Contents(N).Item_Type = Directory) then
            Fill_Dir_Sizes(Root.Contents(N));
         end if;
         Size := Size + Root.Contents(N).Size;
      end loop Leaves_Loop;
      Root.Size := Size;
   end Fill_Dir_Sizes;

   function Find_Dir_To_Remove (Root             : in Data_Item_Access;
                                Amount_To_Remove : in Integer) return Integer is
      Best_Candidate : Integer := Filesystem_Capacity;
      Child_Size     : Integer;
      Answer         : Integer := 0;
   begin
      if (Root.Item_Type = Directory) then
         if (Root.Size >= Amount_To_Remove) then
            Best_Candidate := Root.Size;
            Leaves_Loop:
            for N in 1 .. Root.Nr_Of_Items loop
               Child_Size := Find_Dir_To_Remove(Root.Contents(N), Amount_To_Remove);
               if (Child_Size < Best_Candidate) then
                  Best_Candidate := Child_Size;
               end if;
            end loop Leaves_Loop;
         end if;
      end if;
      return Best_Candidate;
   end Find_Dir_To_Remove;

   Filesystem : Data_Item_Access;

   Free_Space : Integer;
   Required_Extra_Space : Integer;
   Answer     : Integer := 0;
begin
   Load_File(Filesystem);

   Fill_Dir_Sizes(Filesystem);

   --Print_Filesystem(Filesystem, 1);

   Free_Space := Filesystem_Capacity - Filesystem.Size;
   Required_Extra_Space := Required_Space - Free_Space;
   Put_Line("Current free space: " & Integer'Image(Free_Space));
   Put_Line("Extra free space needed: " & Integer'Image(Required_Extra_Space));

   Answer := Find_Dir_To_Remove(Filesystem, Required_Extra_Space);

   Put_Line("The smallest directory we can remove to free up enough space is "
            & Integer'Image(Answer));
end Day07_2;
