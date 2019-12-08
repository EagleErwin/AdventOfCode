with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;

procedure Day06_1 is
   -- ### CONSTANTS ### --
   Object_Capacity : constant Integer := 3_000; -- Maximum number of supported space objects

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is String (1 .. 7);

   subtype Space_Object_Name is String (1 .. 3);
   type Space_Object;
   type Space_Object_Access is access Space_Object;
   type Space_Object is record
      Name    : Space_Object_Name;
      Parent  : Space_Object_Access;
   end record;

   type Space_Array is array (1 .. Object_Capacity) of Space_Object_Access;
   type Space_Model is record
      Numer_Of_Objects : Integer;
      Object_Array     : Space_Array;
   end record;

   procedure Load_File (Output_Model : out Space_Model) is
      Input         : File_Type;
      Current_Line  : Data_Line;
      Output_Array  : Space_Array;
      Idx           : Integer := 0;
      First_Name    : Space_Object_Name;
      Second_Name   : Space_Object_Name;

      First_Object  : Space_Object_Access;
      Second_Object : Space_Object_Access;

      Tmp_Object    : Space_Object_Access;

      First_Found   : Boolean := False;
      Second_Found  : Boolean := False;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day06.input");
      Read_File_Loop:
      while not End_Of_File (Input) loop
         -- Reset state
         First_Found   := False;
         Second_Found  := False;
         First_Object  := null;
         Second_Object := null;

         -- Add the node
         Current_Line := Get_Line(Input);
         First_Name := Head(Current_Line, 3); -- Parent
         Second_Name := Tail(Current_Line, 3); -- Child

         Objects_Loop:
         for O in 1 .. Idx loop
            Tmp_Object := Output_Array(O);
            if Tmp_Object.Name = First_Name then
               First_Object := Tmp_Object;
               First_Found := True;
            elsif Tmp_Object.Name = Second_Name then
               Second_Object := Tmp_Object;
               Second_Found := True;
            end if;
         end loop Objects_Loop;

         if not First_Found then
            First_Object := new Space_Object'(Name => First_Name,
                                              Parent => null);
            Idx := Idx + 1;
            Output_Array(Idx) := First_Object;
            --Put_Line("Added " & First_Name & " without parent" & " on position " & Integer'Image(Idx));
         end if;

         if Second_Found then
            Second_Object.Parent := First_Object;
            --Put_Line("Updating " & Second_Object.Name & " with " & Second_Object.Parent.Name & " as parent");
         else
            Second_Object := new Space_Object'(Name => Second_Name,
                                               Parent => First_Object);
            Idx := Idx + 1;
            Output_Array(Idx) := Second_Object;
            --Put_Line("Added " & Second_Name & " without parent" & " on position " & Integer'Image(Idx));
         end if;
      end loop Read_File_Loop;

      Output_Model := Space_Model'(Numer_Of_Objects => Idx,
                                   Object_Array     => Output_Array);

      Close (File => Input);
   end Load_File;

   function Solve ( Model : in Space_Model ) return Integer is
      Result           : Integer := 0;
      Distance_To_Root : Integer := 0;
      Tmp_Object       : Space_Object_Access;
      Tmp_Parent       : Space_Object_Access;
   begin
      for I in 1 .. Model.Numer_Of_Objects loop
         Distance_To_Root := 0;
         Tmp_Object := Model.Object_Array(I);
         Tmp_Parent := Tmp_Object.Parent;

         while Tmp_Parent /= null loop
            Distance_To_Root := Distance_To_Root + 1;
            Tmp_Parent := Tmp_Parent.Parent;
         end loop;
         --Put_Line("Distance from " & Tmp_Object.Name & " to root is" & Integer'Image(Distance_To_Root));
         Result := Result + Distance_To_Root;
      end loop;
      return Result;
   end Solve;

   Universe : Space_Model;
   Answer   : Integer := 0;
begin
   Load_File (Output_Model => Universe);

   Answer := Solve (Model => Universe);

   Put_Line("Total number of direct and indirect orbits:" & Integer'Image(Answer));
end Day06_1;
