with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;

procedure Day06_2 is
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

   function Get_Number_Of_Common_Parents(First_Object : in Space_Object_Access;
                                         Second_Object : in Space_Object_Access) return Integer is
      type Parent_Array is array (1 .. 500) of Space_Object_Access;

      Parents : Parent_Array := (others => null);
      Number_Of_Parents : Integer := 0;
      Tmp_Object : Space_Object_Access;

      Result : Integer := 0;
   begin
      Tmp_Object := First_Object.Parent;
      Add_Parents_Of_First_Loop:
      while Tmp_Object /= null loop
         Number_Of_Parents := Number_Of_Parents + 1;
         Parents(Number_Of_Parents) := Tmp_Object;
         Tmp_Object := Tmp_Object.Parent;
      end loop Add_Parents_Of_First_Loop;

      Tmp_Object := Second_Object.Parent;
      Remove_Parents_Of_Second_Loop:
      while Tmp_Object /= null loop
         Existence_Check_Loop:
         for I in 1 .. Number_Of_Parents loop
            if Parents(I).Name = Tmp_Object.Name then
               Result := Result + 1;
            end if;
         end loop Existence_Check_Loop;
         Tmp_Object := Tmp_Object.Parent;
      end loop Remove_Parents_Of_Second_Loop;

      return Result;
   end Get_Number_Of_Common_Parents;

   function Get_Number_Of_Parents(Subject : in Space_Object_Access) return Integer is
      Result : Integer := 0;
   begin
      if Subject = null then
         return -1;
      else
         return 1 + Get_Number_Of_Parents(Subject => Subject.Parent);
      end if;
   end Get_Number_Of_Parents;

   function Solve ( Model : in Space_Model ) return Integer is
      Result           : Integer := 0;
      Tmp_Object       : Space_Object_Access;
      Src_Object       : Space_Object_Access;
      Target_Object    : Space_Object_Access;

      Nr_Of_Common_Parents : Integer := 0;
      Nr_Of_Src_Parents : Integer;
      Nr_Of_Target_Parents : Integer;
   begin
      Find_Objects_Loop:
      for I in 1 .. Model.Numer_Of_Objects loop
         Tmp_Object := Model.Object_Array(I);
         if Tmp_Object.Name = "YOU" then
            Src_Object := Tmp_Object;
         elsif Tmp_Object.Name = "SAN" then
            Target_Object := Tmp_Object;
         end if;
      end loop Find_Objects_Loop;

      Nr_Of_Src_Parents := Get_Number_Of_Parents(Src_Object);
      Nr_Of_Target_Parents := Get_Number_Of_Parents(Target_Object);
      Nr_Of_Common_Parents := Get_Number_Of_Common_Parents(Src_Object, Target_Object);

      --Put_Line("Parents of YOU: " & Integer'Image(Nr_Of_Src_Parents));
      --Put_Line("Parents of SAN: " & Integer'Image(Nr_Of_Target_Parents));
      --Put_Line("Common parents: " & Integer'Image(Nr_Of_Common_Parents));
      Result := Nr_Of_Src_Parents + Nr_Of_Target_Parents - (2 * Nr_Of_Common_Parents);
      return Result;
   end Solve;

   Universe : Space_Model;
   Answer   : Integer := 0;
begin
   Load_File (Output_Model => Universe);

   Answer := Solve (Model => Universe);

   Put_Line("Required transfers:" & Integer'Image(Answer));
end Day06_2;
