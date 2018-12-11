with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
procedure Day08_1 is
   Max_Number_Of_Leafs : constant Integer := 25; --Estimation
   Max_Metadata_Numbers : constant Integer := 25; --Estimation
   subtype Data_Line is Unbounded_String;
   
   type Node;
   type Access_Node is access all Node;
   
   subtype Metadata is Integer range -1..100; -- -1 is used for no_data
   type Metadata_Array is array (1 .. Max_Metadata_Numbers) of Metadata;
   type Leaf_Array is array (1 .. Max_Number_Of_Leafs) of Access_Node;
                             
   type Node is record
      Data : Metadata_Array;
      Leafs : Leaf_Array;
   end record;

   procedure Load_File (Data : out Data_Line) is
      Input        : File_Type;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day08.input");
      Data := To_Unbounded_String (Get_Line (Input));
      
      Close (File => Input);
   end Load_File;   
   
   function Get_Next_Number (Input_String : in out Unbounded_String) return Integer is
      Current_Char : Character;
      Buffer : Unbounded_String;
   begin
      Get_Character:
      while Length(Input_String) /= 0 and then Element(Input_String, 1) /= ' ' loop
         Current_Char := Element(Input_String, 1);
         Buffer := Buffer & Current_Char;
         Input_String := Delete(Input_String, 1, 1);
      end loop Get_Character;
      if Length(Input_String) /= 0 then
         Input_String := Delete(Input_String, 1, 1); -- Delete the space
      end if;
      return Integer'Value(To_String(Buffer));
   end Get_Next_Number;
   
   function Get_Node(Input_String : in out Unbounded_String) return Access_Node is
      Num_Child_Nodes : Integer;
      Num_Metadata_Entries : Integer;
      Result : Access_Node := new Node'(Data => (others => -1), Leafs => (others => null));
   begin
      Num_Child_Nodes := Get_Next_Number(Input_String => Input_String);
      Num_Metadata_Entries := Get_Next_Number(Input_String => Input_String);
      Get_Child_Nodes:
      for I in Integer range 1 .. Num_Child_Nodes loop
         Result.Leafs(I) := Get_Node(Input_String => Input_String);
      end loop Get_Child_Nodes;
      Get_Metadata:
      for I in Integer range 1 .. Num_Metadata_Entries loop
         Result.Data(I) := Get_Next_Number(Input_String => Input_String);
      end loop Get_Metadata;     
      return Result;
   end Get_Node;

   function Get_Metadata_For_One_Node(Input : in Access_Node) return Integer is
      The_Metadata : Metadata_Array;
      Sum : Integer := 0;
   begin
      The_Metadata := Input.Data;
      Get_The_Data:
      for D in The_Metadata'Range loop
         exit when The_Metadata(D) = -1;
         Sum := Sum + The_Metadata(D);
      end loop Get_The_Data;
      return Sum;
   end Get_Metadata_For_One_Node;
   
   function Get_Total_Metadata(Input : in Access_Node) return Integer is
      Sum : Integer := 0;
   begin
      Leafs_Loop:
      for I in Input.Leafs'Range loop
         if Input.Leafs(I) /= null then
            Sum := Sum + Get_Total_Metadata(Input.Leafs(I));
         end if;
      end loop Leafs_Loop;
      
      return Sum + Get_Metadata_For_One_Node(Input);
   end Get_Total_Metadata;
   
   Input_Data   : Data_Line;
   Answer : Integer := 0;
   Root : Access_Node;
begin
   Load_File(Data => Input_Data);

   Root := Get_Node(Input_String => Input_Data);
   Answer := Get_Total_Metadata(Root);
   
   Put_Line("Final result: " & Integer'Image(Answer));
end Day08_1;
