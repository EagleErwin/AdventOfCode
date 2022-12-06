with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day06_1 is

   procedure Load_File (Input_Data : out Unbounded_String) is
      Input : File_Type;
   begin
      Open (File => Input, Mode => In_File, Name => "data/day06.input");
      while not End_Of_File (Input) loop
         Input_Data := To_Unbounded_String(Get_Line(Input));
      end loop;
      Close (File => Input);
   end Load_File;

   function Find_Start_Of_Packet_Marker (Input_Data : in Unbounded_String)
                                         return Integer is
      A : Character;
      B : Character;
      C : Character;
      D : Character;

      Answer : Integer := -1;
   begin
      Sliding_Window_Loop:
      for I in 1 .. Length(Input_Data) loop
         A := Element(Input_Data, I);
         B := Element(Input_Data, I + 1);
         C := Element(Input_Data, I + 2);
         D := Element(Input_Data, I + 3);

         if (not (A = B or A = C or A = D or B = C or B = D or C = D)) then
            Answer := I + 3;
            exit Sliding_Window_Loop;
         end if;
      end loop Sliding_Window_Loop;
      return Answer;
   end Find_Start_Of_Packet_Marker;

   Input_Data : Unbounded_String;
   Answer : Integer;
begin
   Load_File(Input_Data);

   Answer := Find_Start_Of_Packet_Marker(Input_Data);

   Put_Line(Integer'Image(Answer) & " characters needed to be processed before "
           & "the first start-of-packet marker was detected.");
end Day06_1;
