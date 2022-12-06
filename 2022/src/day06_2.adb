with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Day06_2 is

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
      E : Character;
      F : Character;
      G : Character;
      H : Character;
      I : Character;
      J : Character;
      K : Character;
      L : Character;
      M : Character;
      N : Character;

      Answer : Integer := -1;
   begin
      Sliding_Window_Loop:
      for X in 1 .. Length(Input_Data) loop
         A := Element(Input_Data, X);
         B := Element(Input_Data, X + 1);
         C := Element(Input_Data, X + 2);
         D := Element(Input_Data, X + 3);
         E := Element(Input_Data, X + 4);
         F := Element(Input_Data, X + 5);
         G := Element(Input_Data, X + 6);
         H := Element(Input_Data, X + 7);
         I := Element(Input_Data, X + 8);
         J := Element(Input_Data, X + 9);
         K := Element(Input_Data, X + 10);
         L := Element(Input_Data, X + 11);
         M := Element(Input_Data, X + 12);
         N := Element(Input_Data, X + 13);

         if (not (A = B or A = C or A = D or A = E or A = F or A = G or A = H or A = I or A = J or A = K or A = L or A = M or A = N or
                    B = C or B = D or B = E or B = F or B = G or B = H or B = I or B = J or B = K or B = L or B = M or B = N or
                      C = D or C = E or C = F or C = G or C = H or C = I or C = J or C = K or C = L or C = M or C = N or
                        D = E or D = F or D = G or D = H or D = I or D = J or D = K or D = L or D = M or D = N or
                          E = F or E = G or E = H or E = I or E = J or E = K or E = L or E = M or E = N or
                            F = G or F = H or F = I or F = J or F = K or F = L or F = M or F = N or
                              G = H or G = I or G = J or G = K or G = L or G = M or G = N or
                                H = I or H = J or H = K or H = L or H = M or H = N or
                                  I = J or I = K or I = L or I = M or I = N or
                                    J = K or J = L or J = M or J = N or
                                      K = L or K = M or K = N or
                                        L = M or L = N or
                                          M = N)) then
            Answer := X + 13;
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
           & "the first start-of-message marker was detected.");
end Day06_2;
