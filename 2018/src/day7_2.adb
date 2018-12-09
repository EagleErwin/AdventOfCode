with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
procedure Day7_2 is
   File_Name : constant String := "data/day7.input";
   
   Blocker_Pos : constant Integer := 6; --Character position of the blocker in the input string
   Waiter_Pos : constant Integer := 37; --Character position of the blocked in the input string
   Max_Number_Of_Steps : constant Integer := 26; -- The number of letters in the alphabet
   
   Number_Of_Workers : constant Integer := 5;
   Duration_Offset : constant Integer := 60; -- Execution time added to each Step
   ASCII_Offset : constant Integer := 64; -- ASCII value minus this constant is Character-specific execution time for Step

   Maximum_Duration : constant Integer := Max_Number_Of_Steps * (Duration_Offset + Max_Number_Of_Steps);

   subtype Data_Line is Unbounded_String;
   subtype Alphabet is Character range 'A' .. 'Z';
   
   subtype Num_Blockers_Type is Integer range 0 .. Max_Number_Of_Steps;
   subtype Time_Type is Integer range 0 .. Maximum_Duration;

   type Step;
   type Step_Access is access Step;
   type Step_Array is array (1 .. Max_Number_Of_Steps) of Step_Access;
   type Step is record
      Id : Alphabet;
      Duration : Time_Type;
      Num_Blockers : Num_Blockers_Type;
      Blockers : Step_Array;
   end record;
   
   type Data_Array is array (Alphabet) of Step_Access;

   type Schedule is array (Time_Type'Range) of Step_Access;
   type Factory is array (Integer range 1 .. Number_Of_Workers) of Schedule;

   procedure Add_Blocker (Subject : in out Step_Access; Blocker : in Step_Access) is
   begin
      Subject.Num_Blockers := Subject.Num_Blockers + 1;
      Subject.Blockers(Subject.Num_Blockers) := Blocker;
   end Add_Blocker;

   procedure Load_File (Data : out Data_Array) is
      Current_Line : Data_Line;
      Input        : File_Type;
      Current_Step : Character;
      Current_Blocker : Character;
   begin
      -- Fill the Data array with initial values
      for L in Alphabet loop
         Data(L) := new Step'(Id => L,
                              Duration => Duration_Offset + Character'Pos(L) - ASCII_Offset,
                              Num_Blockers => 0,
                              Blockers => (others => null));
      end loop;

      Open (File => Input, Mode => In_File, Name => File_Name);
      while not End_Of_File (Input) loop
         Current_Line   := To_Unbounded_String (Get_Line (Input));
         Current_Step := Element(Current_Line, Waiter_Pos);
         Current_Blocker := Element(Current_Line, Blocker_Pos);
         Add_Blocker(Data(Current_Step), Data(Current_Blocker));
      end loop;
      Close (File => Input);
   end Load_File;   
   
   function Steps_Left(Input_Data : in Data_Array) return Boolean is
      Result : Boolean := False;
   begin
      for C in Input_Data'Range loop
         if Input_Data(C) /= null then
            Result := True;
         end if;
      end loop;
      return Result;
   end Steps_Left;

   function Get_Available_Worker(Workers : in Factory;
                                 Current_Time : in Time_Type;
                                 Available_Worker_Id : out Integer) return Boolean is
   begin
      Factory_Loop:
      for I in Workers'Range loop
         if Workers(I)(Current_Time) = null then
            Available_Worker_Id := I;
            return True;
         end if;
      end loop Factory_Loop;
      return False;
   end Get_Available_Worker;
   
   function Get_Available_Step(Input_Data : in Data_Array;
                               Available_Step : out Step_Access) return Boolean is
   begin
      Available_Step := null;
      for I in Input_Data'Range loop
         if Input_Data(I) /= null and then Input_Data(I).Num_Blockers = 0 then
            Available_Step := Input_Data(I);
            return True;
         end if;
      end loop;
      return False;
   end Get_Available_Step;
   
   procedure Plan(Workers : in out Factory;
                  Worker_Id : in Integer;
                  Step_Task : in Step_Access;
                  Input_Data : in out Data_Array;
                  Current_Time : in Time_Type) is
   begin
      -- First, remove the task to schedule from the remaining work.
      Removal_Loop:
      for I in Input_Data'Range loop
         if Input_Data(I) /= null and then Input_Data(I).Id = Step_Task.Id then
            Input_Data(I) := null;
            exit Removal_Loop;
         end if;
      end loop Removal_Loop;
      
      for J in Integer range 0 .. Step_Task.Duration - 1 loop
         Workers(Worker_Id)(Current_Time + J) := Step_Task;
      end loop;
   end Plan;
   
   procedure Free_Up(Finished : in Character; Input_Data : in out Data_Array) is
   begin
      for C in Input_Data'Range loop
         if Input_Data(C) /= null then
            for I in Input_Data(C).Blockers'Range loop
               if Input_Data(C).Blockers(I) /= null and then Input_Data(C).Blockers(I).Id = Finished then
                  Input_Data(C).Blockers(I) := null;
                  Input_Data(C).Num_Blockers := Input_Data(C).Num_Blockers - 1;
               end if;
            end loop;
         end if;
      end loop;
   end Free_Up;
   
   procedure Progress(Elves : in out Factory;
                      Input_Data : in out Data_Array;
                      Current_Time : in Time_Type) is
      Schedule_Candidate : Step_Access;
      Current_Worker_Id : Integer;
      Finished_Step_Id : Character;
   begin
      Finished_Jobs_Loop:
      for I in Elves'Range loop
         if Elves(I)(Current_Time) = null and then Elves(I)(Current_Time -1) /= null then
            Put_Line("Job finished at time " & Integer'Image(Current_Time));
            Finished_Step_Id := Elves(I)(Current_Time - 1).Id;
            Free_Up(Finished   => Finished_Step_Id,
                    Input_Data => Input_Data);
         end if;
      end loop Finished_Jobs_Loop;
      Worker_Loop:
      while (Get_Available_Worker(Workers             => Elves,
                                  Current_Time        => Current_Time,
                                  Available_Worker_Id => Current_Worker_Id)) loop
         if Get_Available_Step(Input_Data, Schedule_Candidate) then
            Plan(Workers => Elves,
                 Worker_Id => Current_Worker_Id,
                 Step_Task => Schedule_Candidate,
                 Input_Data => Input_Data,
                 Current_Time => Current_Time);
            Put_Line("Scheduled " & Schedule_Candidate.Id & " on worker " & Integer'Image(Current_Worker_Id));
         else
            -- No more steps to continue with.
            exit Worker_Loop;
         end if;
      end loop Worker_Loop;
   end Progress;
   
   function Get_All_Tasks_Finished(Workers : in Factory;
                                   Current_Time : in Time_Type) return Boolean is
   begin
      for I in Workers'Range loop
         if Workers(I)(Current_Time) /= null then
            return False;
         end if;
      end loop;
      return Current_Time /= 0;
   end Get_All_Tasks_Finished;
   
   Input_Data   : Data_Array;
   Elves : Factory := (others => (others => null));
   Current_Time : Time_Type := 0;
   Answer : Integer := 0;
begin
   Load_File(Data => Input_Data);
   
   while Steps_Left(Input_Data) loop
      Current_Time := Current_Time + 1;
      Progress(Elves => Elves, Input_Data => Input_Data, Current_Time => Current_Time);
   end loop;

   -- Let all remaining tasks finish
   while not Get_All_Tasks_Finished(Workers      => Elves,
                                    Current_Time => Current_Time) loop
      Current_Time := Current_Time + 1;
      Progress(Elves => Elves, Input_Data => Input_Data, Current_Time => Current_Time);
   end loop;
   
   Answer := Current_Time - 1;
   Put_Line("Final result: " & Integer'Image(Answer));
end Day7_2;
