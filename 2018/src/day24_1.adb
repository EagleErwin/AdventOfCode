with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Containers.Generic_Array_Sort; use Ada.Containers;
procedure Day24_1 is
   Max_Army_Size : constant Integer := 10;
   Max_Number_Of_Elements : constant Integer := 3;

   subtype Data_Line is Unbounded_String;

   type Side is (Immune, Infection);

   type Attack_Element is (Bludgeoning, Cold, Fire, Radiation, Slashing, None);
   type Element_Array is array (1 .. Max_Number_Of_Elements) of Attack_Element;

   type Group;
   type Group_Access is access all Group;
   type Group is record
      Party : Side;
      Number_Of_Units : Integer;
      Hit_Points : Integer;
      Number_Of_Weaknesses : Integer;
      Weaknesses : Element_Array;
      Number_Of_Immunes : Integer;
      Immunes : Element_Array;
      Damage : Integer;
      Damage_Type : Attack_Element;
      Initiative : Integer;
      Effective_Power : Integer;
      Target : Group_Access;
   end record;

   type Group_Array is array (1 .. Max_Army_Size) of Group_Access;
   type Army is record
      Party : Side;
      Groups : Group_Array;
   end record;

   type Planning_Array is array (1 .. 2 * Max_Army_Size) of Group_Access;
   type Planning is record
      Groups : Planning_Array;
   end record;

   procedure Parse_Group_Line (Input_String : in Unbounded_String;
                               Target_Army : in out Army) is
      Buffer       : Unbounded_String;
      Char_Pointer : Integer := 1;
      Current_Char : Character;
      Nr_Units     : Integer;
      Hit_Points   : Integer;

      Nr_Of_Immunities : Integer := 0;
      Immunities_Array : Element_Array := (others => None);
      Nr_Of_Weaknesses : Integer := 0;
      Weaknesses_Array : Element_Array := (others => None);

      Damage         : Integer;
      Damage_Element : Attack_Element;

      Initiative     : Integer;

      Group_Idx      : Integer;
      New_Group      : Group_Access;
   begin
      Unit_Count_Loop:
      for I in Char_Pointer .. Length(Input_String) loop
         Current_Char := Element(Input_String, I);
         exit Unit_Count_Loop when Current_Char = ' ';
         Buffer := Buffer & Current_Char;
      end loop Unit_Count_Loop;
      Nr_Units := Integer'Value(To_String(Buffer));
      Buffer := Null_Unbounded_String;

      Char_Pointer := Index(Input_String, "with") + 5;
      Hit_Points_Loop:
      for I in Char_Pointer .. Length(Input_String) loop
         Current_Char := Element(Input_String, I);
         exit Hit_Points_Loop when Current_Char = ' ';
         Buffer := Buffer & Current_Char;
      end loop Hit_Points_Loop;
      Hit_Points := Integer'Value(To_String(Buffer));
      Buffer := Null_Unbounded_String;

      Char_Pointer := Index(Input_String, "immune to");
      if Char_Pointer /= 0 then -- Not all groups are immune to something
         Char_Pointer := Char_Pointer + 10;
         Immune_Loop:
         for I in Char_Pointer .. Length(Input_String) loop
            Current_Char := Element(Input_String, I);
            if Current_Char = ',' or Current_Char = ';' or Current_Char = ')' then
               Nr_Of_Immunities := Nr_Of_Immunities + 1;
               Immunities_Array(Nr_Of_Immunities) := Attack_Element'Value(To_String(Buffer));
               Buffer := Null_Unbounded_String;
               exit Immune_Loop when Current_Char = ';' or Current_Char = ')';
            else
              Buffer := Buffer & Current_Char;
            end if;
         end loop Immune_Loop;
      end if;

      Char_Pointer := Index(Input_String, "weak to");
      if Char_Pointer /= 0 then -- Not all groups are weak to something
         Char_Pointer := Char_Pointer + 8;
         Weakness_Loop:
         for I in Char_Pointer .. Length(Input_String) loop
            Current_Char := Element(Input_String, I);
            if Current_Char = ',' or Current_Char = ';' or Current_Char = ')' then
               Nr_Of_Weaknesses := Nr_Of_Weaknesses + 1;
               Weaknesses_Array(Nr_Of_Weaknesses) := Attack_Element'Value(To_String(Buffer));
               Buffer := Null_Unbounded_String;
               exit Weakness_Loop when Current_Char = ';' or Current_Char = ')';
            else
              Buffer := Buffer & Current_Char;
            end if;
         end loop Weakness_Loop;
      end if;

      Char_Pointer := Index(Input_String, "that does") + 10;
      Damage_Loop:
      for I in Char_Pointer .. Length(Input_String) loop
         Current_Char := Element(Input_String, I);
         Char_Pointer := I;
         exit Damage_Loop when Current_Char = ' ';
         Buffer := Buffer & Current_Char;
      end loop Damage_Loop;
      Damage := Integer'Value(To_String(Buffer));
      Char_Pointer := Char_Pointer + 1;
      Buffer := Null_Unbounded_String;

      Damage_Element_Loop:
      for I in Char_Pointer .. Length(Input_String) loop
         Current_Char := Element(Input_String, I);
         exit Damage_Element_Loop when Current_Char = ' ';
         Buffer := Buffer & Current_Char;
      end loop Damage_Element_Loop;
      Damage_Element := Attack_Element'Value(To_String(Buffer));
      Buffer := Null_Unbounded_String;

      Char_Pointer := Index(Input_String, "initiative") + 11;
      Initiative_Loop:
      for I in Char_Pointer .. Length(Input_String) loop
         Current_Char := Element(Input_String, I);
         Buffer := Buffer & Current_Char;
      end loop Initiative_Loop;
      Initiative := Integer'Value(To_String(Buffer));

      New_Group := new Group'(Party                => Target_Army.Party,
                              Number_Of_Units      => Nr_Units,
                              Hit_Points           => Hit_Points,
                              Number_Of_Weaknesses => Nr_Of_Weaknesses,
                              Weaknesses           => Weaknesses_Array,
                              Number_Of_Immunes    => Nr_Of_Immunities,
                              Immunes              => Immunities_Array,
                              Damage               => Damage,
                              Damage_Type          => Damage_Element,
                              Initiative           => Initiative,
                              Effective_Power      => Nr_Units * Damage,
                              Target               => null);

      Index_Loop:
      for I in Target_Army.Groups'Range loop
         if Target_Army.Groups(I) = null then
            Group_Idx := I;
            exit Index_Loop;
         end if;
      end loop Index_Loop;

      Target_Army.Groups(Group_Idx) := New_Group;
   end Parse_Group_Line;

   procedure Load_File (Immune_System_Army : out Army;
                        Infection_Army     : out Army) is
      Input               : File_Type;
      Current_Line        : Data_Line;
   begin
      Immune_System_Army := (Party => Immune,
                             Groups => (others => null));
      Infection_Army     := (Party => Infection,
                             Groups => (others => null));

      Open (File => Input, Mode => In_File, Name => "data/day24.input");

      Current_Line := To_Unbounded_String(Get_Line (File => Input)); --Immune system:
      Current_Line := To_Unbounded_String(Get_Line (File => Input));
      Immune_Loop:
      while Length(Source => Current_Line) /= 0 loop
         Parse_Group_Line(Input_String => Current_Line,
                          Target_Army => Immune_System_Army);
         Current_Line := To_Unbounded_String(Get_Line (File => Input));
      end loop Immune_Loop;

      Current_Line := To_Unbounded_String(Get_Line (File => Input)); --Infection:
      Infection_Loop:
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(File => Input));
         Parse_Group_Line(Input_String => Current_Line,
                          Target_Army => Infection_Army);
      end loop Infection_Loop;

      Close (File => Input);
   end Load_File;

   procedure Print_Planning (Army_To_Print : in Planning) is
      Current_Group : Group_Access;
      Group_Target  : Integer := -1;
   begin
      for I in Army_To_Print.Groups'Range loop
         if Army_To_Print.Groups(I) /= null then
            Current_Group := Army_To_Print.Groups(I);
            if Current_Group.Target /= null then
               Group_Target := Current_Group.Target.Number_Of_Units;
            end if;
            Put_Line(Side'Image(Current_Group.Party) & ":"
                  & Integer'Image(Current_Group.Number_Of_Units)
                  & " units each with"
                  & Integer'Image(Current_Group.Hit_Points)
                  & " hit points (WEAK/IMMUNE) with an attack that does"
                  & Integer'Image(Current_Group.Damage)
                  & " " & Attack_Element'Image(Current_Group.Damage_Type)
                  & " damage at initiative"
                  & Integer'Image(Current_Group.Initiative)
                  & " (effective power:"
                  & Integer'Image(Current_Group.Effective_Power)
                  & ") targets the other group that has"
                     & Integer'Image(Group_Target) & " units.");
         end if;
      end loop;
   end Print_Planning;

   procedure Target_Selection_Sort (A : in out Planning_Array) is
      procedure Swap_Elements(A : in out Planning_Array; I, J : in Integer) is
         Temp : Group_Access;
      begin
         Temp := A(J);
         A(J) := A(I);
         A(I) := Temp;
      end Swap_Elements;
   begin
      for I in reverse A'Range loop
         for J in A'First .. I loop
            if A(J) = null then
               Swap_Elements(A => A,
                             I => I,
                             J => J);
            elsif A(I) /= null then
               if A(I).Effective_Power > A(J).Effective_Power then
                  Swap_Elements(A => A,
                                I => I,
                                J => J);
               elsif A(I).Effective_Power = A(J).Effective_Power and then
                 A(I).Initiative > A(J).Initiative then
                  Swap_Elements(A => A,
                                I => I,
                                J => J);
               end if;
            end if;
         end loop;
      end loop;
   end Target_Selection_Sort;

   procedure Attack_Sort (A : in out Planning_Array) is
      procedure Swap_Elements(A : in out Planning_Array; I, J : in Integer) is
         Temp : Group_Access;
      begin
         Temp := A(J);
         A(J) := A(I);
         A(I) := Temp;
      end Swap_Elements;
   begin
      for I in reverse A'Range loop
         for J in A'First .. I loop
            if A(J) = null then
               Swap_Elements(A => A,
                             I => I,
                             J => J);
            elsif A(J).Target = null then
               Swap_Elements(A => A,
                             I => I,
                             J => J);
            elsif A(I) /= null then
               if A(I).Initiative > A(J).Initiative then
                  Swap_Elements(A => A,
                                I => I,
                                J => J);
               end if;
            end if;
         end loop;
      end loop;
   end Attack_Sort;

   function Target_Available (Target : in Group_Access;
                              Attackers : in Army) return Boolean is
   begin
      for A in Attackers.Groups'Range loop
         if Attackers.Groups(A) /= null and then
           Attackers.Groups(A).Target = Target then
            return False;
         end if;
      end loop;
      return True;
   end Target_Available;

   function Calculate_Damage (Attacker, Victim : in Group_Access) return Integer is
      Current_Damage : Integer;
   begin
      Current_Damage := Attacker.Damage;
      Immune_Loop:
      for I_E in 1 .. Victim.Number_Of_Immunes loop
         if Victim.Immunes(I_E) = Attacker.Damage_Type then
            Current_Damage := 0;
            exit Immune_Loop;
         end if;
      end loop Immune_Loop;

      Weakness_Loop:
      for W_E in 1 .. Victim.Number_Of_Weaknesses loop
         if Victim.Weaknesses(W_E) = Attacker.Damage_Type then
            Current_Damage := Current_Damage * 2;
            exit Weakness_Loop;
         end if;
      end loop Weakness_Loop;

      return Current_Damage * Attacker.Number_Of_Units;
   end Calculate_Damage;

   procedure Find_Best_Target(Attacker : in out Group_Access;
                              Enemy_Army, Friendly_Army : in Army) is
      Current_Damage : Integer;
      Most_Damage    : Integer := 0;
      Current_Enemy  : Group_Access;
      Target_Enemy   : Group_Access := null;
   begin
      for I in Enemy_Army.Groups'Range loop
         Current_Enemy := Enemy_Army.Groups(I);
         if Current_Enemy /= null then
            if Target_Available(Target    => Current_Enemy,
                                Attackers => Friendly_Army) then
               Current_Damage := Calculate_Damage(Attacker => Attacker,
                                                  Victim   => Current_Enemy);
               if Current_Damage > Most_Damage then
                  Most_Damage := Current_Damage;
                  Target_Enemy := Current_Enemy;
               elsif Current_Damage = Most_Damage and then Target_Enemy /= null then
                  if Current_Enemy.Effective_Power > Target_Enemy.Effective_Power then
                     Most_Damage := Current_Damage;
                     Target_Enemy := Current_Enemy;
                  elsif Current_Enemy.Effective_Power = Target_Enemy.Effective_Power then
                     if Current_Enemy.Initiative > Target_Enemy.Initiative then
                        Most_Damage := Current_Damage;
                        Target_Enemy := Current_Enemy;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end loop;

      if Most_Damage > 0 then
         Attacker.Target := Target_Enemy;
      end if;
   end Find_Best_Target;

   function Target_Selection(Immune_System_Army, Infection_Army : in Army) return Planning is
      Result         : Planning;
      Current_Group  : Group_Access;
      Target_Army    : Army;
      Attacking_Army : Army;

      Group_Idx      : Integer := 0;
   begin
      for F in Immune_System_Army.Groups'Range loop
         if Immune_System_Army.Groups(F) /= null then
            Group_Idx := Group_Idx + 1;
            Result.Groups(Group_Idx) := Immune_System_Army.Groups(F);
         end if;
      end loop;
      for S in Infection_Army.Groups'Range loop
         if Infection_Army.Groups(S) /= null then
            Group_Idx := Group_Idx + 1;
            Result.Groups(Group_Idx) := Infection_Army.Groups(S);
         end if;
      end loop;
      Target_Selection_Sort(A => Result.Groups);

      for G in Result.Groups'Range loop
         Current_Group := Result.Groups(G);
         if Current_Group /= null then
            if Current_Group.Party = Immune then
               Target_Army := Infection_Army;
               Attacking_Army := Immune_System_Army;
            else
               Target_Army := Immune_System_Army;
               Attacking_Army := Infection_Army;
            end if;

            Find_Best_Target(Attacker      => Current_Group,
                             Enemy_Army    => Target_Army,
                             Friendly_Army => Attacking_Army);
         end if;
      end loop;

      for I in Result.Groups'Range loop
         if Result.Groups(I) /= null and then Result.Groups(I).Target = null then
            Result.Groups(I) := null;
         end if;
      end loop;
      return Result;
   end Target_Selection;

   procedure Attack (Battle_Planning : in out Planning) is
      Attacker : Group_Access;
      Total_Damage : Integer;
      Casualties : Integer;
   begin
      Attack_Sort(A => Battle_Planning.Groups);
      for I in Battle_Planning.Groups'Range loop
         Attacker := Battle_Planning.Groups(I);
         if Attacker /= null then
            Total_Damage := Calculate_Damage(Attacker => Attacker,
                                             Victim   => Attacker.Target);
            Casualties := Total_Damage / Attacker.Target.Hit_Points;

            if Casualties > Attacker.Target.Number_Of_Units then
               Casualties := Attacker.Target.Number_Of_Units;
            end if;

            Attacker.Target.Number_Of_Units := Attacker.Target.Number_Of_Units - Casualties;
            Attacker.Target.Effective_Power := Attacker.Target.Number_Of_Units * Attacker.Target.Damage;
         end if;
      end loop;
   end Attack;

   procedure Garbage_Collect (Garbaged_Army : in out Army) is
   begin
      for I in Garbaged_Army.Groups'Range loop
         if Garbaged_Army.Groups(I) /= null then
            Garbaged_Army.Groups(I).Target := null;
            if Garbaged_Army.Groups(I).Number_Of_Units = 0 then
               Garbaged_Army.Groups(I) := null;
            end if;
         end if;
      end loop;
   end Garbage_Collect;

   function Calculate_Survivors (An_Army : in Army) return Integer is
      Survivors : Integer := 0;
   begin
      for G in An_Army.Groups'Range loop
         if An_Army.Groups(G) /= null then
            Survivors := Survivors + An_Army.Groups(G).Number_Of_Units;
         end if;
      end loop;
      return Survivors;
   end Calculate_Survivors;

   function Fight (Immune_Army : in out Army;
                   Infection_Army : in out Army) return Boolean is
      Battle_Planning : Planning;

      Immune_Survivors    : Integer;
      Infection_Survivors : Integer;
   begin
      Battle_Planning := Target_Selection(Immune_System_Army => Immune_Army,
                                          Infection_Army => Infection_Army);
      --Print_Planning(Battle_Planning);
      Attack(Battle_Planning => Battle_Planning);

      Garbage_Collect(Garbaged_Army => Immune_Army);
      Garbage_Collect(Garbaged_Army => Infection_Army);

      Immune_Survivors := Calculate_Survivors(An_Army => Immune_Army);
      Infection_Survivors := Calculate_Survivors(An_Army => Infection_Army);

      return Immune_Survivors = 0 or Infection_Survivors = 0;
   end Fight;

   function Determine_Remaining_Units(Immune_Army : in Army;
                                      Infection_Army : in Army) return Integer is
      Winner : Army;
      Result : Integer := 0;
   begin
      for I in Immune_Army.Groups'Range loop
         if Immune_Army.Groups(I) /= null then
            Result := Result + Immune_Army.Groups(I).Number_Of_Units;
         end if;
      end loop;

      if Result > 0 then
         Winner := Immune_Army;
         Put_Line("Immune won");
      else
         Result := 0;
         for J in Infection_Army.Groups'Range loop
            if Infection_Army.Groups(J) /= null then
               Result := Result + Infection_Army.Groups(J).Number_Of_Units;
            end if;
         end loop;

         if Result > 0 then
            Winner := Infection_Army;
            Put_Line("Infection won");
         else
            Put_Line("[ERROR] Both armies are dead!");
         end if;
      end if;

      return Result;
   end Determine_Remaining_Units;

   Answer : Integer;
   Finished : Boolean := False;

   Immune_System_Army : Army;
   Infection_Army     : Army;
begin
   Load_File (Immune_System_Army => Immune_System_Army,
              Infection_Army => Infection_Army);

   while not Finished loop
      Finished := Fight(Immune_Army => Immune_System_Army,
                        Infection_Army => Infection_Army);
   end loop;

   Answer := Determine_Remaining_Units(Immune_Army => Immune_System_Army,
                                       Infection_Army => Infection_Army);

   Put_Line("Answer:" & Integer'Image(Answer));
end Day24_1;
