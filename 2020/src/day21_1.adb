with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Containers.Vectors;     use Ada.Containers;
with Ada.Containers.Hashed_Maps; use Ada.Containers;

procedure Day21_1 is
   -- ### CONSTANTS ### --
   Enable_Debug     : constant Boolean := False;
   Number_Of_Inputs : constant Natural := 33;

   -- ### TYPE DEFINITIONS ### --
   subtype Data_Line is Unbounded_String;

   -- A vector of different ingredients
   package Ingredient_Vector is new Vectors (Natural, Unbounded_String);
   use     Ingredient_Vector;

   function Unbounded_String_Hash (Value : Unbounded_String) return Hash_Type is
   begin
      return Hash(To_String(Value));
   end Unbounded_String_Hash;

   -- Map from Allergen to all possible Ingredients
   package Allergen_Map is new Hashed_Maps(Key_Type        => Unbounded_String,
                                           Element_Type    => Ingredient_Vector.Vector,
                                           Hash            => Unbounded_String_Hash,
                                           Equivalent_Keys => "=");
   use Allergen_Map;

   -- Used to store all ingredients from the input
   type Ingredient_Array is array (1 .. Number_Of_Inputs) of Ingredient_Vector.Vector;

   procedure Log_Debug(Text: in String) is
   begin
      if Enable_Debug then
         Put_Line(Text);
      end if;
   end Log_Debug;

   procedure Parse_Line(Input              : in Unbounded_String;
                        Possible_Allergens : in out Allergen_Map.Map;
                        All_Ingredients    : in out Ingredient_Vector.Vector;
                        Input_Ingredients  : in out Ingredient_Array;
                        Ingredient_Index   : in Natural) is
      Contains_Index : Natural := Index(Input, " (contains ");
      Buffer : Unbounded_String := To_Unbounded_String("");
      Curr_Char : Character;
      Ingredient : Unbounded_String;
      Allergen : Unbounded_String;

      Tmp_Ingredients : Ingredient_Vector.Vector;

      Result : Ingredient_Vector.Vector;
   begin
      Ingredient_Loop:
      for I in 1 .. Contains_Index loop
         Curr_Char := Element(Input, I);
         if Curr_Char = ' ' then
            Ingredient := Buffer;
            if not All_Ingredients.Contains(Ingredient) then
               All_Ingredients.Append(Ingredient);
            end if;
            Result.Append(Ingredient);
            Buffer := To_Unbounded_String("");
         else
            Buffer := Buffer & Curr_Char;
         end if;
      end loop Ingredient_Loop;

      Input_Ingredients(Ingredient_Index) := Result;

      Buffer := To_Unbounded_String("");

      Allergen_Loop:
      for J in Contains_Index + 11 .. Length(Input) loop
         Curr_Char := Element(Input, J);
         if Curr_Char = ',' or else Curr_Char = ')' then
            Allergen := Buffer;
            if Possible_Allergens.Find(Allergen) = Allergen_Map.No_Element then
               -- All ingredients from this line are possible for the allergen
               Possible_Allergens.Insert(Allergen, Result);
            else
               Tmp_Ingredients := Possible_Allergens(Allergen);
               Reduce_Loop:
               for Ingr of Tmp_Ingredients loop
                  if not Result.Contains(Ingr) then
                     -- Only the ingredients that are also on this line are possible for the allergen
                     Possible_Allergens(Allergen).Delete(Possible_Allergens(Allergen).Find_Index(Ingr));
                  end if;
               end loop Reduce_Loop;
            end if;
            Buffer := To_Unbounded_String("");
         elsif Curr_Char /= ' ' then
            Buffer := Buffer & Curr_Char;
         end if;
      end loop Allergen_Loop;
   end Parse_Line;

   procedure Load_File (Possible_Allergens : out Allergen_Map.Map;
                        All_Ingredients    : out Ingredient_Vector.Vector;
                        Input_Ingredients  : out Ingredient_Array) is
      Input        : File_Type;
      Current_Line : Data_Line;
      Idx          : Integer := 1;

      Buffer : Unbounded_String := To_Unbounded_String("");
   begin
      Open (File => Input, Mode => In_File, Name => "data/day21.input");
      Read_Loop:
      while not End_Of_File (Input) loop
         Current_Line := To_Unbounded_String(Get_Line(Input));
         Parse_Line(Current_Line, Possible_Allergens, All_Ingredients, Input_Ingredients, Idx);
         Idx := Idx + 1;
      end loop Read_Loop;

   end Load_File;

   procedure Reduce(Allergens : in out Allergen_Map.Map) is
      Ingredient_Removed : Boolean := True;
      Ingredients_To_Remove : Ingredient_Vector.Vector;
   begin
      Reduce_Loop:
      while Ingredient_Removed loop
         Ingredient_Removed := False;
         Allergen_Loop:
         for A in Allergens.Iterate loop
            if Allergens(A).Length = 1 then
               Ingredients_To_Remove.Append(Allergens(A)(0));
            end if;
         end loop Allergen_Loop;

         Remove_Loop:
         for To_Remove of Ingredients_To_Remove loop
            Reduce_Allergen_Loop:
            for A in Allergens.Iterate loop
               if Allergens(A).Length /= 1 and then Allergens(A).Contains(To_Remove) then
                  Ingredient_Removed := True;
                  Allergens(A).Delete(Allergens(A).Find_Index(To_Remove));
               end if;
            end loop Reduce_Allergen_Loop;
         end loop Remove_Loop;
      end loop Reduce_Loop;

   end Reduce;

   -- Returns a Vector of all Non-allergen ingredients. Assumes that the
   -- Allergens map contains only one ingredient per allergen.
   function Get_Non_Allergens(Unique_Ingredients : in Ingredient_Vector.Vector;
                              Allergens :          in Allergen_Map.Map)
                              return Ingredient_Vector.Vector is
      Result : Ingredient_Vector.Vector := Unique_Ingredients;
      Ingr : Unbounded_String;
   begin
      Allergen_Loop:
      for A in Allergens.Iterate loop
         Result.Delete(Result.Find_Index(Allergens(A)(0)));
      end loop Allergen_Loop;
      return Result;
   end Get_Non_Allergens;

   Allergens          : Allergen_Map.Map;
   Unique_Ingredients : Ingredient_Vector.Vector;
   Ingredients        : Ingredient_Array;

   Non_Allergens      : Ingredient_Vector.Vector;

   Answer       : Integer := 0;
begin
   Load_File(Allergens, Unique_Ingredients, Ingredients);

   Reduce(Allergens);

   if Enable_Debug then
      for I in Allergens.Iterate loop
         Put_line(To_String(Key(I)));
         for Ingr of Allergens(I) loop
            Put(To_String(Ingr) & ",");
         end loop;
         Put_Line("");
         Put_Line("");
      end loop;
   end if;

   Non_Allergens := Get_Non_Allergens(Unique_Ingredients, Allergens);

   Count_Loop:
   for Ingr of Non_Allergens loop
      Log_Debug("Non-allergen ingredient: " & To_String(Ingr));
      Input_Loop:
      for Input_Line of Ingredients loop
         Ingredient_Loop:
         for Input_Ingr of Input_Line loop
            if Input_Ingr = Ingr then
               Answer := Answer + 1;
            end if;
         end loop Ingredient_Loop;
      end loop Input_Loop;
   end loop Count_Loop;

   Put_line("Number of apprearances of ingredients that are not allergen is "
            & Integer'Image(Answer));
end Day21_1;
