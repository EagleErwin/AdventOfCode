with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Hash;           use Ada.Strings;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Day16_2 is
   -- ### CONSTANTS ### --
   Ascii_Offset : constant Natural := 48;

   -- ### TYPE DEFINITIONS ### --
   type Binary_Data is mod 16;
   type Bit is mod 2;
   type Bit_Index_Type is mod 4;
   type Pointer is record
      Character_Index : Natural;
      Bit_Index       : Bit_Index_Type; -- Bit offset for the Character_Index
   end record;

   type Packet_Type is (Sum, Product, Minimum, Maximum, Literal, Greater_Than, Less_Than, Equal_To);
   type Packet;
   type Packet_Access is access Packet;
   package Packets is new Vectors(Index_Type   => Natural,
                                  Element_Type => Packet_Access);
   type Packet is record
      Version        : Natural;
      Type_Of_Packet : Packet_Type;
      Value          : Long_Integer;
      Sub_Packets    : Packets.Vector;
   end record;

   type Literal_Part is array (1 .. 5) of Bit;

   function Process_String (Input       : in Unbounded_String;
                            Start_Pos   : in out Pointer) return Packet_Access;

   -- Calculates the pointer position after the given number of increments.
   -- Does not update the actual pointer.
   function Calculate_Pointer (Position  : in Pointer;
                               Increment : in Natural) return Pointer is
      Result : Pointer := Position;
   begin
      Increment_Loop:
      for I in 1 .. Increment loop
         Result.Bit_Index := Result.Bit_Index + 1;
         if Result.Bit_Index = 0 then
            Result.Character_Index := Result.Character_Index + 1;
         end if;
      end loop Increment_Loop;
      return Result;
      end Calculate_Pointer;

   procedure Increment_Pointer (Position  : in out Pointer) is
   begin
      Position.Bit_Index := Position.Bit_Index + 1;
      if Position.Bit_Index = 0 then
         Position.Character_Index := Position.Character_Index + 1;
      end if;
   end Increment_Pointer;

   -- Reads the current bit and increments the pointer to the next bit.
   function Read_Bit(Input    : in Unbounded_String;
                     Position : in out Pointer) return Bit is
      Current_Data : Binary_Data;
      Result : Bit;
   begin
      Current_Data := Binary_Data'Value("16#" & Element(Input, Position.Character_Index) & "#");
      case Position.Bit_Index is
         when 0 => Result := Bit((Current_Data and 2#1000#) / 2#1000#);
         when 1 => Result := Bit((Current_Data and 2#0100#) / 2#0100#);
         when 2 => Result := Bit((Current_Data and 2#0010#) / 2#0010#);
         when 3 => Result := Bit((Current_Data and 2#0001#) / 2#0001#);
      end case;
      Increment_Pointer(Position);
      return Result;
   end Read_Bit;

   -- Get the version number from the current packet.
   -- Assumes that the provided pointer is set to the first bit of the packet.
   function Get_Version (Input : in Unbounded_String;
                         Start_Pos : in out Pointer) return Natural is
      Bit_One   : Bit;
      Bit_Two   : Bit;
      Bit_Three : Bit;
   begin
      Bit_One := Read_Bit(Input, Start_Pos);
      Bit_Two := Read_Bit(Input, Start_Pos);
      Bit_Three := Read_Bit(Input, Start_Pos);
      return Natural(Bit_One) * 2#100# + Natural(Bit_Two) * 2#010# + Natural(Bit_Three) * 2#001#;
   end Get_Version;

   -- Get the type of the current packet.
   -- Assumes that the provided pointer is set to the first bit of the type id.
   function Get_Type (Input : in Unbounded_String;
                      Start_Pos : in out Pointer) return Packet_Type is
      Type_value : Natural;

      Bit_One   : Bit;
      Bit_Two   : Bit;
      Bit_Three : Bit;
   begin
      Bit_One := Read_Bit(Input, Start_Pos);
      Bit_Two := Read_Bit(Input, Start_Pos);
      Bit_Three := Read_Bit(Input, Start_Pos);
      Type_Value := Natural(Bit_One) * 2#100# + Natural(Bit_Two) * 2#010# + Natural(Bit_Three) * 2#001#;

      return Packet_Type'Val(Type_Value);
   end Get_Type;

   -- Returns the value form a literal packet. Assumes the pointer to be at the
   -- start of the first value;
   function Get_Value (Input     : in Unbounded_String;
                       Start_Pos : in out Pointer) return Long_Integer is
      Current_Part : Literal_Part := (others => 1);
      Buffer       : Unbounded_String := To_Unbounded_String("2#");
   begin
      while Current_Part(1) = 1 loop
         Current_Part(1) := Read_Bit(Input, Start_Pos);
         Current_Part(2) := Read_Bit(Input, Start_Pos);
         Current_Part(3) := Read_Bit(Input, Start_Pos);
         Current_Part(4) := Read_Bit(Input, Start_Pos);
         Current_Part(5) := Read_Bit(Input, Start_Pos);
         Buffer := Buffer & Character'Val(Natural(Current_Part(2)) + Ascii_Offset);
         Buffer := Buffer & Character'Val(Natural(Current_Part(3)) + Ascii_Offset);
         Buffer := Buffer & Character'Val(Natural(Current_Part(4)) + Ascii_Offset);
         Buffer := Buffer & Character'Val(Natural(Current_Part(5)) + Ascii_Offset);
      end loop;
      Buffer := Buffer & ("#");

      return Long_Integer'Value(To_String(Buffer));
   end Get_Value;

   -- Returns the length of the sub-packets in an operator packet
   function Get_Length_Of_Sub_Packets (Input : in Unbounded_String;
                                       Position : in out Pointer) return Natural is
      Buffer      : Unbounded_String := To_Unbounded_String("2#");
      Current_Bit : Bit;
   begin
      Parse_Loop:
      for I in 1 .. 15 loop
         Current_Bit := Read_Bit(Input, Position);
         Buffer := Buffer & Character'Val(Natural(Current_Bit) + Ascii_Offset);
      end loop Parse_Loop;
      Buffer := Buffer & ("#");

      return Natural'Value(To_String(Buffer));
   end Get_Length_Of_Sub_Packets;

   function Get_Number_Of_Sub_Packets (Input : in Unbounded_String;
                                       Position : in out Pointer) return Natural is
      Buffer      : Unbounded_String := To_Unbounded_String("2#");
      Current_Bit : Bit;
   begin
      Parse_Loop:
      for I in 1 .. 11 loop
         Current_Bit := Read_Bit(Input, Position);
         Buffer := Buffer & Character'Val(Natural(Current_Bit) + Ascii_Offset);
      end loop Parse_Loop;
      Buffer := Buffer & ("#");

      return Natural'Value(To_String(Buffer));

   end Get_Number_Of_Sub_Packets;

   function Get_Sub_Packets_By_Length (Input : in Unbounded_String;
                                       Position : in out Pointer;
                                       Length : in Natural) return Packets.Vector is
      Result       : Packets.Vector := Packets.Empty_Vector;
      End_Position : Pointer := Calculate_Pointer(Position, Length);
   begin
      -- This works, because a full packet does not fit within 4 bits
      while Position.Character_Index < End_Position.Character_Index loop
         Result.Append(Process_String(Input, Position));
      end loop;
      return Result;
   end Get_Sub_Packets_By_Length;

   -- Returns all sub packets from an operator packet. Assumes the pointer to be
   -- at the length type id bit.
   function Get_Sub_Packets (Input    : in Unbounded_String;
                             Position : in out Pointer) return Packets.Vector is
      Length_Type_Id        : Bit;
      Length_Of_Sub_Packets : Natural;
      Number_Of_Sub_Packets : Natural;

      Result : Packets.Vector := Packets.Empty_Vector;
   begin
      Length_Type_Id := Read_Bit(Input, Position);

      case Length_Type_Id is
         when 0 =>
            Length_Of_Sub_Packets := Get_Length_Of_Sub_Packets(Input, Position);
            Result := Get_Sub_Packets_By_Length(Input, Position, Length_Of_Sub_Packets);
         when 1 =>
            Number_Of_Sub_Packets := Get_Number_Of_Sub_Packets(Input, Position);
            Sub_Packet_Number_Loop:
            for I in 1 .. Number_Of_Sub_Packets loop
               Result.Append(Process_String(Input, Position));
            end loop Sub_Packet_Number_Loop;
      end case;
      return Result;
   end Get_Sub_Packets;

   function Sum (Sub_Packets : in Packets.Vector) return Long_Integer is
      Result : Long_Integer := 0;
   begin
      for P of Sub_Packets loop
         Result := Result + P.Value;
      end loop;
      return Result;
   end Sum;

   function Product (Sub_Packets : in Packets.Vector) return Long_Integer is
      Result : Long_Integer := 1;
   begin
      for P of Sub_Packets loop
         Result := Result * P.Value;
      end loop;
      return Result;
   end Product;

   function Minimum (Sub_Packets : in Packets.Vector) return Long_Integer is
      Result : Long_Integer := Long_Integer'Last;
   begin
      for P of Sub_Packets loop
         if P.Value < Result then
            Result := P.Value;
         end if;
      end loop;
      return Result;
   end Minimum;

   function Maximum (Sub_Packets : in Packets.Vector) return Long_Integer is
      Result : Long_Integer := 0;
   begin
      for P of Sub_Packets loop
         if P.Value > Result then
            Result := P.Value;
         end if;
      end loop;
      return Result;
   end Maximum;

   function Greater_Than (Sub_Packets : in Packets.Vector) return Long_Integer is
   begin
      if Sub_Packets(0).Value > Sub_Packets(1).Value then
         return 1;
      else
         return 0;
      end if;
   end Greater_Than;

   function Less_Than (Sub_Packets : in Packets.Vector) return Long_Integer is
   begin
      if Sub_Packets(0).Value < Sub_Packets(1).Value then
         return 1;
      else
         return 0;
      end if;
   end Less_Than;

   function Equal_To (Sub_Packets : in Packets.Vector) return Long_Integer is
   begin
      if Sub_Packets(0).Value = Sub_Packets(1).Value then
         return 1;
      else
         return 0;
      end if;
   end Equal_To;

   -- Returns true if the pointer reached the end
   function Process_String (Input       : in Unbounded_String;
                            Start_Pos   : in out Pointer) return Packet_Access is
      Current_Version     : Natural;
      Current_Type        : Packet_Type;
      Current_Value       : Long_Integer;
      Current_Sub_Packets : Packets.Vector;

      Result : Packet_Access := new Packet'(Version        => 1,
                                            Type_Of_Packet => Literal,
                                            Value          => 0,
                                            Sub_Packets    => Packets.Empty_Vector);
   begin
      Current_Version := Get_Version(Input, Start_Pos);
      Result.Version := Current_Version;

      Current_Type := Get_Type(Input, Start_Pos);
      Result.Type_Of_Packet := Current_Type;

      if Current_Type = Literal then
         Current_Value := Get_Value(Input, Start_Pos);
         Result.Value := Current_Value;
      else
         Current_Sub_Packets := Get_Sub_Packets(Input, Start_Pos);
         Result.Sub_Packets := Current_Sub_Packets;
         case Current_Type is
            when Literal => Put_Line("ERROR! This should not happen here!");
            when Sum => Result.Value := Sum(Current_Sub_Packets);
            when Product => Result.Value := Product(Current_Sub_Packets);
            when Minimum => Result.Value := Minimum(Current_Sub_Packets);
            when Maximum => Result.Value := Maximum(Current_Sub_Packets);
            when Greater_Than => Result.Value := Greater_Than(Current_Sub_Packets);
            when Less_Than => Result.Value := Less_Than(Current_Sub_Packets);
            when Equal_To => Result.Value := Equal_To(Current_Sub_Packets);
         end case;
      end if;

      return Result;
   end Process_String;

   procedure Load_File (Transmission : out Packet_Access) is
      Input         : File_Type;
      Hex_String    : Unbounded_String;
      End_Reached   : Boolean := False;
      Parse_Pointer : Pointer := (1, 0);
   begin
      Open (File => Input, Mode => In_File, Name => "data/day16.input");
      Hex_String := To_Unbounded_String(Get_Line(Input));
      Close (File => Input);

      Transmission := Process_String(Hex_String, Parse_Pointer);
   end Load_File;

   Transmission   : Packet_Access;
begin
   Load_File(Transmission);

   Put_line("The value of the evaluated expression is "
            & Long_Integer'Image(Transmission.Value) & ".");
end Day16_2;
