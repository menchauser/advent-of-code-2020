with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Integer_Text_IO; use Ada.Long_Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps; 
with Ada.Command_Line; 


procedure Day14 is
    type Val_T is mod 2**64;

    type Mask_T is
        record
            And_Part : Val_T;
            Or_Part  : Val_T;
        end record;

    
    procedure Put (Mask : Mask_T) is
        Bit_Num : Natural;
    begin
        for I in 1 .. 36 loop
            Bit_Num := 36 - I;
            if (Mask.And_Part and Val_T (2**Bit_Num)) = 0 then
                Put ('0');
            elsif (Mask.Or_Part and Val_T (2**Bit_Num)) > 0 then
                Put ('1');
            else 
                Put ('X');
            end if;
        end loop;
    end;


    function Parse_Mask (Line : String) return Mask_T is
        And_Part : Val_T := Val_T'Last;
        Or_Part  : Val_T := 0;
        Digit    : Natural;
    begin 
        for I in reverse Line'Range loop
            Digit := Line'Last - I; 
            if Line (I) = '0' then
                And_Part := And_Part - 2**Digit;
            elsif Line (I) = '1' then
                Or_Part  := Or_Part + 2**Digit;
            end if;
        end loop;
        return (And_Part, Or_Part);
    end;


    function Hash (Key : Natural) return Hash_Type is
        M31 : constant := 2**31 - 1;
    begin
        return Hash_Type (Key) * M31;
    end;


    package Mem_T is new Hashed_Maps
        (Key_Type        => Natural,
         Element_Type    => Val_T,
         Hash            => Hash,
         Equivalent_Keys => "=");


    function Apply (X : Val_T; Mask : Mask_T) return Val_T is
    begin
        return (X and Mask.And_Part) or Mask.Or_Part;
    end;

    -- Main variables
    InputFile : File_Type;
    Line      : Unbounded_String;
    Mask      : Mask_T;
    Address   : Natural;
    Value     : Val_T;
    Memory    : Mem_T.Map := Mem_T.Empty_Map;
    Result    : Val_T := 0;
begin
    Open (InputFile, In_File, Ada.Command_Line.Argument (1));
    while not End_Of_File (InputFile) loop
        Line := Get_Line (InputFile);
        if Head (Line, 4) = "mask" then
            Mask := Parse_Mask (Slice (Line, 8, Length (Line)));
            Put_Line (Line);
            Put ("mask = "); Put (Mask); New_Line;
        elsif Head (Line, 3) = "mem" then
            Address := Natural'Value (
                Slice (Line, 5, Index (Line, "]") - 1)
            );
            Value := Val_T'Value (Slice (Line, Index (Line, "]") + 4, Length (Line)));
            Put ("mem["); Put (Address, 0); Put("] = "); New_Line;
            Put ("  "); Put (Long_Integer (Value), Base => 2, Width => 37); New_Line;
            Put ("  "); Put (Mask); New_Line;
            Put ("  "); Put (Long_Integer (Apply (Value, Mask)), Base => 2, Width => 37); New_Line;

            Put (Val_T'Image (Value)); 
            Put (" => ");
            Value := Apply (Value, Mask);
            Put (Val_T'Image (Value));
            New_Line;
            Memory.Include (Address, Value);
        else 
            Put_Line ("Unexpected input: " & Line);
        end if;
    end loop;
    Close (InputFile);

    for C in Memory.Iterate loop
        -- Put (Mem_T.Key (C)); Put (": "); Put (Val_T'Image (Mem_T.Element (C))); New_Line;
        Result := Result + Mem_T.Element (C);
    end loop;
    Put ("Part 1 result: "); Put (Val_T'Image (Result)); New_Line;

end Day14;
