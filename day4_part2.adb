with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Hash;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Hashed_Maps; use Ada.Containers;

procedure Day4_Part2 is
    Byr : constant Unbounded_String := To_Unbounded_String ("byr");
    Iyr : constant Unbounded_String := To_Unbounded_String ("iyr");
    Eyr : constant Unbounded_String := To_Unbounded_String ("eyr");
    Hgt : constant Unbounded_String := To_Unbounded_String ("hgt");
    Hcl : constant Unbounded_String := To_Unbounded_String ("hcl");
    Ecl : constant Unbounded_String := To_Unbounded_String ("ecl");
    Pid : constant Unbounded_String := To_Unbounded_String ("pid");
    -- Flags for current entry fields
    function Hash_Func(Key : Unbounded_String) return Ada.Containers.Hash_Type is
    begin 
        return Ada.Strings.Hash(To_String(Key));
    end Hash_Func;

    function Eq_Key (Left, Right : Unbounded_String) return Boolean is
    begin
        return Left = Right;
    end Eq_Key;
    
    package Entry_Fields is new Ada.Containers.Hashed_Maps
        (Key_Type => Unbounded_String,
         Element_Type => Unbounded_String,
         Hash => Hash_Func,
         Equivalent_Keys => Eq_Key);
    
    -- Split string utility
    package String_Vectors is new Indefinite_Vectors (Natural, Unbounded_String);
    use String_Vectors;
    procedure Split (Input : Unbounded_String; Output : out String_Vectors.Vector) is
        Start : Positive := 1;
        Finish : Natural := 0;
        TmpStr : Unbounded_String;
    begin
        while Start <= Length(Input) loop
            Find_Token (Input, To_Set (' '), Start, Outside, Start, Finish);
            exit when Start > Finish;
            TmpStr := Unbounded_Slice (Input, Start, Finish);
            Output.Append (TmpStr);
            Start := Finish + 1;
        end loop;
    end Split;

    function Is_Int_In_Range 
        (Fields : Entry_Fields.Map;
         Field_Name : Unbounded_String;
         Lo, Hi : Long_Integer) return Boolean 
    is
         Val : Long_Integer;
    begin
        if Fields.Contains (Field_Name) then
            Val := Long_Integer'Value (To_String (Fields.Element (Field_Name)));
            return Val in Lo .. Hi;
        else 
            return False;
        end if;
    end Is_Int_In_Range;

    function Is_Valid_Hgt (Fields : Entry_Fields.Map) return Boolean is
    begin
        if Fields.Contains (Hgt) then
            declare 
                Val     : String := To_String (Fields.Element (Hgt));
                Height  : String := Val (Val'First .. Val'Last - 2);
                Measure : String := Val (Val'Last - 1 .. Val'Last);
            begin
                if Measure = "cm" then
                    return Long_Integer'Value(Height) in 150 .. 193;
                elsif Measure = "in" then
                    return Long_Integer'Value(Height) in 59 .. 76;
                else
                    return False;
                end if;
            end;
        else 
            return False;
        end if;
    end;

    function Is_Valid_Hcl (Fields : Entry_Fields.Map) return Boolean is
    begin
        if Fields.Contains (Hcl) then
            declare 
                Val : String := To_String (Fields.Element (Hcl));
            begin
                return Val (Val'First) = '#' 
                    and Fixed.Count (Val, To_Set ("abcdef0123456789")) = Val'Last - Val'First;
            end;
        else 
            return False;
        end if;
    end;

    function Is_Valid_Ecl (Fields : Entry_Fields.Map) return Boolean is
    begin
        if Fields.Contains (Ecl) then
            declare 
                Val : String := To_String (Fields.Element (Ecl));
            begin
                return Val = "amb" or Val = "blu" or Val = "brn" or Val = "gry" or Val = "grn" or Val = "hzl" or Val =
                    "oth";
            end;
        else 
            return False;
        end if;
    end;

    function Is_Valid_Pid (Fields : Entry_Fields.Map) return Boolean is
    begin
        if Fields.Contains (Pid) then
            declare 
                Val : String := To_String (Fields.Element (Pid));
            begin
                return (Val'Last - Val'First + 1 = 9) 
                    and Fixed.Count (Val, To_Set ("0123456789")) = 9;
            end;
        else 
            return False;
        end if;
    end;

    function IsValid (Fields : Entry_Fields.Map) return Boolean is
    begin
        return Is_Int_In_Range (Fields, Byr, 1920, 2002)
            and Is_Int_In_Range (Fields, Iyr, 2010, 2020)
            and Is_Int_In_Range (Fields, Eyr, 2020, 2030)
            and Is_Valid_Hgt (Fields)
            and Is_Valid_Hcl (Fields)
            and Is_Valid_Ecl (Fields)
            and Is_Valid_Pid (Fields);
    end;

    -- Main variables
    InputFile   : File_Type;
    Line        : Unbounded_String;
    EntryFields : Entry_Fields.Map;
    Count       : Natural := 0;
    EntryCount  : Natural := 0;
    Result      : Natural := 0;
begin
    -- We read file twice: first to determine size
    Open (InputFile, In_File, "input4.txt");
    while not End_OF_File (InputFile) loop
        Line := Get_Line (InputFile);
        Count := Count + 1;
        if Line = "" then
            EntryCount := EntryCount + 1;
            if IsValid (EntryFields) then
                Result := Result + 1;
            end if;
            EntryFields := Entry_Fields.Empty_Map;
        else 
            -- extract fields to hash
            declare
                Tokens     : String_Vectors.Vector := Empty_Vector;
                DelimIdx   : Natural;
                FieldName  : Unbounded_String;
                FieldValue : Unbounded_String;
            begin
                Split (Line, Tokens);
                for I in Tokens.First_Index .. Tokens.Last_Index loop
                    DelimIdx := Index (Tokens(I), ":", 1);
                    FieldName := Unbounded_Slice (Tokens(I), 1, DelimIdx - 1);
                    FieldValue := Unbounded_Slice (Tokens(I), DelimIdx + 1, Length (Tokens(I)));
                    Entry_Fields.Insert (EntryFields, FieldName, FieldValue);
                end loop;
            end;
        end if;
    end loop;
    Close (InputFile);

    Put ("Lines: ");
    Put (Count);
    New_Line;
    Put ("Entries: ");
    Put (EntryCount);
    New_Line;
    Put ("Result: ");
    Put (Result);
    New_Line;
end Day4_Part2;
