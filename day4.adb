with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Hash;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Hashed_Maps; use Ada.Containers;

procedure Day4 is
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
         Element_Type => Boolean,
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
    
    function IsValid (Fields : Entry_Fields.Map) return Boolean is
    begin
        return Entry_Fields.Contains (Fields, To_Unbounded_String("byr"))
            and Entry_Fields.Contains (Fields, To_Unbounded_String("iyr"))
            and Entry_Fields.Contains (Fields, To_Unbounded_String("eyr"))
            and Entry_Fields.Contains (Fields, To_Unbounded_String("hgt")) 
            and Entry_Fields.Contains (Fields, To_Unbounded_String("hcl"))
            and Entry_Fields.Contains (Fields, To_Unbounded_String("ecl"))
            and Entry_Fields.Contains (Fields, To_Unbounded_String("pid"));
    end;

    -- Main variables
    InputFile   : File_Type;
    Line        : Unbounded_String;
    Tokens      : String_Vectors.Vector := Empty_Vector;
    EntryFields : Entry_Fields.Map;
    FieldName   : Unbounded_String;
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
            Split (Line, Tokens);
            for I in Tokens.First_Index .. Tokens.Last_Index loop
                FieldName := Unbounded_Slice (Tokens(I), 1, Index (Tokens(I), ":", 1) - 1);
                Entry_Fields.Insert (EntryFields, FieldName, True);
            end loop;
            Tokens := Empty_Vector;
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
end Day4;
