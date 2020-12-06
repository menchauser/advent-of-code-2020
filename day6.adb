with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Hash;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Hashed_Sets;

procedure Day6 is
    -- Flags for current entry fields
    function Hash_Func(C : Character) return Ada.Containers.Hash_Type is
    begin 
        return Character'Pos (C);
    end Hash_Func;

    function Eq_Key (Left, Right : Character) return Boolean is
    begin
        return Left = Right;
    end Eq_Key;

    package Group_Answers is new Ada.Containers.Hashed_Sets
        (Element_Type        => Character,
         Hash                => Hash_Func,
         Equivalent_Elements => Eq_Key);

    -- Main variables
    InputFile   : File_Type;
    Line        : Unbounded_String := To_Unbounded_String("");
    Answers     : Group_Answers.Set := Group_Answers.Empty_Set;
    Count       : Natural := 0;
    EntryCount  : Natural := 0;
    Result      : Natural := 0;
begin
    -- Read File
    Open (InputFile, In_File, "input6.txt");
    loop
        Count := Count + 1;
        if End_Of_File (InputFile) then
            Result := Result + Natural(Answers.Length);
            Answers := Group_Answers.Empty_Set;
            EntryCount := EntryCount + 1;
            exit;
        end if;
        Line := Get_Line (InputFile);
        if Line = "" then
            Result := Result + Natural(Answers.Length);
            Answers := Group_Answers.Empty_Set;
            EntryCount := EntryCount + 1;
        end if;

        for I in 1 .. Length (Line) loop
            Answers.Include (Element (Line, I));
        end loop;
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
end Day6;
