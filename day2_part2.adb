with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; 
with Ada.Strings.Maps;

procedure Day2_part2 is
    InputFile : File_Type;
    Result    : Natural := 0;

    function Is_Valid (PasswordLine: String) return Boolean is 
        package F renames Ada.Strings.Fixed;
        package M renames Ada.Strings.Maps;
        -- PasswordLine format: %d-%d %c: %s
        PolicyDelimIdx : Natural := F.Index (PasswordLine, "-");
        PolicyEndIdx   : Natural := F.Index (PasswordLine, " ");
        FirstPos       : Natural := Natural'Value (PasswordLine (PasswordLine'First..(PolicyDelimIdx-1)));
        SecondPos      : Natural := Natural'Value (PasswordLine ((PolicyDelimIdx+1)..PolicyEndIdx));
        Char           : Character := PasswordLine(PolicyEndIdx+1);
        PasswordIdx    : Natural := F.Index (PasswordLine, " ", Backward) + 1;
        Password       : String := PasswordLine (PasswordIdx..PasswordLine'Last);
    begin
        return (Password(PasswordIdx+FirstPos-1) = Char) xor (Password(PasswordIdx+SecondPos-1) = Char);
    end Is_Valid;
begin
    Open (InputFile, In_File, "input2.txt");
    while not End_Of_File (InputFile) loop	
        if Is_Valid (Get_Line (InputFile)) then
            Result := Result + 1;
        end if;
    end loop;
    Put ("Total number of valid passwords: ");
    Put (Result);
    New_Line;
end Day2_part2;

