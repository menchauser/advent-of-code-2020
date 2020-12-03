with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Day2 is
    InputFile : File_Type;
    TempStr   : String (1..80);
    Last      : Natural;
    Count     : Natural := 0;
    Result    : Natural := 0;

    function Is_Valid (PasswordLine: String) return Boolean is 
        -- PasswordLine format: %d-%d %c: %s
        PolicyDelimIdx : Natural := Index (PasswordLine, "-");
        PolicyEndIdx   : Natural := Index (PasswordLine, " ");
        MinChars       : Natural := Natural'Value (PasswordLine (PasswordLine'First..(PolicyDelimIdx-1)));
        MaxChars       : Natural := Natural'Value (PasswordLine ((PolicyDelimIdx+1)..PolicyEndIdx));
        Char           : Character := PasswordLine(PolicyEndIdx+1);
        PasswordIdx    : Natural := Index (PasswordLine, " ", Backward) + 1;
        Password       : String := PasswordLine (PasswordIdx..PasswordLine'Last);
        CharCount      : Natural := 0;
    begin
        for I in Password'Range loop
            if Password(I) = Char then
                CharCount := CharCount + 1;
            end if;
        end loop;
        if CharCount in MinChars .. MaxChars then
            return True;
        else 
            return False;
        end if;
    end Is_Valid;
begin
    -- We read file twice: first to determine size
    Open (InputFile, In_File, "input2.txt");
    while not End_OF_File (InputFile) loop
        Get_Line (InputFile, TempStr, Last);
        Count := Count + 1;  
    end loop;
    Put ("Number of lines in input: ");
    Put (Count);
    New_Line;
    Reset (InputFile);
    -- Now we read and handle actual data
    while not End_Of_File (InputFile) loop	
        if Is_Valid (Get_Line (InputFile)) then
            Result := Result + 1;
        end if;
    end loop;
    Put ("Total number of valid passwords: ");
    Put (Result);
    New_Line;
end Day2;

