with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Day1 is
    InputFile : File_Type;
    TempStr   : String (1..80);
    Last      : Natural;
    Count     : Natural := 0;
    type IntArray is array (Integer range <>) of Integer;
begin
    -- We read file twice: first to determine size
    Open (InputFile, In_File, "input1.txt");
    while not End_OF_File (InputFile) loop
        Get_Line (InputFile, TempStr, Last);
        Count := Count + 1;
    end loop;
    Put ("File contains lines: ");
    Put (Count);
    New_Line;
    Reset (InputFile);
    -- Now we read and handle actual data
    declare 
        InputData : IntArray (1..Count);
        Idx       : Natural := 1;
    begin
        while not End_OF_File (InputFile) loop
            Get (InputFile, InputData(Idx));
            Idx := Idx + 1;
        end loop;

        for I in InputData'Range loop
            for J in Integer range I..Count loop
                if InputData(I) + InputData(J) = 2020 then
                    Put ("Result: ");
                    Put (InputData(I) * InputData(J));
                    New_Line;
                end if;
            end loop;
        end loop;
    end;
end Day1;
