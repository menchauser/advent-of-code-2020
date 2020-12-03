with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Integer_Text_IO; use Ada.Long_Integer_Text_IO;

procedure Day3 is
    MaxWidth     : constant Natural := 31;

    procedure Update_Pos (Pos : in out Natural; Right : Natural; Width : Natural := MaxWidth) is
    begin
        Pos := Pos + Right;
        if Pos > Width then
            Pos := Pos mod (Width + 1) + 1;
        end if;
    end Update_Pos;
    
    -- We need big natural type because Ada does not provide one
    type Big_Nat is range 0 .. 2**32;

    InputFile : File_Type;
    -- Length of map row is hardcoded for now
    MapRow    : String (1 .. MaxWidth);
    Pos11     : Natural := 1;
    Pos31     : Natural := 1;
    Pos51     : Natural := 1;
    Pos71     : Natural := 1;
    Pos12     : Natural := 1;
    TC11      : Long_Integer := 0;
    TC31      : Long_Integer := 0;
    TC51      : Long_Integer := 0;
    TC71      : Long_Integer := 0;
    TC12      : Long_Integer := 0;
    RowNumber : Natural := 1;
begin
    -- We read file twice: first to determine size
    Open (InputFile, In_File, "input3.txt");
    while not End_OF_File (InputFile) loop
        MapRow := Get_Line (InputFile);
        Put (Pos31);
        Put (" ");
        Put_Line (MapRow);
        if MapRow(Pos11) = '#' then
            TC11 := TC11 + 1;
        end if;
        if MapRow(Pos31) = '#' then
            TC31 := TC31 + 1;
        end if;
        if MapRow(Pos51) = '#' then
            TC51 := TC51 + 1;
        end if;
        if MapRow(Pos71) = '#' then
            TC71 := TC71 + 1;
        end if;
        if RowNumber mod 2 = 1 then
            if MapRow(Pos12) = '#' then
                TC12 := TC12 + 1;
            end if;
        end if;
        Update_Pos (Pos11, 1);
        Update_Pos (Pos31, 3);
        Update_Pos (Pos51, 5);
        Update_Pos (Pos71, 7);
        if RowNumber mod 2 = 1 then
            Update_Pos (Pos12, 1);
        end if;
        RowNumber := RowNumber + 1;
    end loop;
    Put (TC11);
    Put (TC31);
    Put (TC51);
    Put (TC71);
    Put (TC12);
    New_Line;
    Put (TC11 * TC31 * TC51 * TC71 * TC12);
    New_Line;
end Day3;
