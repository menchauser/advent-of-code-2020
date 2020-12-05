with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed, Ada.Strings.Maps; use Ada.Strings;
with Ada.Containers.Generic_Constrained_Array_Sort;

procedure Day5_Part2 is
    subtype Code_Type is String (1 .. 10);

    function To_Number (Code : String; M : Maps.Character_Mapping) return Natural is
        DigitsCode: String := Fixed.Translate (Code, M);
    begin
        return Integer'Value("2#" & DigitsCode & "#");
    end;


    RowM      : constant Maps.Character_Mapping := Maps.To_Mapping ("FB", "01");
    ColM      : constant Maps.Character_Mapping := Maps.To_Mapping ("LR", "01");

    InputFile : File_Type;
    SeatCode  : Code_Type;
    MinSeatId : Natural := Natural'Last;
    MaxSeatId : Natural := 0;
    Count     : Natural := 0;
begin
    Open (InputFile, In_File, "input5.txt");
    -- Count number of records to prepare array 
    while not End_Of_File (InputFile) loop
        SeatCode := Get_Line (InputFile);
        Count := Count + 1;
    end loop;
    Put ("File contains lines: ");
    Put (Count);
    New_Line;
    -- Read all seat codes into array
    Reset (InputFile);
    declare 
        Row      : Natural;
        Col      : Natural;
        SeatId   : Natural := 0;
        Sum      : Natural := 0;
    begin 
        Count := 0;
        while not End_Of_File (InputFile) loop
            SeatCode := Get_Line (InputFile);
            Row := To_Number (SeatCode (1 .. 7), RowM);
            Col := To_Number (SeatCode (8 .. 10), ColM);
            SeatId := Row * 8 + Col;
            MinSeatId := Integer'Min (SeatId, MinSeatId);
            MaxSeatId := Integer'Max (SeatId, MaxSeatId);
            Count := Count + 1;
            Sum := Sum + SeatId;
        end loop;
        Put ("Max seat id: ");
        Put (MaxSeatId, 4);
        New_Line;
        -- Missing Seat Id:
        SeatId := (MinSeatId + MaxSeatId) * (Count + 1) / 2 - Sum;
        Put ("Missing seat: ");
        Put (SeatId, 4);
    end;

    Close (InputFile);

end Day5_Part2;
