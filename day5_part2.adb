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
        subtype Code_Array_Idx is Natural range 1 .. Count;
        type Code_Array_Type is array (Code_Array_Idx) of Natural;
        SeatIds  : Code_Array_Type;
        Row      : Natural;
        Col      : Natural;
        SeatId   : Natural := 0;

        procedure Sort is new Ada.Containers.Generic_Constrained_Array_Sort 
            (Index_Type => Code_Array_Idx,
             Element_Type => Natural, 
             Array_Type => Code_Array_Type);
    begin 
        Count := 0;
        while not End_Of_File (InputFile) loop
            SeatCode := Get_Line (InputFile);
            Row := To_Number (SeatCode (1 .. 7), RowM);
            Col := To_Number (SeatCode (8 .. 10), ColM);
            SeatId := Row * 8 + Col;
            MaxSeatId := Integer'Max (SeatId, MaxSeatId);
            Count := Count + 1;
            SeatIds(Count) := SeatId;
        end loop;
        -- Sort array and search for missing seat
        Sort (SeatIds);
        SeatId := SeatIds (1);
        for I in 2 .. SeatIds'Last loop
            if (SeatIds(I) - SeatId) /= 1 then
                Put ("Missing seat: " & Integer'Image(SeatId + 1));
                New_Line;
                exit;
            end if;
            SeatId := SeatIds(I);
        end loop;
    end;

    Close (InputFile);

    Put ("Highest seat id: ");
    Put (MaxSeatId, 4);
    New_Line;

end Day5_Part2;
