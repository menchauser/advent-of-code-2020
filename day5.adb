with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed, Ada.Strings.Maps; use Ada.Strings;

procedure Day5 is
    subtype Code_Type is String (1 .. 10);

    function To_Number (Code : String; M : Maps.Character_Mapping) return Natural is
        DigitsCode: String := Fixed.Translate (Code, M);
    begin
        return Integer'Value("2#" & DigitsCode & "#");
    end;

    
    function Seat_Row (SeatCode : Code_Type) return Natural is
    begin
        return To_Number (SeatCode (1 .. 7), Maps.To_Mapping ("FB", "01"));
    end;

    RowM      : constant Maps.Character_Mapping := Maps.To_Mapping ("FB", "01");
    ColM      : constant Maps.Character_Mapping := Maps.To_Mapping ("LR", "01");

    InputFile : File_Type;
    SeatCode  : Code_Type;
    Row       : Natural;
    Col       : Natural;
    SeatId    : Natural := 0;
    MaxSeatId : Natural := 0;
    Count     : Natural := 0;
begin
    Open (InputFile, In_File, "input5.txt");
    while not End_OF_File (InputFile) loop
        SeatCode := Get_Line (InputFile);
        Row := To_Number (SeatCode (1 .. 7), RowM);
        Col := To_Number (SeatCode (8 .. 10), ColM);
        SeatId := Row * 8 + Col;
        MaxSeatId := Integer'Max (SeatId, MaxSeatId);
        Put (SeatCode & " row: " );
        Put (Row, 3);
        Put (", col: ");
        Put (Col, 1);
        Put (", seat id: ");
        Put (SeatId, 4);
        New_Line;
        Count := Count + 1;
    end loop;
    Close (InputFile);

    Put ("File contains lines: ");
    Put (Count);
    New_Line;

    Put ("Highest seat id: ");
    Put (MaxSeatId, 4);
    New_Line;

end Day5;
