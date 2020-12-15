with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Integer_Text_IO; use Ada.Long_Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line; 


procedure Day15 is
    type Nat_Array is array (Natural range <>) of Natural;


    function Parse_Array (Input : Unbounded_String) return Nat_Array is
        Size   : Natural := Ada.Strings.Unbounded.Count (Input, ",") + 1;
        Result : Nat_Array (1 .. Size);
        First  : Positive := 1;
        Last   : Natural := 0;
        I      : Natural := 1;
    begin
        while First <= Length (Input) loop
            Find_Token (Input, To_Set (","), First, Outside, First, Last);
            exit when First > Last;
            Result (I) := Natural'Value (Slice (Input, First, Last));
            I := I + 1;
            First := Last + 1;
        end loop;
        return Result;
    end;

    function Read_Input (File : String) return Nat_Array is
        InputFile : File_Type;
    begin
        Open (InputFile, In_File, Ada.Command_Line.Argument (1));
        declare 
            Input : Nat_Array := Parse_Array (Get_Line (InputFile));
        begin
            Close (InputFile);
            return Input;
        end;
    end;


    type Dyn_Array is array (Natural range <>) of Natural;
    type Dyn_Array_Access is access Dyn_Array;
    -- Main variables
    Input   : Nat_Array := Read_Input (Ada.Command_Line.Argument (1));
    Count   : Natural := Natural'Value (Ada.Command_Line.Argument (2));
    Data    : Dyn_Array_Access := new Dyn_Array (0 .. Count);
    LastNum : Natural;
    NextNum : Natural;
    Turn    : Natural := 1;
begin
    for X of Input loop
        Data (X) := Turn;
        Turn := Turn + 1;
    end loop;

    -- first will always be X => 0
    LastNum := Input (Input'Last);
    NextNum := 0;
    Data (LastNum) := Turn - 1;
   
    for Turn in (Input'Last + 2) .. Count loop
        LastNum := NextNum;
        if Data (LastNum) = 0 then
            NextNum := 0;
        else
            NextNum := Turn - 1 - Data (LastNum);
        end if;
        Data (LastNum) := Turn - 1;
    end loop;

    Put (NextNum, 0); New_Line;

end Day15;
