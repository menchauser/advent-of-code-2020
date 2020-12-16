with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Integer_Text_IO; use Ada.Long_Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line; 
with Ada.Containers.Vectors;


procedure Day16 is
    Number_Set : Character_Set := To_Set ("0123456789");


    type Field_Range is
        record
            Name                       : Unbounded_String;
            Start1, End1, Start2, End2 : Natural;
        end record;
    

    -- class: 1-3 or 5-7  =>  (1, 3, 5, 7)
    function Parse_Range (Input : Unbounded_String) return Field_Range is
        Start  : Positive := 1;
        Finish : Natural := 0;
        Name   : Unbounded_String;
        Start1, End1, Start2, End2 : Natural;
    begin
        Find_Token (Input, Number_Set, Finish + 1, Inside, Start, Finish);
        Start1 := Natural'Value (Slice (Input, Start, Finish));

        Find_Token (Input, Number_Set, Finish + 1, Inside, Start, Finish);
        End1 := Natural'Value (Slice (Input, Start, Finish));
        
        Find_Token (Input, Number_Set, Finish + 1, Inside, Start, Finish);
        Start2 := Natural'Value (Slice (Input, Start, Finish));

        Find_Token (Input, Number_Set, Finish + 1, Inside, Start, Finish);
        End2 := Natural'Value (Slice (Input, Start, Finish));

        Name := Unbounded_Slice (Input, 1, Index (Input, ":") - 1);
        return (Name, Start1, End1, Start2, End2);
    end;


    function Includes (In_Range : Field_Range; Value : Natural) return Boolean is
    begin
        return (Value >= In_Range.Start1 and Value <= In_Range.End1) 
            or (Value >= In_Range.Start2 and Value <= In_Range.End2);
    end;


    package Field_Range_V is new Ada.Containers.Vectors (
        Index_Type   => Positive,
        Element_Type => Field_Range
    );


    -- check that value is included in any range
    function Includes (In_Range : Field_Range_V.Vector; Value : Natural) return Boolean is
    begin
        for R of In_Range loop
            if Includes (R, Value) then
                return True;
            end if;
        end loop;
        return False;
    end;


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


    -- Main variables
    Input_File : File_Type;
    Rules      : Field_Range_V.Vector;
    Line       : Unbounded_String;
    Result     : Natural := 0;
begin
    -- Part 1
    Open (Input_File, In_File, Ada.Command_Line.Argument (1));

    -- First we read lines up until empty line: ranges
    loop
        Get_Line (Input_File, Line);
        exit when Line = "";
        Rules.Append (Parse_Range (Line));
    end loop;

    Put_Line ("Parsed rules: ");
    for X of Rules loop
        Put (X.Start1, 0); Put ("-"); Put (X.End1, 0);
        Put (" or ");
        Put (X.Start2, 0); Put ("-"); Put (X.End2, 0);
        New_Line;
    end loop;
    
    -- Until second empty line: your ticket
    loop
        Get_Line (Input_File, Line);
        exit when Line = "";
    end loop;

    -- Other lines: nearby tickets
    -- Skip line "nearby tickets"
    Get_Line (Input_File, Line);

    Put_Line ("Count completely invalid tickets");
    loop
        Get_Line (Input_File, Line);
        Put_Line (Line);
        declare 
            Values : Nat_Array := Parse_Array (Line);
        begin
            for X of Values loop
                if not Includes (Rules, X) then
                    Put ("Value "); Put (X, 0); Put (" is not in declared ranges");
                    New_Line;
                    Result := Result + X;
                    exit;
                end if;
            end loop;
        end;
        exit when End_Of_File (Input_File);
    end loop;

    Close (Input_File);

    Put ("Part 1 result: ");
    Put (Result, 0);
    New_Line;

end Day16;
