with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Integer_Text_IO; use Ada.Long_Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Vectors;
with Ada.Command_Line; 


procedure Day13 is
    type Bus_T is array (Positive range <>) of Positive;
    package Nat_Vec is new Ada.Containers.Vectors 
        (Index_Type   => Natural,
         Element_Type => Natural);
    package String_Vec is new Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Unbounded_String);

    function Split (Input : Unbounded_String) return String_Vec.Vector is
        Start  : Positive := 1;
        Finish : Natural := 0;
        TmpStr : Unbounded_String;
        Output : String_Vec.Vector := String_Vec.Empty_Vector;
    begin
        while Start <= Length(Input) loop
            Find_Token (Input, To_Set (","), Start, Outside, Start, Finish);
            exit when Start > Finish;
            TmpStr := Unbounded_Slice (Input, Start, Finish);
            Output.Append (TmpStr);
            Start := Finish + 1;
        end loop;
        return Output;
    end Split;


    -- Main variables
    InputFile : File_Type;
    Timestamp : Natural;
    -- BusId = 0 means that there are no restrictions on departure 
    BusIds    : Nat_Vec.Vector := Nat_Vec.Empty_Vector;
begin
    Open (InputFile, In_File, Ada.Command_Line.Argument (1));
    Timestamp := Integer'Value (Get_Line (InputFile));
    declare 
        BusLine : Unbounded_String;
    begin 
        BusLine := Get_Line (InputFile);
        Close (InputFile);
        for X of Split (BusLine) loop
            if X = "x" then
                BusIds.Append (0);
            else
                BusIds.Append (Integer'Value (To_String (X)));
            end if;
        end loop;
    end;

    Put ("Earliest timestamp: "); Put (Timestamp, 0); New_Line;

    -- Part 1
    declare
        Ratio      : Natural;
        Bus_Shift  : Natural;
        Min_Shift  : Natural := Natural'Last;
        Min_Bus_Id : Natural;
    begin
        for X of BusIds loop
            if X /= 0 then
                Ratio := Timestamp / X;
                Bus_Shift := X * (Ratio + 1) - Timestamp;
                if Bus_Shift < Min_Shift then
                    Min_Shift := Bus_Shift;
                    Min_Bus_Id := X;
                end if;
            end if;
        end loop;
        Put ("Min shift: "); Put (Min_Shift, 0); New_Line;
        Put ("Min bus id: "); Put (Min_Bus_Id, 0); New_Line;
        Put ("Part 1 result: "); Put (Min_Shift * Min_Bus_Id, 0); New_Line;
    end;

    -- Part 2
    declare 
        function Is_Good_Sequence (
            Start : Long_Integer; 
            StartIdx : Natural; 
            BusIds : Nat_Vec.Vector
        ) return Boolean 
        is
            Next : Long_Integer;
        begin
            -- Put ("Check sequence starting with "); Put (Start, 0); New_Line;

            for I in BusIds.First_Index .. (StartIdx - 1) loop
                Next := Long_Integer (BusIds.Element (I));
                -- Put (I, 0); Put (":"); Put (Next, 0);
                if Next /= 0 then
                    -- Put ("\");
                    -- Put (Start - Long_Integer (StartIdx - I), 0);
                    if ((Start - Long_Integer (StartIdx - I)) mod Next) /= 0 then
                        -- New_Line;
                        return False;
                    end if;
                end if;
                -- Put (" ");
            end loop;

            for I in (StartIdx + 1) .. BusIds.Last_Index loop
                Next := Long_Integer (BusIds.Element (I));
                -- Put (I, 0); Put (":"); Put (Next, 0);
                if Next /= 0 then
                    -- Put ("\");
                    -- Put (Start + Long_Integer (I - StartIdx), 0);
                    if ((Start + Long_Integer (I - StartIdx)) mod Next) /= 0 then
                        -- New_Line;
                        return False;
                    end if;
                end if;
                -- Put (" ");
            end loop;
            -- New_Line;
            return True;
        end;

        MaxId    : Long_Integer := 0;
        MaxIdIdx : Natural;
        Iter     : Long_Integer;
    begin
        for I in BusIds.First_Index .. BusIds.Last_Index loop
            if Long_Integer (BusIds.Element (I)) > MaxId then
                MaxId := Long_Integer (BusIds.Element (I));
                MaxIdIdx := I;
            end if;
        end loop;
        Put ("Max Id: "); Put (MaxId, 0);
        Put (", index: "); Put (MaxIdIdx, 0);
        New_Line;
        Iter := 100000000000000 / MaxId;
        loop
            exit when Is_Good_Sequence (MaxId * Iter, MaxIdIdx, BusIds);
            Iter := Iter + 1;
        end loop;

        Put ("Part 2 result: "); 
        Put (MaxId * Iter - Long_Integer (MaxIdIdx), 0); 
        New_Line;
        Put ("Used iterations: ");
        Put (Iter, 0);
        New_Line;
    end;

end Day13;
