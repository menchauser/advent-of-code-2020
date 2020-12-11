with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Integer_Text_IO; use Ada.Long_Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Vectors;
with Ada.Command_Line; 


procedure Day11 is
    type Input_T is array (Positive range <>, Positive range <>) of Character;

    function Read_Input (FileName : String) return Input_T is
        File  : File_Type;
        Line  : Unbounded_String;
        Width : Natural := 0;
        Count : Natural := 0;
    begin
        Open (File, In_File, FileName);
        loop
            Line := Get_Line (File);
            Width := Natural'Max (Width, Length (Line));
            Count := Count + 1;
            exit when End_Of_File (File);
        end loop;
        Put ("Max row: "); Put (Count, 0); New_Line;
        Put ("Max col: "); Put (Width, 0); New_Line;
        declare
            I      : Positive;
            Result : Input_T (1 .. Count, 1 .. Width);
        begin
            Reset (File);
            I := 1;
            loop 
                Line := Get_Line (File);
                for J in 1 .. Length (Line) loop
                    Result(I, J) := Element (Line, J);
                end loop;
                I := I + 1;
                exit when End_Of_File (File);
            end loop;
            Close (File);
            return Result;
        end;
    end;

    function Count_Occupied_Adj (Input : Input_T; Row, Col : Positive) return Natural is
        StartRow : Positive := Integer'Max (1, Row - 1);
        EndRow   : Positive := Integer'Min (Row + 1, Input'Last (1));
        StartCol : Positive := Integer'Max (1, Col - 1);
        EndCol   : Positive := Integer'Min (Col + 1, Input'Last (2));
        Result : Natural := 0;
    begin
        -- Put ("Count adjacents of "); Put (Row, 0); Put(','); Put (Col, 0); New_Line;
        for I in StartRow .. EndRow loop
            for J in StartCol .. EndCol loop
                -- Put (Input (I, J));
                if (I /= Row or J /= Col) and Input (I, J) = '#' then
                    Result := Result + 1;
                end if;
            end loop;
            -- New_Line;
        end loop; 
        -- Put ("count = "); Put (Result, 0); New_Line;
        
        return Result;
    end;

    function Populate (Input : Input_T) return Input_T is
        Result : Input_T := Input;
    begin
        for I in Input'Range (1) loop
            for J in Input'Range (2) loop
                if Input (I, J) = 'L' and then Count_Occupied_Adj (Input, I, J) = 0 then
                    Result (I, J) := '#';
                elsif Input (I, J) = '#' and then Count_Occupied_Adj (Input, I, J) >= 4 then
                    Result (I, J) := 'L';
                else
                    Result (I, J) := Input (I, J);
                end if;
            end loop;
        end loop;
        return Result;
    end;

    -- Main variables
    Input     : Input_T := Read_Input (Ada.Command_Line.Argument (1));
begin
    -- for I in Input'Range (1) loop
    --     for J in Input'Range (2) loop
    --         Put (Input (I, J));
    --     end loop;
    --     New_Line;
    -- end loop;

    -- Run Simulation
    declare 
        Current  : Input_T := Input;
        Next     : Input_T := Populate (Current);
        Occupied : Natural := 0;
    begin
        loop
            -- for I in Next'Range (1) loop
            --     for J in Next'Range (2) loop
            --         Put (Next (I, J));
            --     end loop;
            --     New_Line;
            -- end loop;
            -- New_Line;
            if Next = Current then
                Put_Line ("Found stable solution");
                for I in Next'Range (1) loop
                    for J in Next'Range (2) loop
                        if Next (I, J) = '#' then
                            Occupied := Occupied + 1;
                        end if;
                    end loop;
                end loop;
                Put ("Result: "); 
                Put (Occupied, 0);
                New_Line;
                exit;
            end if;
            Current := Next;
            Next := Populate (Current);
        end loop;
    end;

end Day11;
