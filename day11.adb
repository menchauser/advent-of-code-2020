with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
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
                    Result (I, J) := Element (Line, J);
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
        for I in StartRow .. EndRow loop
            for J in StartCol .. EndCol loop
                if (I /= Row or J /= Col) and Input (I, J) = '#' then
                    Result := Result + 1;
                end if;
            end loop;
        end loop; 
        return Result;
    end;

    
    -- Return 1 if first seat visible from (Row, Col) in direction determined 
    -- by X, Y is occupied
    function Occupied_Direction (
        Input    : Input_T;
        Row, Col : Positive;
        X, Y     : Integer
        ) return Natural 
    is
        I : Natural := Row + X;
        J : Natural := Col + Y;
    begin
        while I > 0 
            and then J > 0 
            and then I <= Input'Last(1)
            and then J <= Input'Last(2) loop
            if Input (I, J) = 'L' then
                return 0;
            elsif Input (I, J) = '#' then
                return 1;
            end if;
            I := I + X;
            J := J + Y;
        end loop;
        return 0;
    end;

   
    function Count_Occupied_Dir (Input : Input_T; Row, Col : Positive) return Natural is
        Result : Natural := 0;
    begin
        -- go clockwise from right
        return
            Occupied_Direction (Input, Row, Col,    0,  1)  -- right
            + Occupied_Direction (Input, Row, Col,  1,  1)  -- down-right
            + Occupied_Direction (Input, Row, Col,  1,  0)  -- down
            + Occupied_Direction (Input, Row, Col,  1, -1)  -- down-left
            + Occupied_Direction (Input, Row, Col,  0, -1)  -- left
            + Occupied_Direction (Input, Row, Col, -1, -1)  -- up-left
            + Occupied_Direction (Input, Row, Col, -1,  0)  -- up
            + Occupied_Direction (Input, Row, Col, -1,  1); -- up-right
    end;


    function Populate (
        Input : Input_T; 
        Count_Adj : access function (I : Input_T; Row, Col : Positive) return Natural;
        Limit : Natural
    ) return Input_T 
    is
        Result : Input_T := Input;
    begin
        for I in Input'Range (1) loop
            for J in Input'Range (2) loop
                if Input (I, J) = 'L' and then Count_Adj (Input, I, J) = 0 then
                    Result (I, J) := '#';
                elsif Input (I, J) = '#' and then Count_Adj (Input, I, J) >= Limit then
                    Result (I, J) := 'L';
                else
                    Result (I, J) := Input (I, J);
                end if;
            end loop;
        end loop;
        return Result;
    end;


    function Get_Stable_Result (
        Input : Input_T; 
        Count_Adj : access function (I : Input_T; Row, Col : Positive) return Natural;
        Limit : Natural
    ) return Natural 
    is
        Current  : Input_T := Input;
        Next     : Input_T := Populate (Current, Count_Adj, Limit);
        Occupied : Natural := 0;
    begin
        -- Run Simulation
        loop
            if Next = Current then
                -- found stable solution
                for I in Next'Range (1) loop
                    for J in Next'Range (2) loop
                        if Next (I, J) = '#' then
                            Occupied := Occupied + 1;
                        end if;
                    end loop;
                end loop;
                return Occupied;
            end if;
            Current := Next;
            Next := Populate (Current, Count_Adj, Limit);
        end loop;
    end;

    -- Main variables
    Input  : Input_T := Read_Input (Ada.Command_Line.Argument (1));
    Result : Natural;
begin
    -- Run Simulation
    Result := Get_Stable_Result (Input, Count_Occupied_Adj'Access, 4);
    Put ("Part 1 Result: ");
    Put (Result, 0);
    New_Line;

    Result := Get_Stable_Result (Input, Count_Occupied_Dir'Access, 5);
    Put ("Part 2 Result: ");
    Put (Result, 0);
    New_Line;

end Day11;
