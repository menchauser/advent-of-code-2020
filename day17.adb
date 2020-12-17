with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line; 


procedure Day17 is
    -- Contains one slice of space 
    type Slice_Array is array (Integer range <>, Integer Range <>) 
        of Character;
    -- First index: Z-layer, second index: slice rows in Z-layer
    type Space_Array is array (Integer range <>, Integer range <>, Integer range <>) 
        of Character;


    type Space4d_Array is array (
        Integer range <>,
        Integer range <>,
        Integer range <>,
        Integer range <>
    ) of Character;


    function Read_Input (File_Name : String) return Slice_Array is
        File  : File_Type;
        Width : Natural := 0;
        Count : Natural := 0;
        Line  : Unbounded_String;
    begin
        Open (File, In_File, File_Name);
        loop
            Get_Line (File, Line);
            Count := Count + 1; 
            exit when End_Of_File (File);
        end loop;
        Width := Length (Line);
        Reset (File);
        declare
            Result : Slice_Array (1 .. Count, 1 .. Width);
        begin
            Count := 1;
            loop
                Get_Line (File, Line);
                for I in 1 .. Length (Line) loop
                    Result (Count, I) := Element (Line, I);
                end loop;
                Count := Count + 1;
                exit when End_Of_File (File);
            end loop;
            Close (File);
            return Result;
        end;
    end;


    procedure Put (X : Slice_Array) is
    begin
        for I in X'Range (1) loop
            for J in X'Range (2) loop
                Put (X (I, J));
            end loop;
            New_Line;
        end loop;
    end;


    procedure Put (X : Space_Array) is
    begin
        for I in X'Range (1) loop
            Put ("z="); Put (I, 0); New_Line;
            for J in X'Range (2) loop
                for K in X'Range (3) loop
                    Put (X (I, J, K));
                end loop;
                New_Line;
            end loop;
            New_Line;
        end loop;
    end;


    procedure Put (X : Space4d_Array) is
    begin
        for I in X'Range (1) loop
            for J in X'Range (2) loop
                Put ("z="); Put (J, 0); 
                Put (", w="); Put (I, 0);
                New_Line;
                for K in X'Range (3) loop
                    for L in X'Range (4) loop
                        Put (X (I, J, K, L));
                    end loop;
                    New_Line;
                end loop;
                New_Line;
            end loop;
        end loop;
    end;

    -- Note that first index is Z slice, then X, then Y
    procedure Calc_Bounds (
        Input   : Space_Array;
        -- Current coordinates: Z, X, Y
        I, J, K : Integer;
        StartZ, EndZ : out Integer;
        StartX, EndX : out Integer;
        StartY, EndY : out Integer
    ) is
    begin
        StartZ := Integer'Max (Input'First (1), I - 1);
        EndZ   := Integer'Min (I + 1, Input'Last (1));

        StartX := Integer'Max (Input'First (2), J - 1);
        EndX   := Integer'Min (J + 1, Input'Last (2));

        StartY := Integer'Max (Input'First (3), K - 1);
        EndY   := Integer'Min (K + 1, Input'Last (3));
    end;


    procedure Calc_Bounds (
        Input   : Space4d_Array;
        -- Current coordinates: W, Z, X, Y
        I, J, K, L : Integer;
        StartW, EndW : out Integer;
        StartZ, EndZ : out Integer;
        StartX, EndX : out Integer;
        StartY, EndY : out Integer
    ) is
    begin
        StartW := Integer'Max (Input'First (1), I - 1);
        EndW   := Integer'Min (I + 1, Input'Last (1));

        StartZ := Integer'Max (Input'First (2), J - 1);
        EndZ   := Integer'Min (J + 1, Input'Last (2));

        StartX := Integer'Max (Input'First (3), K - 1);
        EndX   := Integer'Min (K + 1, Input'Last (3));

        StartY := Integer'Max (Input'First (4), L - 1);
        EndY   := Integer'Min (L + 1, Input'Last (4));
    end;


    function Count_Neighbours (
        Input   : Space_Array;
        Z, X, Y : Integer
    ) return Natural is
        StartZ, EndZ : Integer;
        StartX, EndX : Integer;
        StartY, EndY : Integer;
        Count        : Natural := 0;
    begin
        Calc_Bounds (Input, Z, X, Y, StartZ, EndZ, StartX, EndX, StartY, EndY);
        for I in StartZ .. EndZ loop
            for J in StartX .. EndX loop
                for K in StartY .. EndY loop
                    if Input (I, J, K) = '#' then
                        Count := Count + 1;
                    end if;
                end loop;
            end loop;
        end loop;
        if Input (Z, X, Y) = '#' then
            Count := Count - 1;
        end if;
        return Count;
    end;


    function Count_Neighbours4d (
        Input   : Space4d_Array;
        W, Z, X, Y : Integer
    ) return Natural is
        StartW, EndW : Integer;
        StartZ, EndZ : Integer;
        StartX, EndX : Integer;
        StartY, EndY : Integer;
        Count        : Natural := 0;
    begin
        Calc_Bounds (Input, W, Z, X, Y, StartW, EndW, StartZ, EndZ, StartX, EndX, StartY, EndY);
        for I in StartW .. EndW loop
            for J in StartZ .. EndZ loop
                for K in StartX .. EndX loop
                    for L in StartY .. EndY loop
                        if Input (I, J, K, L) = '#' then
                            Count := Count + 1;
                        end if;
                    end loop;
                end loop;
            end loop;
        end loop;
        if Input (W, Z, X, Y) = '#' then
            Count := Count - 1;
        end if;
        return Count;
    end;


    function Cycle (X : Space_Array) return Space_Array is
        X1 : Space_Array(
            (X'First (1) - 1) .. (X'Last (1) + 1),
            (X'First (2) - 1) .. (X'Last (2) + 1),
            (X'First (3) - 1) .. (X'Last (3) + 1)
        );
        Count : Natural := 0;
    begin
        for C of X1 loop
            C := '.';
        end loop;
        for I in X'Range (1) loop
            for J in X'Range (2) loop
                for K in X'Range (3) loop
                    Count := Count_Neighbours (X, I, J, K);
                    if X (I, J, K) = '#' then
                        if Count < 2 or Count > 3 then
                            X1 (I, J, K) := '.';
                        else
                            X1 (I, J, K) := '#';
                        end if;
                    else
                        if Count = 3 then
                            X1 (I, J, K) := '#';
                        else
                            X1 (I, J, K) := '.';
                        end if;
                    end if;
                end loop;
            end loop;
        end loop;
        return X1;
    end;


    function Cycle4d (X : Space4d_Array) return Space4d_Array is
        X1 : Space4d_Array(
            (X'First (1) - 1) .. (X'Last (1) + 1),
            (X'First (2) - 1) .. (X'Last (2) + 1),
            (X'First (3) - 1) .. (X'Last (3) + 1),
            (X'First (4) - 1) .. (X'Last (4) + 1)
        );
        Count : Natural := 0;
    begin
        for C of X1 loop
            C := '.';
        end loop;
        for I in X'Range (1) loop
            for J in X'Range (2) loop
                for K in X'Range (3) loop
                    for L in X'Range (4) loop
                        Count := Count_Neighbours4d (X, I, J, K, L);
                        if X (I, J, K, L) = '#' then
                            if Count < 2 or Count > 3 then
                                X1 (I, J, K, L) := '.';
                            else
                                X1 (I, J, K, L) := '#';
                            end if;
                        else
                            if Count = 3 then
                                X1 (I, J, K, L) := '#';
                            else
                                X1 (I, J, K, L) := '.';
                            end if;
                        end if;
                    end loop;
                end loop;
            end loop;
        end loop;
        return X1;
    end;


    -- Main variables
    Input : Slice_Array := Read_Input (Ada.Command_Line.Argument (1));
    X   : Space_Array (
        -1 .. 1, 
        (Input'First (1) - 1) .. (Input'Last (1) + 1),
        (Input'First (2) - 1) .. (Input'Last (2) + 1)
    );
    X4d : Space4d_Array (
        -1 .. 1, 
        -1 .. 1, 
        (Input'First (1) - 1) .. (Input'Last (1) + 1),
        (Input'First (2) - 1) .. (Input'Last (2) + 1)
    );
begin
    -- Part 1
    Put_Line ("Read input: ");
    Put (Input);

    -- Fill source 3d space
    for C of X loop
        C := '.';
    end loop;
    for I in Input'Range (1) loop
        for J in Input'Range (2) loop
            X (0, I, J) := Input (I, J);
        end loop;
    end loop;

    declare
        X1 : Space_Array := Cycle (X);
        X2 : Space_Array := Cycle (X1);
        X3 : Space_Array := Cycle (X2);
        X4 : Space_Array := Cycle (X3);
        X5 : Space_Array := Cycle (X4);
        X6 : Space_Array := Cycle (X5);
        Count : Natural := 0;
    begin
        for C of X6 loop
            if C = '#' then
                Count := Count + 1;
            end if;
        end loop;
        Put ("Part 1: executed 6 cycles. Cube count: ");
        Put (Count, 0);
        New_Line;
    end;

    -- Part 2
    for C of X4d loop
        C := '.';
    end loop;
    for I in Input'Range (1) loop
        for J in Input'Range (2) loop
            X4d (0, 0, I, J) := Input (I, J);
        end loop;
    end loop;

    declare
        X1 : Space4d_Array := Cycle4d (X4d);
        X2 : Space4d_Array := Cycle4d (X1);
        X3 : Space4d_Array := Cycle4d (X2);
        X4 : Space4d_Array := Cycle4d (X3);
        X5 : Space4d_Array := Cycle4d (X4);
        X6 : Space4d_Array := Cycle4d (X5);
        Count : Natural := 0;
    begin
        for C of X6 loop
            if C = '#' then
                Count := Count + 1;
            end if;
        end loop;
        Put ("Part 2: executed 6 cycles. Cube count: ");
        Put (Count, 0);
        New_Line;
    end;

end Day17;
