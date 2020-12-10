with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Integer_Text_IO; use Ada.Long_Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Vectors;
with Ada.Command_Line; 


procedure Day10 is
    package Int_Vector_T is new Ada.Containers.Vectors
        (Element_Type => Integer,
         Index_Type   => Natural);
    package Int_Vector_Sorter is new Int_Vector_T.Generic_Sorting;
    
    procedure Put (V : Int_Vector_T.Vector) is
        First : Boolean := True;
    begin
        for X of V loop
            if First then
                First := False;
            else
                Put (' ');
            end if;
            Put (X, 0);
        end loop;
    end;


    -- Main variables
    InputFile : File_Type;
    Line      : Unbounded_String;
    Count     : Natural := 0;
    Input     : Int_Vector_T.Vector := Int_Vector_T.Empty_Vector;
    Next      : Integer;
    Diff_Counts : array (1 .. 3) of Natural := (others => 0);
begin
    -- Read File
    Open (InputFile, In_File, Ada.Command_Line.Argument (1));
    loop 
        Get (InputFile, Next);
        Input.Append (Next);
        exit when End_Of_File (InputFile);
    end loop;
    Close (InputFile);

    Int_Vector_Sorter.Sort (Input);

    Put_Line ("Sorted vector: ");
    Put (Input);
    New_Line;

    declare 
        Diff : Integer;
        Tmp  : Natural;
    begin
        for I in (Input.First_Index + 1) .. Input.Last_Index loop
            Diff := Input(I) - Input(I - 1);
            Tmp := Diff_Counts(Diff);
            Diff_Counts(Diff) := Tmp + 1;
        end loop;
        -- Add start and ending diffs
        Diff := Input.First_Element;
        Diff_Counts(Diff) := Diff_Counts(Diff) + 1;
        Diff_Counts(3) := Diff_Counts(3) + 1;
    end;

    for I in Diff_Counts'Range loop
        Put (I, 0); 
        Put (": ");
        Put (Diff_Counts(I));
        New_Line;
    end loop;
    Put ("Part 1 result: ");
    Put (Diff_Counts(1) * Diff_Counts(3));
    New_Line;

    -- For part 2 we can start to go from end and remove 1-2 diffs
    -- Test_Permutations (Input);
    -- let's find out longest groups of succeeding numbers
    declare 
        Diff : Integer := 0;
        GroupLength : Natural := 1;
        Result : Long_Integer := 1;
        Multipliers : array (1 .. 5) of Long_Integer := (1, 1, 2, 4, 7);
    begin
        Input.Prepend (0);
        Input.Append (Input.Last_Element + 3);
        Put (Input); New_Line;
        for I in Input.First_Index .. Input.Last_Index - 1 loop
            Diff := Input(I + 1) - Input(I);
            if Input(I + 1) - Input(I) = 1 then
                GroupLength := GroupLength + 1;
            else
                Put ("Group finished, length: ");
                Put (GroupLength);
                New_Line;
                if GroupLength > 1 then
                    Result := Result * Multipliers(GroupLength);
                    GroupLength := 1;
                end if;
            end if;
        end loop;
        if GroupLength > 0 then
            Put ("Group finished, length: ");
            Put (GroupLength);
            New_Line;
            if GroupLength > 1 then
                Result := Result * Multipliers(GroupLength);
            end if;
        end if;

        Put ("Part 2 Result: ");
        Put (Result);
        New_Line;
    end;


end Day10;
