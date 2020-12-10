with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Vectors;


procedure Day10 is
    package Int_Vector_T is new Ada.Containers.Vectors
        (Element_Type => Integer,
         Index_Type   => Natural);
    package Int_Vector_Sorter is new Int_Vector_T.Generic_Sorting;
    
    -- Main variables
    InputFile : File_Type;
    Line      : Unbounded_String;
    Count     : Natural := 0;
    Input     : Int_Vector_T.Vector := Int_Vector_T.Empty_Vector;
    Next      : Integer;
    Diff_Counts : array (1 .. 3) of Natural := (others => 0);
begin
    -- Read File
    Open (InputFile, In_File, "input10.txt");
    loop 
        Get (InputFile, Next);
        Input.Append (Next);
        exit when End_Of_File (InputFile);
    end loop;
    Close (InputFile);

    Int_Vector_Sorter.Sort (Input);

    Put_Line ("Sorted vector: ");
    for I in Input.First_Index .. Input.Last_Index loop
        Put (Input (I), 0); Put (' '); 
    end loop;
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
    Put ("Result: ");
    Put (Diff_Counts(1) * Diff_Counts(3));
end Day10;
