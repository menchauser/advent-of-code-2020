with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Integer_Text_IO; use Ada.Long_Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Vectors;


procedure Day9 is
    PreambleSize : constant Positive := 25;
    type Index_T is range 0 .. (PreambleSize - 1);
    type Preamble_T is array (Index_T) of Long_Integer;

    package Int_Vector_T is new Ada.Containers.Vectors
        (Element_Type => Long_Integer,
         Index_Type   => Natural);
    
    function Is_Sum_Of (Value : Long_Integer; Addends : Preamble_T) return Boolean is
    begin
        for I in Addends'Range loop
            for J in (I + 1) .. Addends'Last loop
                if Value = Addends (I) + Addends (J) then
                    return True;
                end if;
            end loop;
        end loop;
        return False;
    end;
    
    -- Main variables
    InputFile : File_Type;
    Line      : Unbounded_String;
    Count     : Natural := 0;
    Input     : Int_Vector_T.Vector := Int_Vector_T.Empty_Vector;
    Addends   : Preamble_T;
    Next      : Long_Integer;
    Invalid   : Long_Integer;
    -- Variables for Part 2
    PartSum   : Long_Integer;
    Smallest  : Long_Integer := Long_Integer'Last;
    Largest   : Long_Integer := 0;
    StartIdx  : Natural;
begin
    Put_Line ("Start");
    -- Read File
    Open (InputFile, In_File, "input9.txt");
    loop 
        Get (InputFile, Next);
        Input.Append (Next);
        exit when End_Of_File (InputFile);
    end loop;
    Close (InputFile);

    for I in 0 .. Addends'Last loop
        Addends(I) := Input(Natural(I));
        Count := Count + 1;
    end loop;

    loop
        Next := Input (Count);
        Count := Count + 1;
        if Is_Sum_Of (Next, Addends) then
            Addends(Index_T((Count - 1) mod PreambleSize)) := Next;
        else 
            Put ("Found invalid value: ");
            Put (Next);
            New_Line;
            Invalid := Next;
            exit;
        end if;
        exit when Count > Input.Last_Index;
    end loop;
    
    -- Now we have invalid number, reset file cursor and do part 2
    Put_Line ("Searching for encryption weakness");
    StartIdx := 0;
    Count := StartIdx;
    PartSum := 0;
    loop
        Next := Input (Count); Count := Count + 1;
        Put (Next, 0); Put (" + ");
        if Next = Invalid then
            Put_Line ("Encountered invalid value. Skip");
        elsif Next > Invalid then
            Put ("Encountered value "); Put (Next, 0); Put_Line (" > invalid. Reset");
            PartSum := 0;
            Smallest := Long_Integer'Last;
            Largest := 0;
            StartIdx := StartIdx + 1;
            Count := StartIdx;
        else
            Smallest := Long_Integer'Min (Smallest, Next);
            Largest  := Long_Integer'Max (Largest, Next);
            PartSum := PartSum + Next;
            if PartSum = Invalid then
                Put_Line ("Found invalid subsequence");
                Put ("Smallest = "); Put (Smallest, 0);
                Put (", Largest = "); Put (Largest, 0);
                Put (", Weakness = "); Put (Smallest + Largest, 0);
                New_Line;
                exit;
            end if;
            if PartSum > Invalid then
                Put ("Partial sum "); Put (PartSum, 0); Put_Line(" is greater then invalid value. Reset");
                PartSum := 0;
                Smallest := Long_Integer'Last;
                Largest := 0;
                StartIdx := StartIdx + 1;
                Count := StartIdx;
            end if;
        end if;
        exit when StartIdx > Input.Last_Index;
    end loop;

end Day9;
