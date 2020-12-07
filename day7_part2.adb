with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Hash;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Hashed_Maps; 

procedure Day7_Part2 is
    -- Bag descriptor
    type Bag_Descriptor is
        record
            Count : Positive;
            Color : Unbounded_String;
        end record;

    package Bag_Vectors is new Ada.Containers.Indefinite_Vectors (Natural, Bag_Descriptor);
    use Bag_Vectors;

    -- Represents one line from source file
    type Bag_Mapping is
        record
            SourceBag  : Unbounded_String;
            TargetBags : Bag_Vectors.Vector;
        end record;

    -- Parse string like '3 faded plum bags' to record
    function Parse_Record (Input : Unbounded_String) return Bag_Descriptor is
        Output : Bag_Descriptor;
        NumStr : String := Slice (Input, 1, Index (Input, " ") - 1);
        Color  : Unbounded_String := Unbounded_Slice (
            Input, 
            Index (Input, " ") + 1, 
            Index (Input, " ", Backward) - 1
        );
    begin
        Output := 
            (Count => Positive'Value (NumStr),
             Color => Color);
        return Output;
    end;

    function Parse_Line (Input : Unbounded_String) return Bag_Mapping is
        -- bounds of ' contain ' delimiter
        DelimStart   : Natural := Index (Input, " contain ");
        DelimEnd     : Natural := DelimStart + 8;
        SourceBag    : Unbounded_String := Unbounded_Slice (Input, 1, DelimStart - 6);
        TargetBagStr : Unbounded_String := Unbounded_Slice (Input, DelimEnd + 1, Length (Input) - 1); 
        TargetBags   : Bag_Vectors.Vector := Bag_Vectors.Empty_Vector;
        -- temporary variables for tokenization
        Start        : Positive := 1;
        Finish       : Natural := 0;
        CurrentRec   : Unbounded_String;
    begin
        if TargetBagStr /= "no other bags" then
            while Start <= Length (TargetBagStr) loop
                Find_Token (TargetBagStr, To_Set (","), Start, Outside, Start, Finish);
                exit when Start > Finish;
                CurrentRec := Trim (Unbounded_Slice (TargetBagStr, Start, Finish), To_Set(" "), To_Set(" "));
                TargetBags.Append (Parse_Record (CurrentRec));
                Start := Finish + 1;
            end loop;
        end if;
        return (SourceBag => SourceBag, TargetBags => TargetBags);
    end;

    
    -- Map from bags to bags
    function Hash_Func(Key : Unbounded_String) return Ada.Containers.Hash_Type is
    begin 
        return Ada.Strings.Hash(To_String(Key));
    end Hash_Func;

    function Eq_Key (Left, Right : Unbounded_String) return Boolean is
    begin
        return Left = Right;
    end Eq_Key;
    
    package Bag_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type => Unbounded_String,
         Element_Type => Bag_Vectors.Vector,
         Hash => Hash_Func,
         Equivalent_Keys => Eq_Key);

    function Count_Result (InMap : Bag_Maps.Map; Root : Unbounded_String) return Natural is
        Result  : Natural := 0;
        Queue   : Bag_Vectors.Vector;
        CurDesc : Bag_Descriptor;
    begin
        if InMap.Contains (Root) then
            -- enqueue first-level accessible elements 
            for X of InMap.Element (Root) loop
                Queue.Append (X);
            end loop;
            -- and run through queue
            loop 
                CurDesc := Queue(Queue.First_Index);
                Result := Result + CurDesc.Count;
                if InMap.Contains (CurDesc.Color) then
                    for X of InMap.Element (CurDesc.Color) loop
                        Queue.Append ((Count => CurDesc.Count * X.Count, Color => X.Color));
                    end loop;
                end if;
                Queue.Delete_First;
                exit when Queue.Is_Empty;
            end loop;
        end if;
        return Result;
    end Count_Result;


    -- Main variables
    InputFile   : File_Type;
    Line        : Unbounded_String := To_Unbounded_String("");
    Count       : Natural := 0;
    BagMap      : Bag_Maps.Map := Bag_Maps.Empty_Map;
    Result      : Natural := 0;
begin
    -- Read File
    Open (InputFile, In_File, "input7.txt");
    loop
        Line := Get_Line (InputFile);
        Count := Count + 1;
        declare 
            CurMapping : Bag_Mapping := Parse_Line (Line);
            Target     : Bag_Vectors.Vector := CurMapping.TargetBags;   
        begin
            BagMap.Insert (CurMapping.SourceBag, Target);
        end;
        exit when End_Of_File (InputFile);
    end loop;
    Close (InputFile);

    Put ("Lines:   ");
    Put (Count, 5);
    New_Line;

    Put ("Result:  ");
    Put (Count_Result(BagMap, To_Unbounded_String("shiny gold")));
    New_Line;
end Day7_Part2;
