with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Hash;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Hashed_Maps; 
with Ada.Containers.Hashed_Sets;

procedure Day7 is
    -- Split string utility
    package String_Vectors is new Ada.Containers.Indefinite_Vectors (Natural, Unbounded_String);
    use String_Vectors;
    procedure Split (Input : Unbounded_String; Output : out String_Vectors.Vector) is
        Start : Positive := 1;
        Finish : Natural := 0;
        TmpStr : Unbounded_String;
    begin
        while Start <= Length(Input) loop
            Find_Token (Input, To_Set (","), Start, Outside, Start, Finish);
            exit when Start > Finish;
            TmpStr := Trim (Unbounded_Slice (Input, Start, Finish), To_Set(" "), To_Set(" ")); 
            Output.Append (TmpStr);
            Start := Finish + 1;
        end loop;
    end Split;
    
    -- Map for bags to bags
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
         Element_Type => String_Vectors.Vector,
         Hash => Hash_Func,
         Equivalent_Keys => Eq_Key);

    package String_Sets is new Ada.Containers.Hashed_Sets
        (Element_Type => Unbounded_String,
         Hash => Hash_Func,
         Equivalent_Elements => Eq_Key);
    
    function Count_Result (InMap : Bag_Maps.Map; Name : Unbounded_String; Visited: in out String_Sets.Set) return Natural is
        Result : Natural := 0;
        Vec    : String_Vectors.Vector;
    begin
        Put ("Bag color containing "); Put (Name);
        New_Line;
        if InMap.Contains (Name) then
            Vec := InMap.Element (Name);
            for I in Vec.First_Index .. Vec.Last_Index loop
                Put ("  ");
                Put (Vec(I));
                New_Line;
            end loop;
            for I in Vec.First_Index .. Vec.Last_Index loop
                if not Visited.Contains (Vec(I)) then
                    Result := Result + 1 + Count_Result (InMap, Vec(I), Visited);
                end if;
            end loop;
        end if;
        Visited.Include (Name);
        return Result;
    end Count_Result;

    -- Main variables
    InputFile   : File_Type;
    Line        : Unbounded_String := To_Unbounded_String("");
    Count       : Natural := 0;
    BagMap      : Bag_Maps.Map := Bag_Maps.Empty_Map;
    SrcVector   : String_Vectors.Vector;
    Result      : Natural := 0;
begin
    -- Read File
    Open (InputFile, In_File, "input7.txt");
    loop
        Line := Get_Line (InputFile);
        Count := Count + 1;
        declare 
            DelimStartIdx : Natural := Index (Line, " contain ");
            DelimEndIdx   : Natural := DelimStartIdx + 8; -- Length of "contain "
            SrcBag        : Unbounded_String := Unbounded_Slice (Line, 1, DelimStartIdx - 1);
            DstBags       : String_Vectors.Vector;
        begin
            SrcBag := Unbounded_Slice(SrcBag, 1, Index (SrcBag, " ", Backward) - 1);
            Split (Unbounded_Slice (Line, DelimEndIdx + 1, Length (Line) - 1), DstBags);

            Put (SrcBag);
            Put (" => ");

            declare 
                -- ClearedBags : String_Vectors.Vector := String_Vectors.Empty_Vector;
                BagName     : Unbounded_String;
            begin
                for I in DstBags.First_Index .. DstBags.Last_Index loop
                    -- Trim number at start
                    BagName := Unbounded_Slice(
                        DstBags(I),
                        Index (DstBags(I), " ") + 1,
                        Length (DstBags(I)) 
                    );
                    -- Trim 'bag[s]' at end
                    BagName := Unbounded_Slice(
                        BagName,
                        1,
                        Index (BagName, " ", Backward) - 1
                    );
                    if not BagMap.Contains (BagName) then
                        BagMap.Insert (BagName, String_Vectors.Empty_Vector);
                    end if;
                    SrcVector := BagMap.Element (BagName);
                    SrcVector.Append (SrcBag);
                    BagMap.Include (BagName, SrcVector);
                    -- ClearedBags.Append (BagName);
                    Put ("'");
                    Put (BagName);
                    Put ("'");
                end loop;
                New_Line;
            end;
        end;
        exit when End_Of_File (InputFile);
    end loop;
    Close (InputFile);

    Put ("Lines:   ");
    Put (Count, 5);
    New_Line;

    declare 
        Visited: String_Sets.Set := String_Sets.Empty_Set;
    begin
        Put ("Result:  ");
        Put (Count_Result (BagMap, To_Unbounded_String("shiny gold"), Visited));
        New_Line;
    end;

end Day7;
