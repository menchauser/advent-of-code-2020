with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Integer_Text_IO; use Ada.Long_Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Command_Line; 


procedure Day18 is
    Digit_Chars : constant Character_Set := To_Set ("1234567890");
    Op_Chars    : constant Character_Set := To_Set ("+*(");

    type Node_Class is (Number, Op);

    type Node (Class : Node_Class) is record
        case Class is
            when Number =>
                Value : Long_Integer;
            when Op =>
                Op : Character;
        end case;
    end record;


    type Op_Token is record
        Op    : Character;
        Depth : Natural;
    end record;
    

    package Long_Int_Vec is new Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Long_Integer);

    package Op_Vec is new Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Op_Token);
    
    package Char_Vec is new Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Character);

    package Node_Vec is new Ada.Containers.Indefinite_Vectors
        (Index_Type   => Natural,
         Element_Type => Node);


    procedure Put (V : Node_Vec.Vector) is
        First : Boolean := True;
    begin
        Put ("RPN: ");
        for X of V loop
            if First then
                First := False;
            else
                Put (' ');
            end if;
            case X.Class is
                when Number =>
                    Put (X.Value, 0);
                when Op =>
                    Put (X.Op);
            end case;
        end loop;
    end;


    function Parse_To_RPN (Expr : Unbounded_String) return Node_Vec.Vector is
        -- Input      : Unbounded_String := Expr & ' ';
        Result     : Node_Vec.Vector := Node_Vec.Empty_Vector;
        Op_Stack   : Char_Vec.Vector := Char_Vec.Empty_Vector;
        I          : Natural := 1;
        C          : Character;
        Token      : Unbounded_String;
    begin
        loop
            C := Element (Expr, I);
            if Is_In (C, Digit_Chars) then
                Token := Token & C;
            elsif Is_In (C, Op_Chars) then
                Op_Stack.Append (C);
            elsif C = ')' then
                if Token /= "" then
                    -- last token was number, add it to result
                    -- also add token from op stack, if exists
                    Result.Append ((
                        Class => Number,
                        Value => Long_Integer'Value (To_String (Token))
                    ));
                    Token := Null_Unbounded_String;
                end if;
                loop
                    C := Op_Stack (Op_Stack.Last_Index);
                    Op_Stack.Delete_Last;
                    exit when C = '(';
                    Result.Append ((Class => Op, Op => C));
                end loop;
                if not Op_Stack.Is_Empty 
                    and then Op_Stack.Last_Element /= '(' then 
                    C := Op_Stack.Last_Element;
                    Op_Stack.Delete_Last;
                    Result.Append ((Class => Op, Op => C));
                end if;
            elsif C = ' ' then
                if Token /= "" then
                    -- last token was number, add it to result
                    -- also add token from op stack, if exists
                    Result.Append ((
                        Class => Number,
                        Value => Long_Integer'Value (To_String (Token))
                    ));
                    Token := Null_Unbounded_String;
                    if not Op_Stack.Is_Empty 
                        and then Op_Stack.Last_Element /= '(' then 
                        C := Op_Stack.Last_Element;
                        Op_Stack.Delete_Last;
                        Result.Append ((Class => Op, Op => C));
                    end if;
                end if;
            end if;
            I := I + 1;
            if I > Length (Expr) then
                if Token /= "" then
                    -- check last number at the end of string
                    Result.Append ((
                        Class => Number, 
                        Value => Long_Integer'Value (To_String (Token))
                    ));
                    Token := Null_Unbounded_String;
                end if;
                while not Op_Stack.Is_Empty loop
                    C := Op_Stack.Last_Element;
                    Op_Stack.Delete_Last;
                    Result.Append ((Class => Op, Op => C));
                end loop;
                exit;
            end if;
        end loop;
        return Result;
    end Parse_To_RPN;


    function Eval_RPN (Expr : Node_Vec.Vector) return Long_Integer is
        Data_Stack : Long_Int_Vec.Vector := Long_Int_Vec.Empty_Vector;
        Result     : Long_Integer := 0;
        Left       : Long_Integer;
        Right      : Long_Integer;
    begin
        for X of Expr loop
            case X.Class is
                when Number =>
                    Data_Stack.Append (X.Value);
                when Op =>
                    Left := Data_Stack (Data_Stack.Last_Index - 1);
                    Right := Data_Stack (Data_Stack.Last_Index);
                    Data_Stack.Delete_Last (2);

                    case X.Op is
                        when '+' => Data_Stack.Append (Left + Right); 
                        when '*' => Data_Stack.Append (Left * Right); 
                        when others =>
                            Put_Line ("Unexpected op: " & X.Op);
                    end case;
            end case;
        end loop;
        return Data_Stack (Data_Stack.Last_Index);
    end Eval_RPN;

    -- Main variables
    File   : File_Type;
    Line   : Unbounded_String;
    RPN    : Node_Vec.Vector;
    Value  : Long_Integer;
    Result : Long_Integer := 0;
begin
    Open (File, In_File, Ada.Command_Line.Argument (1));

    while not End_Of_File (File) loop
        Get_Line (File, Line);
        if Element (Line, 1) /= '#' then
            Put_Line (Line);
            RPN := Parse_To_RPN (Line);
            Put (" => "); Put (RPN); New_Line;
            Value := Eval_RPN (RPN);
            Put (" = "); Put (Value, 0);
            Result := Result + Value;
            New_Line;
        end if;
    end loop;

    Close (File);

    Put ("Part 1 result: "); 
    Put (Result, 0);
    New_Line;
end Day18;