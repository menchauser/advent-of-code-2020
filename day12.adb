with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line; 


procedure Day12 is
    type Dir_T is mod 360;
    type Action_T is (N, S, E, W, L, R, F);
    type Instruction is
        record
            Action : Action_T;
            Value  : Natural;
        end record;

    procedure Put (X : Action_T) is
    begin
        Put (Action_T'Image (X));
    end;

    function Parse (X : Unbounded_String) return Instruction is
    begin
        return (
            Action => Action_T'Value ("" & Element (X, 1)),
            Value  => Natural'Value (Slice (X, 2, Length (X)))
        );
    end;

    -- Restricted sine and cosine functions for right-clockwise rotations
    function My_Cos (Dir : Dir_T) return Integer is
    begin
        case Dir is
            when 0 => 
                return 1;
            when 180 =>
                return -1;
            when others =>
                return 0;
        end case;
    end;

    function My_Sin (Dir : Dir_T) return Integer is
    begin
        case Dir is
            when 90 => 
                return -1;
            when 270 =>
                return 1;
            when others =>
                return 0;
        end case;
    end;

    -- Main variables
    InputFile : File_Type;
    Line      : Unbounded_String;
    I         : Instruction;

    -- Part 1 coordinates
    -- Ship's position
    -- East and north are positive, south and west - negative
    X         : Integer := 0;
    Y         : Integer := 0;
    Dir       : Dir_T := 0;
    -- Part 2 coordinates
    -- ship coords
    S_X       : Integer := 0;
    S_Y       : Integer := 0;
    -- waypoint coords
    W_X       : Integer := 10;
    W_Y       : Integer := 1;
    W_Dir     : Dir_T;
    Tmp1      : Integer;
    Tmp2      : Integer;
begin
    -- Part 1 and Part 2 are calculated simultaneously
    Open (InputFile, In_File, Ada.Command_Line.Argument (1));

    while not End_Of_File (InputFile) loop
        I := Parse (Get_Line (InputFile));
        case I.Action is
            when N =>
                Y := Y + I.Value;
                W_Y := W_Y + I.Value;
            when S =>
                Y := Y - I.Value;
                W_Y := W_Y - I.Value;
            when E =>
                X := X + I.Value;
                W_X := W_X + I.Value;
            when W =>
                X := X - I.Value;
                W_X := W_X - I.Value;
            when L =>
                Dir := Dir - Dir_T'Mod (I.Value);
                -- rotate WP left counter-clockwise 
                W_Dir := Dir_T'Mod (I.Value);
                Tmp1 := W_X;
                Tmp2 := W_Y;
                W_X := Tmp1 * My_Cos (W_Dir) + Tmp2 * My_Sin (W_Dir);
                W_Y := - (Tmp1) * My_Sin (W_Dir) + Tmp2 * My_Cos (W_Dir);
            when R =>
                Dir := Dir + Dir_T'Mod (I.Value);
                -- rotate WP right clockwise 
                W_Dir := Dir_T'Mod (I.Value);
                Tmp1 := W_X;
                Tmp2 := W_Y;
                W_X := Tmp1 * My_Cos (W_Dir) - Tmp2 * My_Sin (W_Dir);
                W_Y := Tmp2 * My_Cos (W_Dir) + Tmp1 * My_Sin (W_Dir);
            when F =>
                X := X + My_Cos (Dir) * I.Value;
                Y := Y + My_Sin (Dir) * I.Value;
                S_X := S_X + W_X * I.Value;
                S_Y := S_Y + W_Y * I.Value;
        end case;
        Put (I.Action);
        Put (I.Value, 0);
        Put (" => ");
        Put (S_X, 0); Put (", "); Put (S_Y, 0);
        Put (" (");
        Put (W_X, 0);
        Put (", ");
        Put (W_Y, 0);
        Put (")");
        New_Line;
    end loop;
    Close (InputFile);

    Put ("Part 1 Result: ");
    Put (Abs (X) + Abs (Y), 0);
    New_Line;

    Put ("Part 2 Result: ");
    Put (Abs (S_X) + Abs (S_Y), 0);
    New_Line;


end Day12;
