with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line; 


procedure Day12 is
    type Dir_T is mod 360;

    function X_Coeff (Dir : Dir_T) return Integer is
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

    function Y_Coeff (Dir : Dir_T) return Integer is
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
    -- Ship's position
    -- East and north are positive, south and west - negative
    X         : Integer := 0;
    Y         : Integer := 0;
    Dir       : Dir_T := 0 ;
    Arg       : Natural;
    Result    : Natural;
begin
    Open (InputFile, In_File, Ada.Command_Line.Argument (1));
    while not End_Of_File (InputFile) loop
        Get_Line (InputFile, Line);
        Arg := Integer'Value (Slice (Line, 2, Length (Line)));
        case Element (Line, 1) is
            when 'N' =>
                Y := Y + Arg;
            when 'S' =>
                Y := Y - Arg;
            when 'E' =>
                X := X + Arg;
            when 'W' =>
                X := X - Arg;
            when 'L' =>
                Dir := Dir - Dir_T'Mod (Arg);
            when 'R' =>
                Dir := Dir + Dir_T'Mod (Arg);
            when 'F' =>
                X := X + X_Coeff (Dir) * Arg;
                Y := Y + Y_Coeff (Dir) * Arg;
            when others =>
                Put ("WARN: Unexpected command: ");
                Put (Line);
                New_Line;
        end case;
        Put (Line);
        Put (" => ");
        Put (X, 0);
        Put (", ");
        Put (Y, 0);
        New_Line;
    end loop;
    Close (InputFile);

    Put ("Part 1 Result: ");
    Put (Abs (X) + Abs (Y), 0);
    New_Line;


end Day12;
