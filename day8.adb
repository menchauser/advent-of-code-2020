with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Hash;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Indefinite_Vectors;


procedure Day8 is
    -- Representation of input line
    type Instruction is
        record
            Op  : String (1 .. 3);
            Arg : Integer;
        end record;

    function Parse_Instruction (Line : Unbounded_String) return Instruction is
    begin
        return (
            Op  => Slice (Line, 1, 3),
            Arg => Integer'Value (Slice (Line, 5, Length (Line)))
        );
    end Parse_Instruction;

    procedure Put (Item : Instruction) is
    begin
        Put (Item.Op);
        Put (' ');
        if Item.Arg >= 0 then
            Put ('+');
        end if;
        Put (Item.Arg, 0);
    end;

    -- These types will help us executing program and searching for loop
    package Program_Type is new Ada.Containers.Indefinite_Vectors (Natural, Instruction);
    use Program_Type;
    
    procedure Find_Loop_Acc (Program : Program_Type.Vector;
                             Accumulator : out Integer;
                             Terminated  : out Boolean) is
        type Executed_T is array (1 .. Natural (Length (Program))) of Boolean
            with Default_Component_Value => False;
        Executed    : Executed_T;
        I           : Instruction;
        Pointer     : Natural := 1;
    begin
        Accumulator := 0;
        Terminated := False;
        loop 
            I := Program(Pointer - 1); -- In vector indices start from 0
            exit when Executed (Pointer);
            Executed (Pointer) := True;
            if I.Op = "nop" then
                -- do nothing
                Pointer := Pointer + 1;
            elsif I.Op = "acc" then
                Accumulator := Accumulator + I.Arg;
                Pointer := Pointer + 1;
            elsif I.Op = "jmp" then
                Pointer := Pointer + I.Arg;
            else 
                raise Program_Error;
            end if;
            if Pointer > Natural (Length (Program)) then
                Terminated := True;
                exit;
            end if;
        end loop;
    end Find_Loop_Acc;

    -- In part 2 we could iterate over all commands and switch nop/jmp and try searching for acc 
    procedure Fix_Loop (Program : Program_Type.Vector;
                        Accumulator : out Integer) is
        Fixed_Program : Program_Type.Vector := Program;
        Terminated    : Boolean;
        Instr         : Instruction;
    begin
        Accumulator := 0;
        for I in Fixed_Program.First_Index .. Program.Last_Index loop
            Instr := Fixed_Program(I);
            if Instr.Op = "jmp" then
                Put ("Fix at line "); Put (I + 1); Put (':');
                Put (Instr);
                Put_Line (" to nop");
                Fixed_Program(I) := ("nop", Instr.Arg);
                Find_Loop_Acc (Fixed_Program, Accumulator, Terminated);
                if Terminated then 
                    Put_Line ("Fix worked!");
                    exit;
                else 
                    Put_Line ("Rollback");
                    Fixed_Program(I) := ("jmp", Instr.Arg);
                end if;
            end if;
        end loop;
    end Fix_Loop;

    -- Main variables
    InputFile   : File_Type;
    Line        : Unbounded_String := To_Unbounded_String("");
    CurInstr    : Instruction;
    Program     : Program_Type.Vector;
    Count       : Natural := 0;
    Result      : Integer;
    Terminated  : Boolean;
begin
    -- Read File
    Open (InputFile, In_File, "input8.txt");
    loop
        Line := Get_Line (InputFile);
        Count := Count + 1;
        CurInstr := Parse_Instruction (Line);
        Program.Append (CurInstr);
        exit when End_Of_File (InputFile);
    end loop;
    Close (InputFile);

    Put ("Lines:  ");
    Put (Count, 0);
    New_Line;
    
    -- Execute program and search for loop
    Find_Loop_Acc (Program, Result, Terminated);
    Put ("Result: ");
    Put (Result, 0);
    New_Line;
    Put ("Terminated: ");
    Put (Boolean'Image (Terminated));

    Fix_Loop (Program, Result);
    Put ("Fixed loop result: ");
    Put (Result, 0);
    New_Line;

end Day8;
