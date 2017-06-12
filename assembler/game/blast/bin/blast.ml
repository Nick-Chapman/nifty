open Z_machine.Numbers
open Nifty_assembler_lib
open Assemble
open Instr
open Var

let dict = Dict.empty
let objects = Objects.create []
let buffers = Buffers.create []
let text = Text.empty

let g5 = Global 5

let code = 
  Code.create [
    Print "Countdown"; Newline;
    
    Store (Target g5, Int 100);

  Label "count-loop";

    Print_num (Var g5); Print " ";
    Jump_eq (Var g5, Int 1, "end-of-count");
    Sub (Var g5, Int 1, g5);
    Jump (Lab "count-loop");

  Label "end-of-count";
  
    Newline; Print "Blast off!"; Newline;
    Quit;

  ]
		   
let story = Story.create dict objects buffers text code
let zversion = Zversion.Z3
let () = assemble_and_save zversion story
