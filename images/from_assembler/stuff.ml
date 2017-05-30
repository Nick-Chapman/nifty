open Z_machine.Numbers
open Nifty_lib
open Assemble
open Instr

let dict = Dict.create ["north";"south";"inventory";"east"]
(*let dict = Dict.create []d *)
let objects = Objects.create () (* no objects yet *)
let global n = Var (Global n)
let target n = Target (Global n)

let text = Text.empty
let p1,text = Text.add text "Apples are nice"
let p2,text = Text.add text "Oranges are better"

let string t = String t

let sp = Var Sp
let target_sp = Target Sp

open! Arg

let code = 
  let open Instr in
  Code.create [
    Print "hello, world!";
    Newline;
    Store (target 5, string p1);
    Print_paddr (global 5);
    Newline;
    Store (target 5, string p2);
    Print_paddr (global 5);
    Newline;
    Print_num (Int 42);
    Newline;
    (* not sure if stack can be used in code before we make the first func-call
       fizmo & zorkmid bark.
       But frotz is happy. Assis niz.
    *)
    (*Store (target_sp, Int 991);
    Print_num (sp);
    Newline;*)
    
    (*Print "Prompt ";
    Sread(Int 40,Int 50);
    Newline;
    Print "Thanks";
    Newline;*)
    Quit;
  ]
    
		   
let story = Story.create dict objects text code
let zversion = Zversion.Z3
let () = assemble_and_save zversion story
