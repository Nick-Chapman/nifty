open Core
open Z_machine.Numbers
open Nifty_assembler_lib
open Assemble
open Instr
open Arg

let dict =
  List.fold ~f:(fun s x -> snd (Dict.add s x)) ~init:Dict.empty
    ["north";"south";"inventory";"east"]

let objects = Objects.create [
  Objects.Ob.create ~short_name:"my first object";
  Objects.Ob.create ~short_name:"another object";
]

let buffers = Buffers.create [
  ("pbuf", 100);
  ("tbuf", 100);
]
let tbuf = Buffer "tbuf"
let pbuf = Buffer "pbuf"

let text = Text.empty
let _p1,text = Text.add text "Apples are nice"
let _p2,text = Text.add text "Oranges are better"
let not_in_dict,text = Text.add text "Not in dictionary"

let g0 = Var.Global 0
let g1 = Var.Global 1
let g2 = Var.Global 2
let g3 = Var.Global 3

let code = 
  Code.create [

(*    Print "hello, world!";
    Newline;*)
(*
    Store (target 5, string p1);
    Print_paddr (global 5);
    Newline;
    Store (target 5, string p2);
    Print_paddr (global 5);
    Newline;
    Print_num (Int 42);
    Newline;
*)

    (* not sure if stack can be used in code before we make the first func-call
       fizmo & zorkmid bark.
       But frotz is happy. Assis niz.
    *)
    (*Store (target_sp, Int 991);
    Print_num (sp);
    Newline;*)

(*
    Jump_eq (Int 100, Int 100, l1);
    Print "Maybe see this"; Newline;
    Label l1;
    Print "But definitely see this"; Newline;
*)

    Print "This story has two objects!";
    Newline;
    Print_obj (Int 1); Newline;
    Print_obj (Int 2); Newline;
    Store (Target g0, Int 1);
    
    Print "Prompt! "; Sread(tbuf,pbuf);
    Print "Thanks"; Newline;

    (* g1 - temp
       g3 - total number of words to print
       g2 - current word# being printed (0.. total-1)
    *)
    
    Print "Number of words typed, ";
    Load_word (pbuf, Int 0, g3); (* this should be load_byte from index 1 *)
    Print_num (Var g3); Newline;

    Store (Target g2, Int 0);

    Label ("999");
    Jump_neq (Var g2, Var g3, "1000");
    Print "Done";
    Quit;
    Label ("1000");

    Print "Word ";
    Print_num (Var g2);
    Print ", ";
    Mul (Var g2, Int 2, Sp);
    Add (Var Sp, Int 1, Sp);
    Load_word (pbuf, Var Sp, Sp);
    Div (Var Sp, Int 2, g1);
    Jump_neq (Var g1, Int 0, "1001");
    Store (Target g1, (String not_in_dict));
    Label ("1001");
    Print_paddr (Var g1); 
    Newline;

    Add (Var g2, Int 1, g2);
    Jump (Lab "999");

  ]
		   
let story = Story.create dict objects buffers text code
let zversion = Zversion.Z3
let () = assemble_and_save zversion story
