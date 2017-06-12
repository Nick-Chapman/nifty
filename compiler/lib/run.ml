open Z_machine.Numbers
open Nifty_assembler_lib
open Compile

(* compile, assemble, and run *)
let start (uam: unit Action.t In_image.t) = 
  let zversion = Zversion.Z3 in
  let image0 = Compile.compile_and_assemble zversion uam in
  Zork_expect_test.Runner.create ~image0 ~tracing:false ()

let run (uam: unit Action.t In_image.t) = 
  let `enter _enter = start uam in
  ()

(* compile, assemble, and disassemble *)
let dis (uam: unit Action.t In_image.t) = 
  let story = Compile.compile uam in
  let zversion = Zversion.Z3 in
  let () = Assemble.assemble_and_disassemble zversion story in
  ()
 
(* compile, assemble, disassemble, and run *)
let rund (uam: unit Action.t In_image.t) = 
  let zversion = Zversion.Z3 in
  let story = Compile.compile uam in
  let () = Assemble.assemble_and_disassemble zversion story in
  let image0 = Compile.compile_and_assemble zversion uam in
  let `enter _enter = 
    Zork_expect_test.Runner.create ~image0 ~tracing:false () in
  ()
