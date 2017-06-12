open Z_machine.Numbers
open Nifty_compiler_lib
open Nifty_assembler_lib

let game = Game0_lib.Game0.game
let story = Compile.compile game
let zversion = Zversion.Z3
let () = Assemble.assemble_and_save zversion story
