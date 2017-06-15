open Z_machine.Numbers
open Nifty_compiler_lib
open Nifty_assembler_lib

module Game0 = Game0_lib.Game0.F(struct let debug = false end)
let game = Game0.game
let story = Compile.compile game
let zversion = Zversion.Z3
let () = Assemble.assemble_and_save zversion story
