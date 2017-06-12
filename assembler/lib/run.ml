open Z_machine.Numbers
open Assemble

let no_dict = Dict.empty
let no_objects = Objects.create []
let no_buffers = Buffers.create []

(* assemble and run *)    
let start ?(dict=no_dict) ?(objects=no_objects) ?(buffers=no_buffers) text code = 
  let story = Story.create dict objects buffers text code in
  let zversion = Zversion.Z3 in
  let image0 = assemble zversion story in
  Zork_expect_test.Runner.create ~image0 ~tracing:false ()

let run ?dict ?objects ?buffers text code = 
  let `enter _enter = start ?dict ?objects ?buffers text code in
  ()

(* assemble and disassemble *)    
let dis ?(dict=no_dict) ?(objects=no_objects) ?(buffers=no_buffers) text code =
  let story = Story.create dict objects buffers text code in
  let zversion = Zversion.Z3 in
  let () = assemble_and_disassemble zversion story in
  ()

(* assemble, disassemble and run *)    
let rund ?(dict=no_dict) ?(objects=no_objects) ?(buffers=no_buffers) text code =
  let story = Story.create dict objects buffers text code in
  let zversion = Zversion.Z3 in
  let () = assemble_and_disassemble zversion story in
  let image0 = assemble zversion story in
  let `enter _enter = 
    Zork_expect_test.Runner.create ~image0 ~tracing:false () in
  ()
