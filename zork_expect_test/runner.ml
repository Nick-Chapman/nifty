open Core
open Z_machine

let game_print string = 
  Printf.printf "%s%!" string

let runner_print string =
  Printf.printf "%s%!" string

let runner_printn string =
  Printf.printf "%s\n%!" string

let create ~image0 ~tracing () = 
  let save _ = false in
  let restore () = None in
  let trace = 
    if tracing then 
      fun step -> 
	(*Tracing.display ~print:runner_printn step*)
	runner_print 
	  (sprintf !"%{sexp:Numbers.Loc.t} " 
	     (Tracing.program_counter step))

    else
      fun _ -> ()
  in
  let xs = ref [] in
  let output x = xs := x::(!xs) in
  let flush () = 
    (List.iter (List.rev (!xs)) ~f:game_print; xs := []) 
  in

  let module Eval = Eval.F(struct 
    let image0 = image0 
    let hide_unimplemented = false
  end) in

  let callbacks = { Eval. output; trace; save; restore;} in

  let e = ref (Eval.init callbacks) in

  let getE() = Option.value_exn (!e) in

  let objects () = 
    if tracing then (
      runner_print "\n";
      Eval.display_object_tree ~print:runner_printn (getE());
    )
  in
  objects();
  flush();
  let enter reply = 
    runner_printn reply;
    e := Eval.command (getE()) ~reply callbacks;
    objects ();
    flush();
  in
  `enter enter
