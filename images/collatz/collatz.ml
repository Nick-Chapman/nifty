open Z_machine.Numbers
open Nifty_lib
open Compile
open Compile.Action
open Compile.Exp
let (>>=) = In_image.(>>=)

let pr s = print (string s) -$$ newline

let uam = 
  Global.int >>= fun v ->
  Global.int >>= fun steps ->
  In_image.return (
    print (string "starting collatz with value ")
    -$$ assign steps (int 0)
    -$$ assign v (int 7)
    -$$ print_num (read v)
    -$$ newline
    -$$ forever (
      print_num (read v)
      -$$ assign steps (add (read steps) (int 1))
      -$$ newline
      -$$ if_ (eqI (read v) (int 1)) (
	print (string "finished collatz in ")
	-$$ print_num (read steps)
	-$$ pr " steps"
	-$$ quit
      ) (
	if_ (eqI (mod_ (read v) (int 2)) (int 0)) (
	  assign v (div (read v) (int 2))
	) (
	  assign v (add (int 1) (mul (read v) (int 3)))
	)
      )
    )
  )

let story = Compile.compile uam
let zversion = Zversion.Z3
let () = Assemble.assemble_and_save zversion story
