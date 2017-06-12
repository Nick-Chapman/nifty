open Nifty_compiler_lib

open Compile
let (>>=) = In_image.(>>=)
open Exp
open Action
let print_string s = print (string s)

let game =

  Word.add "north" >>= fun north ->
  Word.add "south" >>= fun south ->
  Word.add "east" >>= fun east ->

  Text_buf.create ~size:100 >>= fun text_buf ->
  Parse_buf.create ~size:100 >>= fun parse_buf ->

  let parse = Parse.sread text_buf parse_buf in

  let understand_one_word_with f =
    let_ (Parse_buf.num_entries parse_buf) (fun n ->
      if_ (eqI n (int 0))
	(print_string "Say something please!\n")
	(if_ (not (leqI n (int 1)))
	   (print_string "One word only please!\n")
	   (let_ (Parse_buf.get parse_buf (int 0)) (fun word ->
	     (if_ (Word.is_null word)
		(print_string "I don't understand the word <..>\n")
		(f word))))))
  in

  let welcome = print (string "Welcome") -$$ newline in

  let say_bye = print (string "Bye") -$$ newline in

  let cant_go_that_way word = 
    print (string "You can't go that way!")
    -$$ print (string ", word=") -$$ print_num (castInt word)
    -$$ newline
  in

  let end_the_game = 
    print (string "You have escaped!\n")
    -$$ say_bye -$$ quit
  in

  Global.bool >>= fun where -> (* true=A, false=B *)

  let move_to_a = assign where (bool true) in
  let move_to_b = assign where (bool false) in
  
  let when_inA word =
    if_ (Word.eq word north) move_to_b 
      (cant_go_that_way word)
  in

  let when_inB word =
    if_ (Word.eq word south) move_to_a
      (if_ (Word.eq word east) end_the_game
	 (cant_go_that_way word))
  in

  let read_user_input_and_action =
    parse -$$ understand_one_word_with (fun w ->
      if_ (read where) 
	(when_inA w) 
	(when_inB
	   w))
  in
  
  let roomA_desc = string "You are in room A (north to B)" in
  let roomB_desc = 
    string "You are in room B (south to A, east to leave)" in

  let where_desc =
    ite (read where) roomA_desc roomB_desc 
  in

  let print_where = 
    print where_desc
    -$$ newline
  in

  let main_loop =
    welcome 
    -$$ move_to_a
    -$$ forever (
      print_where
      -$$ print_string "? "
      -$$ read_user_input_and_action
    )
  in

  In_image.return main_loop
