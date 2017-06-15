open Nifty_compiler_lib

open Compile
let (>>=) = In_image.(>>=)
open Exp
open Action
let print_string s = print (string s)

type 'a lv = 'a Lv.t
type 'a exp = 'a Exp.t
type 'a action = 'a Action.t

module F(X : sig val debug : bool end) = struct

  let _ = X.debug

  let game =

    Word.add "north" >>= fun north ->
    Word.add "south" >>= fun south ->
    Word.add "east" >>= fun east ->

    Text_buf.create ~size:100 >>= fun text_buf ->
    Parse_buf.create ~size:100 >>= fun parse_buf ->

    let parse = Parse.sread text_buf parse_buf in

    let show_item_text_char offset i =
      let_ (Text_buf.get_char text_buf (add offset i)) (fun c ->
	print_char c
      )
    in

    let foreach_int_upto (n:num exp) (i:num lv) ~(f:num exp -> unit action) =
      assign i (int 0)
      -$$ while_ (lessI (read i) n) (
	f (read i)
	-$$ assign i (add (read i) (int 1))
      )
    in
    
    Global.int >>= fun index_chars ->

    let show_item_text offset size =
      print_string "show_item_text, offset="
      -$$ print_num offset
      -$$ print_string ", size="
      -$$ print_num size
      -$$ print_string ", chars:<"
      -$$ foreach_int_upto size index_chars ~f:(show_item_text_char offset)
      -$$ print_string ">\n"
    in
    
    let show_user_item e =
      let_ (Parse_buf.get_word_size parse_buf e) (fun size ->
	let_ (Parse_buf.get_text_offset parse_buf e) (fun offset ->
	  show_item_text offset size))
    in

    Global.int >>= fun index_words ->

    let show_what_was_typed n =
      print_string "#words=" -$$ print_num n -$$ newline
      -$$ foreach_int_upto n index_words ~f:show_user_item
    in

    let show_entered_word e =
      let_ (Parse_buf.get_word_size parse_buf e) (fun size ->
	let_ (Parse_buf.get_text_offset parse_buf e) (fun offset ->
	  foreach_int_upto size index_chars ~f:(show_item_text_char offset)))
    in

    let understand_one_word_with f =
      let_ (Parse_buf.num_entries parse_buf) (fun n ->
	let add_debug_maybe a =
	  if X.debug 
	  then show_what_was_typed n -$$ a 
	  else a
	in
	add_debug_maybe (
	  if_ (eqI n (int 0))
	    (print_string "Say something please!\n")
	    (if_ (not (leqI n (int 1)))
	       (print_string "One word only please!\n")
	       (let_ (Parse_buf.get parse_buf (int 0)) (fun word ->
		 (if_ (Word.is_null word)
		    (print_string "I don't understand the word: "
		     -$$ show_entered_word (int 0)
		     -$$ newline)
		    (f word)))))))
    in

    let welcome = 
      print (string "Welcome to game0.\n\
It's called game0 because that's how much fun it'll be.\n\

You will find 2 linked rooms, 3 single-word commands, and one exit.\n\
Have fun... (ah, I forgot, you won't)\n\
")
    in
    let say_bye = print (string "Bye") -$$ newline in

    let cant_go_that_way _word = 
      print (string "You can't go that way!")
      (*    -$$ print (string ", word=") -$$ print_num (castInt word)*)
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
      newline
      -$$ print where_desc
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

end
  
