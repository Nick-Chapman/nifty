
module F(X : Compile_intf.S) : sig

  open X
  val game : unit Action.t In_image.t

end = struct
  open X

  let (>>=) = In_image.(>>=)
  open Exp
  open Action
  let print_string s = print (string s)

  let game =

    Word.add "north" >>= fun north ->
    Word.add "south" >>= fun south ->
    Word.add "east" >>= fun east ->

    Text_buf.create ~size:100 >>= fun text_buf ->
    Lex_buf.create ~size:100 >>= fun lex_buf ->

    let parse = Parse.sread lex_buf text_buf in

    let understand_one_word_with f =
      let_ (Lex_buf.num_entries lex_buf) (fun n ->
	if_ (eqI n (int 0))
	  (print_string "say something please!\n")
	  (if_ (not (leqI n (int 1)))
	     (print_string "one word only please!\n")
	     (let_ (Lex_buf.get lex_buf (int 0)) (fun word ->
	       (if_ (Word.is_null word)
		  (print_string "dont understand the word <..>\n")
		  (f word))))))
    in

    let welcome = print (string "Welcome") -$$ newline in

    let say_bye = print (string "Bye") -$$ newline in

    let cant_go_that_way = 
      print (string "You can't go that way!")
      -$$ newline
    in

    let end_the_game = 
      print (string "You have escaped!")
      -$$ say_bye -$$ quit
    in

    Lv.bool >>= fun where ->

    let move_to_a = assign where (bool false) in
    let move_to_b = assign where (bool true) in
    
    let when_inA word =
      if_ (Word.eq word north) move_to_b 
	cant_go_that_way
    in

    let when_inB word =
      if_ (Word.eq word south) move_to_a
	(if_ (Word.eq word east) end_the_game
	   cant_go_that_way)
    in

    let read_user_input_and_action =
      parse -$$ understand_one_word_with (fun w ->
	if_ (read where) 
	  (when_inA w) 
	  (when_inB w))
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
      -$$ forever (
	print_where
	-$$ read_user_input_and_action
      )
    in

    In_image.return main_loop

end
