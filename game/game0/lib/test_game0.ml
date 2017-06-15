open Nifty_compiler_lib.Run

module Game0 = Game0.F(struct let debug = true end)

let%expect_test "game zero" =
  let (`enter enter) = start Game0.game in
  enter ""; [%expect {|
    Welcome to game0.
    It's called game0 because that's how much fun it'll be.

    You will find 2 linked rooms, 3 single-word commands, and one exit.
    Have fun... (ah, I forgot, you won't)

    You are in room A (north to B)
    ?
    #words=0
    Say something please!

    You are in room A (north to B)
    ? |}];
  enter "some thing"; [%expect {|
    some thing
    #words=2
    show_item_text, offset=1, size=4, chars:<some>
    show_item_text, offset=6, size=5, chars:<thing>
    One word only please!

    You are in room A (north to B)
    ? |}];
  enter "something"; [%expect {|
    something
    #words=1
    show_item_text, offset=1, size=9, chars:<something>
    I don't understand the word: something

    You are in room A (north to B)
    ? |}];
  [%expect {| |}];
  enter "south"; [%expect {|
    south
    #words=1
    show_item_text, offset=1, size=5, chars:<south>
    You can't go that way!

    You are in room A (north to B)
    ? |}];
  enter "east"; [%expect {|
    east
    #words=1
    show_item_text, offset=1, size=4, chars:<east>
    You can't go that way!

    You are in room A (north to B)
    ? |}];
  enter "north"; [%expect {|
    north
    #words=1
    show_item_text, offset=1, size=5, chars:<north>

    You are in room B (south to A, east to leave)
    ? |}];
  enter "north"; [%expect {|
    north
    #words=1
    show_item_text, offset=1, size=5, chars:<north>
    You can't go that way!

    You are in room B (south to A, east to leave)
    ? |}];
  enter "south"; [%expect {|
    south
    #words=1
    show_item_text, offset=1, size=5, chars:<south>

    You are in room A (north to B)
    ? |}];
  enter "north"; [%expect {|
    north
    #words=1
    show_item_text, offset=1, size=5, chars:<north>

    You are in room B (south to A, east to leave)
    ? |}];
  enter "veryverybigword small third-word four"; [%expect {|
    veryverybigword small third-word four
    #words=4
    show_item_text, offset=1, size=15, chars:<veryverybigword>
    show_item_text, offset=17, size=5, chars:<small>
    show_item_text, offset=23, size=10, chars:<third-word>
    show_item_text, offset=34, size=4, chars:<four>
    One word only please!

    You are in room B (south to A, east to leave)
    ? |}];
  enter "east"; [%expect {|
    east
    #words=1
    show_item_text, offset=1, size=4, chars:<east>
    You have escaped!
    Bye |}];
  ()
