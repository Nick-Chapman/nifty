open Nifty_compiler_lib.Run

let%expect_test "game zero" =
  let (`enter enter) = start Game0.game in
  enter ""; [%expect {|
    Welcome
    You are in room A (north to B)
    ?
    Say something please!
    You are in room A (north to B)
    ? |}];
  enter "some thing"; [%expect {|
    some thing
    One word only please!
    You are in room A (north to B)
    ? |}];
  enter "something"; [%expect {|
    something
    I don't understand the word <..>
    You are in room A (north to B)
    ? |}];
  [%expect {| |}];
  enter "south"; [%expect {|
    south
    You can't go that way!, word=78
    You are in room A (north to B)
    ? |}];
  enter "east"; [%expect {|
    east
    You can't go that way!, word=71
    You are in room A (north to B)
    ? |}];
  enter "north"; [%expect {|
    north
    You are in room B (south to A, east to leave)
    ? |}];
  enter "north"; [%expect {|
    north
    You can't go that way!, word=85
    You are in room B (south to A, east to leave)
    ? |}];
  enter "south"; [%expect {|
    south
    You are in room A (north to B)
    ? |}];
  enter "north"; [%expect {|
    north
    You are in room B (south to A, east to leave)
    ? |}];
  enter "east"; [%expect {|
    east
    You have escaped!
    Bye |}];
  ()
