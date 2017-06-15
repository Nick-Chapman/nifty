open Nifty_compiler_lib
open Compile

module F(X : sig val debug : bool end) : sig
  val game : unit Action.t In_image.t
end
