
module Z = Z_machine
open Z.Numbers

module In_image : sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module Lv : sig
  type 'a t
  (*val bool : bool t In_image.t*)
end

module Global : sig
  val string : string Lv.t In_image.t
  val int : int Lv.t In_image.t
end

module Exp : sig
  type 'a t
  val int : int -> int t
  val string : string -> string t
  val read : 'a Lv.t -> 'a t
  val add : int t -> int t -> int t
  val sub : int t -> int t -> int t
  val mul : int t -> int t -> int t
end

module Action : sig
  type 'a t
  val newline : unit t
  val print : string Exp.t -> unit t
  val print_num : int Exp.t -> unit t
  val assign : 'a Lv.t -> 'a Exp.t -> unit t
  val (-$$) : unit t -> 'a t -> 'a t
end

val compile : unit Action.t In_image.t -> Assemble.Story.t

val compile_and_assemble : Zversion.t -> unit Action.t In_image.t -> Z.Mem.t
