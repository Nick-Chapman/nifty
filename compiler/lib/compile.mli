open Import
open Z.Numbers

module In_image : sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

type num

module Lv : sig
  type 'a t
end

module Global : sig
  val string : string Lv.t In_image.t
  val int : num Lv.t In_image.t
  val bool : bool Lv.t In_image.t
end

module Exp : sig
  type 'a t
  val int : int -> num t
  val string : string -> string t
  val read : 'a Lv.t -> 'a t
  val add : num t -> num t -> num t
  val sub : num t -> num t -> num t
  val mul : num t -> num t -> num t
  val div : num t -> num t -> num t
  val mod_: num t -> num t -> num t

  val eqI : num t -> num t -> bool t

  val leqI : num t -> num t -> bool t
  val not : bool t -> bool t
  val bool : bool -> bool t
  val ite : bool t -> 'a t -> 'a t -> 'a t

  val castInt : 'a t -> num t

end

module Action : sig
  type 'a t
  val quit : unit t
  val newline : unit t
  val print : string Exp.t -> unit t
  val print_num : num Exp.t -> unit t
  val assign : 'a Lv.t -> 'a Exp.t -> unit t
  val (-$$) : unit t -> 'a t -> 'a t
  val if_ : bool Exp.t -> unit t -> unit t -> unit t (* 'a ? *)
  val forever : unit t -> unit t

  val let_ : 'a Exp.t -> ('a Exp.t -> 'b t) -> 'b t

end

module Word : sig
  type t
  val add : string -> t In_image.t
  val is_null : t Exp.t -> bool Exp.t
  val eq : t Exp.t -> t -> bool Exp.t
end

module Text_buf : sig
  type t
  val create : size:int -> t In_image.t
end

module Parse_buf : sig
  type t
  val create : size:int -> t In_image.t
  val num_entries : t -> num Exp.t
  val get : t -> num Exp.t -> Word.t Exp.t
end

module Parse : sig
  val sread : Text_buf.t -> Parse_buf.t -> unit Action.t
end


val compile : unit Action.t In_image.t -> Assemble.Story.t

val compile_and_assemble : Zversion.t -> unit Action.t In_image.t -> Z.Mem.t
