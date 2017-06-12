
module type S = sig
    
  module In_image : sig
    type 'a t
    val return : 'a -> 'a t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module Lv : sig
    type 'a t
    val bool : bool t In_image.t
  end

  module Exp : sig
    type 'a t
    val read : 'a Lv.t -> 'a t
    val bool : bool -> bool t
    val int : int -> int t
    val string : string -> string t
    val not : bool t -> bool t
    val leqI : int t -> int t -> bool t
    val eqI : int t -> int t -> bool t
    val ite : bool t -> 'a t -> 'a t -> 'a t
  end

  module Action : sig
    type 'a t
    val return : 'a Exp.t -> 'a t
    val (-->) : 'a t -> ('a Exp.t -> 'b t) -> 'b t
    val (-$$) : unit t -> 'a t -> 'a t
    val if_ : bool Exp.t -> 'a t -> 'a t -> 'a t
    val let_ : 'a Exp.t -> ('a Exp.t -> 'b t) -> 'b t
    val nothing : unit t
    val newline : unit t
    val print : string Exp.t -> unit t
    val assign : 'a Lv.t -> 'a Exp.t -> unit t
    val forever : unit t -> unit t
    val quit : unit t
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

  module Lex_buf : sig
    type t
    val create : size:int -> t In_image.t
    val num_entries : t -> int Exp.t
    val get : t -> int Exp.t -> Word.t Exp.t
  end

  module Parse : sig
    val sread : Lex_buf.t -> Text_buf.t -> unit Action.t
  end

  val compile : unit Action.t In_image.t -> unit

end
