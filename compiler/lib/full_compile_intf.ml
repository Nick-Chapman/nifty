
module type S = sig
    
  module In_image : sig
    type 'a t
    val return : 'a -> 'a t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module Lv : sig
    type 'a t
    val unit : unit t In_image.t
    val bool : bool t In_image.t
    val char : char t In_image.t
    val int : int t In_image.t
    val string : string t In_image.t
    val pair : 'a t * 'b t -> ('a * 'b) t
  end

  module Exp : sig
    type 'a t
    val read : 'a Lv.t -> 'a t
    val unit : unit t
    val bool : bool -> bool t
    val char : char -> char t
    val int : int -> int t
    val string : string -> string t
    val pair : 'a t * 'b t -> ('a * 'b) t
    val case_pair : ('a * 'b) t -> ('a t -> 'b t -> 'c t) -> 'c t

    val not : bool t -> bool t

    val leqI : int t -> int t -> bool t
    val eqI : int t -> int t -> bool t

    val if_ : bool t -> 'a t -> 'a t -> 'a t
    val let_ : 'a t -> ('a t -> 'b t) -> 'b t
  end
  type 'a exp = 'a Exp.t

  module Action : sig
    type 'a t
    val return : 'a exp -> 'a t
    val (-->) : 'a t -> ('a exp -> 'b t) -> 'b t
    val (-$$) : unit t -> 'a t -> 'a t
    val if_ : bool exp -> 'a t -> 'a t -> 'a t
    val case_pair : ('a * 'b) exp -> ('a exp -> 'b exp -> 'c t) -> 'c t
    val nothing : unit t
    val newline : unit t
    val print : string Exp.t -> unit t
    val print_char : char Exp.t -> unit t
    val assign : 'a Lv.t -> 'a exp -> unit t
    val let_ : 'a Exp.t -> ('a Exp.t -> 'b t) -> 'b t

    val forever : unit t -> unit t
    val quit : unit t

    (*val forever : unit t -> never_returns t
    val quit : never_returns t
    val never_returns : never_returns t -> unit t*)

  end

  module Word : sig
    type t
    val add : string -> t In_image.t
    val is_null : t exp -> bool exp
    val eq : t exp -> t -> bool exp
  end

  module Text_buf : sig
    type t
    val create : size:int -> t In_image.t
    val get : t (*exp*) -> int exp -> char exp
    val set : t -> int exp -> unit Action.t
  end

  module Lex_buf : sig
    type t
    val create : size:int -> t In_image.t
    val num_entries : t -> int exp
    val get : t -> int exp -> (Word.t * int(* * int*)) exp
  end

  module Parse : sig
    val sread : Lex_buf.t -> Text_buf.t -> unit Action.t
  end

  module Func : sig
    type ('a,'b) t
    val define : ('a exp -> 'b Action.t) -> ('a,'b) t In_image.t
    val call : ('a,'b) t -> 'a exp -> 'b exp
  end

end
