
type t [@@deriving sexp_of] (*store*)
type word [@@deriving sexp_of] (*id*)
type env

val empty : t
val add : t -> string -> word * t

val encode : t -> env Emit.t

val lookup : env -> word -> Emit.mark
