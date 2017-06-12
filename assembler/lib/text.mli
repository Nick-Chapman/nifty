
type store [@@deriving sexp_of]
type id [@@deriving sexp_of]
type env

val empty : store
val add : store -> string -> id * store

module String : sig
  val encode : string -> unit Emit.t
end

val encode : store -> env Emit.t

val lookup : env -> id -> Emit.mark
