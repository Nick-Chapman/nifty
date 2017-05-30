
type store [@@deriving sexp_of]
type t [@@deriving sexp_of]
type env

val empty : store
val add : store -> string -> t * store

module String : sig
  val encode : string -> unit Emit.t
end

val encode : store -> env Emit.t

val lookup : env -> t -> Emit.mark
