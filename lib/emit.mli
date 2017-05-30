open! Core.Std
open Z_machine.Numbers

type 'a t
type mark

val exec : 'a t -> start:Loc.t -> 'a * Byte.t list * (mark -> Loc.t)

val here : mark t
val packed_address : Zversion.t -> mark -> unit t
val loc : mark -> unit t
val align2 : unit t

val return : 'a -> 'a t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val reverse_bind : 
  first_f: (backwards_flowing_info:'b -> 'a t) -> 
  second : 'b t -> 
  'a t

val size : 'a t -> ('a * int) t
val assert_size : ?tag:string -> int -> 'a t -> 'a t

val nothing : unit t
val byte : Byte.t -> unit t
val bytes : Byte.t list -> unit t
val word : Word.t -> unit t
val seq : unit t list -> unit t