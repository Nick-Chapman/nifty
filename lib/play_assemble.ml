open! Core

(* WORK IN PROGRESS *)

let (>>=) = Emit.(>>=)
let return = Emit.return 

module Lab : sig (* assembler branch/jump targets *)
  type t
  include Comparable with type t := t
  val zero : t
  val inc : t -> t
end = struct
  module T = struct
    type t = int [@@deriving sexp, compare]
  end
  include T
  include Comparable.Make(T)
  let zero = 0
  let inc x = x+1
end
type lab = Lab.t


type plain_i =
| Add

type cond_i =
| Leq

type i = 
| Plain of plain_i
| Label of lab 
| Cond of cond_i * lab
| Jump_back of lab

type code = Code of lab list * i list
    
let emit_plain : plain_i -> unit Emit.t = failwith ""
let emit_cond_forward : cond_i -> int -> unit Emit.t = failwith ""
let emit_jump : Emit.mark -> unit Emit.t = failwith ""

type env = Emit.mark Lab.Map.t

type offsets = int Lab.Map.t

let increase_offsets : offsets -> int -> offsets =
  fun m n -> Lab.Map.map m ~f:(fun x -> (x+n))

let rec emit_is : env -> i list -> offsets Emit.t = 
  fun env -> function
  | [] -> Emit.return Lab.Map.empty
  | i::is ->
    match i with

    | Label lab ->
      Emit.here >>= fun mark ->
      let env = Lab.Map.add env ~key:lab ~data:mark in
      emit_is env is >>= fun offsets ->
      let offsets = Lab.Map.add offsets ~key:lab ~data:0 in
      return offsets

    | Jump_back lab -> 
      let mark = Lab.Map.find_exn env lab in
      Emit.size (emit_jump mark) >>= fun ((),n) ->
      emit_is env is >>= fun offsets ->
      let offsets = increase_offsets offsets n in
      return offsets

    | Plain p -> 
      Emit.size (emit_plain p) >>= fun ((),n) ->
      emit_is env is >>= fun offsets ->
      let offsets = increase_offsets offsets n in
      return offsets
      
    | Cond (cond_i,lab) ->
      Emit.reverse_bind
	~first_f:(fun ~backwards_flowing_info:offsets ->
	  let offset = Lab.Map.find_exn offsets lab in
	  Emit.size (emit_cond_forward cond_i offset) >>= fun ((),n) ->
	  let offsets = increase_offsets offsets n in
	  return offsets)
	~second:(emit_is env is)
