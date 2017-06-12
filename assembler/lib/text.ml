open Core
open Z_machine.Numbers

let (>>=) = Emit.(>>=)
let return = Emit.return

module Zchar : sig

  type t
  val encode : char -> t list
  val pack : t list -> unit Emit.t

end = struct

  type t = int

  let create_exn n =
    if not (n>=0 && n<=31) then (
      failwithf "Zchar.create_exn: %d" n ()
    );
    n

  let select_upper = create_exn 4 (* this is Z3 *)
  let select_punc = create_exn 5

  let code_a = Char.to_int 'a'
  let lower c = create_exn (6 + Char.to_int c - code_a)

  let code_A = Char.to_int 'A'
  let upper c = create_exn (6 + Char.to_int c - code_A)

  let punc = function
    | '.' -> create_exn (6 + 12)
    | ',' -> create_exn (6 + 13)
    | '!' -> create_exn (6 + 14)
    | '?' -> create_exn (6 + 15)
    | '_' -> create_exn (6 + 16)
    | '#' -> create_exn (6 + 17)
    | '\''-> create_exn (6 + 18)
    | '(' -> create_exn 30
    | ')' -> create_exn 31
    | '\n' -> create_exn 7
    | c -> failwithf "todo, punc: (%d) '%c'" (Char.to_int c) c ()

  let space = create_exn 0

  let ascii c =
    let n = Char.to_int c in
    let a,b = n/32, n%32 in
    [select_punc ; create_exn 6; create_exn a; create_exn b]

  let encode : char -> t list = 
    function
    | ' ' -> [space]
    | ('a'..'z') as c -> [lower c]
    | ('A'..'Z') as c -> [select_upper; upper c]
    | ('<' | '>' | '=') as c -> ascii c
    | c -> 
      [select_punc; punc c]
  (*failwithf "Zchar.encode: '%c'" c ()*)

  let pack =
    let pack3 (a,b,c) ~continue : Byte.t list =
      let int = 
	((if continue then 0 else 1) lsl 15) 
	lor (a lsl 10) lor (b lsl 5) lor c
      in
      let word = Word.of_int_exn int in
      let high,low = Word.to_high_low word in
      [high;low]
    in    
    let fill = select_punc in
    let rec loop = function
      | [] -> failwith "impossible"
      | [a] -> pack3 (a,fill,fill) ~continue:false
      | [a;b] -> pack3 (a,b,fill) ~continue:false
      | [a;b;c] -> pack3 (a,b,c) ~continue:false
      | a::b::c::more -> pack3 (a,b,c) ~continue:true @ loop more
    in
    function
    | [] -> failwith "pack empty list of z char"
    | zs -> Emit.bytes (loop zs)

end

module String = struct

  let encode s = 
    assert (s <>"");
    let zs = List.concat_map (String.to_list s) ~f:Zchar.encode in
    Zchar.pack zs

end

module T = struct
  type t = Id of int [@@deriving sexp,compare]
end
include T
include Comparable.Make(T)

type id = t [@@deriving sexp_of]
type store = { next: int; entries: (t * string) list }
[@@deriving sexp_of]

let empty = { next = 1; entries = [] }
let add {next;entries} string = 
  let id = Id next in
  id, { next = next+1; entries = (id, string) :: entries }

type env = { map : Emit.mark Map.t; }
let lookup e text = Map.find_exn e.map text

let encode store =
  let rec loop acc = function
    | [] -> return { map = Map.of_alist_exn acc }
    | (id,string)::entries ->
	Emit.here >>= fun loc ->
	String.encode string >>= fun () ->
	loop ((id,loc)::acc) entries
  in
  loop [] store.entries
