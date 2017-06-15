open Core
open Z_machine.Numbers

let (>>=) = Emit.(>>=)
let return = Emit.return

let emit_fill_pattern byte n = 
  Emit.bytes (List.init n ~f:(fun _ -> byte))

module Id = struct
  module T = struct
    type t = Id of int [@@deriving sexp,compare]
  end
  include T
  include Comparable.Make(T)
end

type word = Id.t [@@deriving sexp_of]
type t = { next: int; entries: (word * string) list } [@@deriving sexp_of]

let empty = { next = 1; entries = [] }
let add {next;entries} string = 
  let id = Id.Id next in
  id, { next = next+1; entries = (id, string) :: entries }

type env = { map : Emit.mark Id.Map.t; }
let lookup e text = Id.Map.find_exn e.map text

let text_chars = 6
let text_bytes = 4
  
let entry_extra_data_length = 3 (* odd a problem ? *)
let entry_length = text_bytes + entry_extra_data_length

let encode_header ~num_entries =
  Emit.seq  [

    Emit.byte (Byte.of_int_exn 3);
    Emit.byte (Byte.of_char ',');
    Emit.byte (Byte.of_char '.');
    Emit.byte (Byte.of_char '"');

    Emit.byte (Byte.of_int_exn entry_length);
    Emit.word (Word.of_int_exn num_entries);
  ]

let _b44 = Byte.of_int_exn 0x44 (* 44 so visible in Z dump *)

let encode_entry word =
  let prefix = String.prefix word text_chars in
  let emit =
    Emit.assert_size 
      ~tag:(sprintf "dict_entry:%s" word) text_bytes 
      (Text.String.encode prefix)
  in
  Emit.seq [
    emit;
    emit_fill_pattern Byte.zero entry_extra_data_length;
  ]

let encode t =
  let rec loop acc = function
    | [] -> return { map = Id.Map.of_alist_exn acc }
    | (id,string)::entries ->
	Emit.here >>= fun loc ->
	encode_entry string >>= fun () ->
	loop ((id,loc)::acc) entries
  in
  let num_entries = List.length t.entries in
  encode_header ~num_entries >>= fun () ->
  let byname (_,s1) (_,s2) = String.compare s1 s2 in
  loop [] (List.sort ~cmp:byname t.entries)
