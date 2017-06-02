open Core
open Z_machine.Numbers
module Value = Z_machine.Value

let (>>=) = Emit.(>>=)
let return = Emit.return

let emit_fill_pattern byte n = 
  Emit.bytes (List.init n ~f:(fun _ -> byte))

let b77 = Byte.of_int_exn 0x77
let _ = b77


module Dict : sig

  type t [@@deriving sexp_of]
  val create : string list -> t

  val encode : t -> unit Emit.t

end = struct
    
  type t = string list [@@deriving sexp_of]
  let create xs = xs

  let text_chars = 6
  let text_bytes = 4
      
  let entry_extra_data_length = 2 (* odd a problem ? *)
  let entry_length = text_bytes + entry_extra_data_length

  let b44 = Byte.of_int_exn 0x44

  let encode_entry word =
    let prefix = String.prefix word text_chars in
    let emit =
      Emit.assert_size 
	~tag:(sprintf "dict_entry:%s" word) text_bytes 
	(Text.String.encode prefix)
    in
    Emit.seq [
      emit;
      emit_fill_pattern b44 entry_extra_data_length;
    ]
    
  let encode xs = 
    let num_entries = List.length xs in
    Emit.seq [
      Emit.byte Byte.zero; (* number of word seps = 0 *)
      (* list of 0 word seps here *)
      Emit.byte (Byte.of_int_exn entry_length);
      Emit.word (Word.of_int_exn num_entries);
      Emit.seq (List.map xs ~f:encode_entry);
    ]

end

module Objects : sig

  module Ob : sig
    type t
    val create : short_name:string -> t
  end

  type t [@@deriving sexp_of]
  val create : Ob.t list -> t
  val encode : t -> unit Emit.t

end = struct

  module Ob = struct

    type t = {
      short_name : string
    } [@@deriving sexp_of]

    let create ~short_name = { short_name }
      
    let emit_size_prefixed_string string =
      Emit.reverse_bind
	~first_f:(fun ~backwards_flowing_info:size ->
	  assert(size%2 = 0);
	  let half_size = size/2 in
	  Emit.byte (Byte.of_int_exn half_size)
	)
	~second:(
	  Emit.size (Text.String.encode string) >>= fun ((),size) ->
	  return size
	)

    let emit_properties = (* none for now *)
      Emit.byte Byte.zero

    let emit_prop_table t = 
      Emit.seq [
	emit_size_prefixed_string t.short_name;
	emit_properties;
      ]

    let emit_attributes = emit_fill_pattern Byte.zero 4 (* z1..z3*)

    let emit_parent = Emit.byte Byte.zero
    let emit_sibling = Emit.byte Byte.zero
    let emit_child = Emit.byte Byte.zero

    let emits (ts : t list) : unit Emit.t = 
      Emit.reverse_bind
	~first_f:(fun ~backwards_flowing_info:pairs ->
	    Emit.seq (
	      List.map pairs ~f:(fun (_t,prop_table) ->
		Emit.seq [
		  emit_attributes;
		  emit_parent;
		  emit_sibling;
		  emit_child;
		  Emit.loc prop_table;
		])))
	~second:(
	  Emit.concat (
	    List.map ts ~f:(fun t ->
	      Emit.here >>= fun loc ->
	      emit_prop_table t >>= fun () ->
	      return (t,loc))))

  end

  type t = Ob.t list [@@deriving sexp_of]
  let create xs = xs

  let encode_prop_defaults = (* 31 words for z1..z3 *)
    Emit.seq (
      List.map (List.range 1 32) ~f:(fun i ->
	Emit.word (Word.of_int_exn i)))
      
  let encode xs = 
    Emit.seq [
      encode_prop_defaults;
      Ob.emits xs;
    ]

end

module I = struct

  type arg_desc = ByteConst | WordConst | ByteVar

  type opkind = 
  | Op0
  | Op1 of arg_desc
  (*  | Op2 of arg_desc * arg_desc *)
  | Op2varForm
  | OpV

  let arg_desc_of_arg = function
    | Instr.Var _ -> ByteVar
    | Instr.Byte _ -> ByteConst
    | Instr.Word _ -> WordConst
    | Instr.Target _ -> ByteConst
    | Instr.String _ -> WordConst
    | Instr.Int _ -> WordConst

  let argsDescI = 
    function
    | Instr.Print _ -> []
    | Instr.Print_paddr _ -> (*[arg_desc_of_arg a]*) []
    | Instr.Newline -> []
    | Instr.Quit -> []
    | Instr.Print_num a -> [arg_desc_of_arg a]

    | Instr.Store (a,b)
    | Instr.Sread (a,b)
    | Instr.Add (a,b,_)
    | Instr.Sub (a,b,_)
    | Instr.Mul (a,b,_)
    | Instr.Jump_eq (a,b,_)
      -> [arg_desc_of_arg a; arg_desc_of_arg b]
    | Instr.Define_label _ -> failwith "argsDescI: Label"
      
  let opkindI = 
    function
(*    | Instr.Store (a,b) -> Op2 (arg_desc_of_arg a, arg_desc_of_arg b)*)
    | Instr.Store _ -> Op2varForm
    | Instr.Print _ -> Op0
    | Instr.Print_paddr a -> Op1 (arg_desc_of_arg a)
    | Instr.Newline -> Op0
    | Instr.Quit -> Op0
    | Instr.Sread _ -> OpV
    | Instr.Print_num _ -> OpV
    | Instr.Add _ -> Op2varForm
    | Instr.Sub _ -> Op2varForm
    | Instr.Mul _ -> Op2varForm
    | Instr.Define_label _ -> failwith "opkindI: Label"
    | Instr.Jump_eq _ -> Op2varForm

  let opcodeI = 
    function
    | Instr.Store _ -> 13
    | Instr.Print _ -> 2
    | Instr.Print_paddr _ -> 13
    | Instr.Newline -> 11
    | Instr.Quit -> 10
    | Instr.Sread _ -> 4
    | Instr.Print_num _ -> 6
    | Instr.Add _ -> 20
    | Instr.Sub _ -> 21
    | Instr.Mul _ -> 22
    | Instr.Define_label _ -> failwith "opcodeI: Label"
    | Instr.Jump_eq _ -> 1
      
  let bits_of_arg_desc = function
    | WordConst -> 0
    | ByteConst -> 1
    | ByteVar -> 2

  let bits_of_no_arg = 3

  let assert3 n = assert (n>=0 && n<=3)

  let combine4 : int * int * int * int -> Byte.t =
    fun (a,b,c,d) ->
      assert3 a; assert3 b; assert3 c; assert3 d;
      Byte.of_int_exn ((a lsl 6) lor (b lsl 4) lor (c lsl 2) lor d)

  let isOp1 = function Op1 _ -> true | _ -> false

  let emit_arg_desc_list opkind = function
    | [] -> Emit.nothing
    | [_] when isOp1 opkind -> Emit.nothing (* did it when wrote opcode *)

    | [d1] -> 
      combine4 (bits_of_arg_desc d1,
		bits_of_no_arg,
		bits_of_no_arg,
		bits_of_no_arg) |> Emit.byte

    | [d1;d2] -> 
      combine4 (bits_of_arg_desc d1,
		bits_of_arg_desc d2,
		bits_of_no_arg,
		bits_of_no_arg) |> Emit.byte

    | _ -> 
      failwith "emit_arg_desc_list"

  let int_of_var = function
    | Var.Sp -> 0
    | Local n -> assert (n>=1 && n<15); n
    | Global n -> assert (n>=0 && n<=239); (16+n)

  let emit_var var = 
    Emit.byte (Byte.of_int_exn (int_of_var var))

  let emit_arg env zversion = function
    | Instr.Byte b -> Emit.byte b
    | Instr.Word w -> Emit.word w
    | Instr.Var var -> emit_var var

    | Instr.String id -> 
      let mark = Text.lookup env id in
      Emit.packed_address zversion mark

    | Instr.Target var -> emit_var var
    | Instr.Int i -> Emit.word (Value.to_word (Value.of_int i))
      

  let byte_of_opcode : (opkind -> opcode:int -> Byte.t) =
    fun opkind ~opcode ->

      match opkind with
      | Op0 ->
	assert (opcode >=0 && opcode <= 31);
	let byte = Byte.of_int_exn opcode in
	let byte = Byte.set_bitN 7 byte in
	let byte = Byte.set_bitN 5 byte in
	let byte = Byte.set_bitN 4 byte in
	byte

      | Op1 arg_desc ->
	assert (opcode >=0 && opcode <= 31);
	let byte = Byte.of_int_exn opcode in
	let byte = Byte.set_bitN 7 byte in
	Byte.of_int_exn (
	  Byte.to_int byte 
	  lor (bits_of_arg_desc arg_desc lsl 4) (* bits 5/4 *)
	)

(*      | Op2 (desc1, desc2) ->*)

      | Op2varForm ->
	assert (opcode >=0 && opcode <= 31);
	let byte = Byte.of_int_exn opcode in
	let byte = Byte.set_bitN 7 byte in
	let byte = Byte.set_bitN 6 byte in
	let byte = Byte.clear_bitN 5 byte in
	byte

      | OpV ->
	assert (opcode >=0 && opcode <= 63);
	let byte = Byte.of_int_exn opcode in
	let byte = Byte.set_bitN 7 byte in
	let byte = Byte.set_bitN 6 byte in
	let byte = Byte.set_bitN 5 byte in
	byte

  let emit_opcode i = 
    let opkind = opkindI i in
    let opcode = opcodeI i in
    let arg_descs = argsDescI i in
    let byte = byte_of_opcode opkind ~opcode in
    Emit.seq [
      Emit.byte byte;
      emit_arg_desc_list opkind arg_descs;
    ]


  let emit_args env zversion = 
    function
    | Instr.Print string -> Text.String.encode string
    | Instr.Newline -> Emit.nothing
    | Instr.Quit -> Emit.nothing

    | Instr.Print_paddr arg
    | Instr.Print_num arg 
      -> emit_arg env zversion arg

    | Instr.Store(a,b)
    | Instr.Sread(a,b) 
      -> 
      Emit.seq [
	emit_arg env zversion a;
	emit_arg env zversion b;
      ]

    | Instr.Add(a,b,var)  
    | Instr.Sub(a,b,var)  
    | Instr.Mul(a,b,var)  
      ->
      Emit.seq [
	emit_arg env zversion a;
	emit_arg env zversion b;
	emit_var var
      ]

    | Instr.Define_label _ -> failwith "emit: Label"
    | Instr.Jump_eq (a,b,_) ->
      Emit.seq [
	emit_arg env zversion a;
	emit_arg env zversion b;
      ]


  let emit env zversion i = 
    Emit.seq [
      emit_opcode i;
      emit_args env zversion i
    ]

end

module Header = struct

  let emit_of_zversion zversion = Emit.bytes [Zversion.to_byte zversion]

  let emit_of_string s = 
    Emit.bytes (List.map (String.to_list s) ~f:Byte.of_char)
	   
  let emit_zeros n = 
    Emit.bytes (List.init n ~f:(fun _ -> Byte.of_int_exn 0))

  let fixed_size = 64

  let emit zversion ~dict_loc ~objects_loc ~base_globals ~init_pc ~end_loc =
    Emit.assert_size ~tag:"header" fixed_size (
      Emit.seq [
	emit_of_zversion zversion; (* 1 *)
	emit_zeros 1;
	Emit.word (Word.of_int_exn 42);   (*release*)
	Emit.loc end_loc;  (*base_high*)
	Emit.loc init_pc;
	Emit.loc dict_loc;
	Emit.loc objects_loc;
	Emit.loc base_globals;
	Emit.loc end_loc; (* base_static *)
	emit_zeros 2;
	emit_of_string "NIFTY!"; (*6*)
	Emit.word (Word.of_int_exn 0);  (*base_abbrev*)
	emit_zeros 38;
      ])

end

module Lab : sig (* assembler branch/jump targets *)
  type t = Instr.label
  include Comparable with type t := t
end = struct
  module T = struct
    type t = Instr.label [@@deriving sexp, compare]
  end
  include T
  include Comparable.Make(T)
end

module Forward_offsets : sig
  type t
  val empty : t
  val increase : t -> int -> t
  val extend_label_at_zero_offset : t -> Instr.label -> t
  val lookup : t -> Instr.label -> int
end = struct
  type t = int Lab.Map.t
  let empty = Lab.Map.empty
  let increase m n = Lab.Map.map m ~f:(fun x -> (x+n))
  let extend_label_at_zero_offset m lab = Lab.Map.add m ~key:lab ~data:0
  let lookup m lab = Lab.Map.find_exn m lab
end


module Code = struct
    
  type t = Code of Instr.t list
  [@@deriving sexp_of]

  let create xs = Code xs

  let emit_label ~(forward_offset:int) : unit Emit.t =
    assert (forward_offset>=0);
    let forward_offset = forward_offset + 2 in (* 0,1 have special meaning *)
    if (forward_offset > 63) then failwith "todo: emit_label: long form" else
      let positive_sense = 0x1 lsl 7 in
      let small = 0x1 lsl 6 in
      Emit.byte (
	Byte.of_int_exn (positive_sense lor small lor forward_offset)
      )

      

  let emit_branch env zversion i ~(forward_offset:int) : unit Emit.t =
    Emit.seq [
      I.emit_opcode i;
      I.emit_args env zversion i;
      emit_label ~forward_offset
    ]


  let emit_i env zversion 
      : Forward_offsets.t -> Instr.t -> Forward_offsets.t Emit.t =
    fun fo (x:Instr.t) ->
      let _ = Forward_offsets.lookup in
      match x with
      | Instr.Define_label lab -> 
	return (Forward_offsets.extend_label_at_zero_offset fo lab)

      | Instr.Jump_eq (_,_,lab) -> 
	let forward_offset = Forward_offsets.lookup fo lab in
	emit_branch env zversion x ~forward_offset >>= fun () ->
	return fo

      | _ ->
	I.emit env zversion x >>= fun () ->
	return fo


  let emit_is env zversion : Instr.t list -> unit Emit.t =
    let rec loop : Instr.t list -> Forward_offsets.t Emit.t = function
      | [] -> return Forward_offsets.empty
      | x::xs -> 
	Emit.reverse_bind
	  ~first_f:(fun ~backwards_flowing_info:fo ->
	    Emit.size (emit_i env zversion fo x) >>= fun (fo,size) ->
	    return (Forward_offsets.increase fo size)
	  )
	  ~second:(
	    loop xs >>= fun fo ->
	    return fo
	  )
    in
    fun xs -> 
      loop xs >>= fun _fo ->
      return ()

  let compile zversion env (Code instructions) = 
    (* #args in dummy routine wrapping init code *)
    Emit.byte Byte.zero >>= fun () ->
    Emit.here >>= fun init_pc -> 
    emit_is env zversion instructions >>= fun () ->
    return init_pc

end

module Story = struct

  type t = {
    dict : Dict.t;
    objects : Objects.t;
    text : Text.store;
    code : Code.t;
  } [@@deriving sexp_of]

  let create dict objects text code = { 
    dict; 
    objects; 
    text;
    code;
  }

  let compile_globals_space n =
    emit_fill_pattern (*Byte.zero*) b77 (2*n)

  let n_globals = 40 (* really 240, indexed 0..239 *)

  let emit1 zversion t = 

    Emit.here >>= fun dict_loc ->
    Dict.encode t.dict >>= fun () ->

    Emit.here >>= fun objects_loc ->
    Objects.encode t.objects >>= fun () ->

    Emit.here >>= fun base_globals ->
    compile_globals_space n_globals >>= fun () ->

    begin
      Emit.reverse_bind
	~first_f:(fun ~backwards_flowing_info:(text_loc,env) ->
	  Emit.align2 >>= fun () ->
	  Code.compile zversion env t.code >>= fun init_pc ->
	  return (text_loc,init_pc)
	)
	~second:(
	  Emit.align2 >>= fun () ->
	  Emit.here >>= fun text_loc ->
	  Text.encode t.text >>= fun env ->
	  return (text_loc,env)
	)
    end

    >>= fun (text_loc,init_pc) ->
      
    Emit.here >>= fun end_loc ->
    let header = 
      Header.emit zversion
	~dict_loc ~objects_loc ~base_globals ~init_pc ~end_loc
    in

    return (text_loc,header)

  let emit zversion t =
    Emit.reverse_bind 
      ~first_f:(fun ~backwards_flowing_info:(text_loc,header) -> 
	header >>= fun () ->
	return text_loc
      )
      ~second:(emit1 zversion t)

  let assemble zversion story = 
    let text_loc,bytes,locate = 
      Emit.exec (emit zversion story) ~start:Loc.zero
    in
    locate text_loc, bytes

end

module Bytes = struct

  let chop size : 'a list -> 'a list list =
    let rec loop n ~acc xs =
      if n <= size then List.rev (xs::acc) else
	let (front,rest) = List.split_n xs size in
	loop (n - size) ~acc:(front::acc) rest
    in
    fun xs -> loop (List.length xs) ~acc:[] xs

  let show_hex_bytes t = 
    List.iteri (chop 16 t) ~f:(fun _i bs ->
      printf "%s\n"
	(String.concat ~sep:" " (List.map bs ~f:Byte.to_hexstring)))

  let to_story_data t =
    String.of_char_list (List.map t ~f:Byte.to_char)

  let write t ~target_file = 
    Out_channel.write_all target_file ~data:(to_story_data t)

end

let bar() = printf "--------------------------------------------------\n"

module Disassemble : sig

  val show_text_and_code : text_loc:Loc.t -> Byte.t list -> unit

end = struct

  let show_text_and_code ~text_loc bytes = 
    let module Z = Z_machine in
    let story = Bytes.to_story_data bytes in
    let mem = Z.Mem.from_string ~story in
    let () = bar() in
    let () = 
      let module Text = Z.Text.F(struct let the_mem = mem end) in
      let text_start = text_loc in
      let text_end = Loc.of_int (Z.Mem.size mem) in
      let () = Text.print_between(text_start,text_end) in
      ()
    in
    let () = bar() in
    let () = 
      let module I_decoder = Z.I_decoder.F(struct let the_mem = mem end) in
      let code_start = Z.Header.code_start mem in
      let code_end = text_loc in
      let () = I_decoder.disassemble_between(code_start,code_end) in
      ()
    in
    let () = bar() in
    ()

end

let assemble zversion story =
  let _text_loc,bytes = Story.assemble zversion story in
  let story = Bytes.to_story_data bytes in
  let image = Z_machine.Mem.from_string ~story in
  image

let assemble_and_disassemble zversion story =
  let () = printf !"assembly...%{sexp:Story.t}\n" story in
  let () = bar() in
  let text_loc,bytes = Story.assemble zversion story in
  let () = Bytes.show_hex_bytes bytes in
  let () = Disassemble.show_text_and_code ~text_loc bytes in
  ()

let assemble_and_save zversion story =
  let () = printf !"assembly...%{sexp:Story.t}\n" story in
  let () = bar() in
  let target_file = 
    sprintf "%s.%s" 
      (String.chop_suffix_exn ~suffix:".exe" (Filename.basename Sys.argv.(0)))
      (Zversion.to_file_extension zversion)
  in
  let text_loc,bytes = Story.assemble zversion story in
  let () = bar() in
  let () = printf "write story1 to %s...\n" target_file in
  let () = Bytes.write bytes ~target_file in
  let () = bar() in
  let () = Bytes.show_hex_bytes bytes in
  let () = Disassemble.show_text_and_code ~text_loc bytes in
  ()
