open Core
open Z_machine.Numbers
module Value = Z_machine.Value
module Arg = Instr.Arg

let (>>=) = Emit.(>>=)
let return = Emit.return

let rec emit_sequence emit = function
  | [] -> Emit.nothing
  | x::xs -> emit x >>= fun () -> emit_sequence emit xs

let emit_fill_pattern byte n = 
  Emit.bytes (List.init n ~f:(fun _ -> byte))

let b77 = Byte.of_int_exn 0x77
let bFF = Byte.of_int_exn 0xFF
let _ = b77,bFF

module Buffers : sig

  type t [@@deriving sexp_of]
  val create : (string * int) list -> t

  type env
  val encode : t -> env Emit.t

  val lookup : env -> string -> Emit.mark

end = struct

  type t = {
    pairs : (string * int) list
  } [@@deriving sexp_of]

  module Map = String.Map

  let create pairs = { pairs }

  type env = { map : Emit.mark Map.t; }
  let lookup e text = Map.find_exn e.map text

  let encode t =
    let rec loop acc = function
      | [] -> return { map = Map.of_alist_exn acc }
      | (name,size)::entries ->
	Emit.here >>= fun loc ->
	emit_fill_pattern Byte.zero size >>= fun () ->
	loop ((name,loc)::acc) entries
    in
    loop [] t.pairs

end

module Env : sig
  type t
  val create : Text.env -> Buffers.env -> Dict.env -> t
  val lookup_text : t -> Text.id -> Emit.mark
  val lookup_buffer : t -> string -> Emit.mark
  val lookup_dict_entry : t -> Dict.word -> Emit.mark
end = struct
  type t = {
    text_env : Text.env;
    buffers_env : Buffers.env;
    dict_env : Dict.env;
  }
  let create text_env buffers_env dict_env = { text_env; buffers_env; dict_env }
  let lookup_text t id = Text.lookup t.text_env id
  let lookup_buffer t s = Buffers.lookup t.buffers_env s
  let lookup_dict_entry t w = Dict.lookup t.dict_env w
end


module II = struct

  type arg_desc = ByteConst | WordConst | ByteVar | NoArg

  let arg_desc_of_arg = function
    | Arg.Var _ -> ByteVar
    | Arg.Byte _ -> ByteConst
    | Arg.Word _ -> WordConst
    | Arg.Target _ -> ByteConst
    | Arg.String _ -> WordConst
    | Arg.Int _ -> WordConst
    | Arg.Buffer _ -> WordConst
    | Arg.Dict_entry _ -> WordConst
    | Arg.Mark _ -> WordConst
    | Arg.Lab _ -> WordConst

  let int_of_var = function
    | Var.Sp -> 0
    | Local n -> assert (n>=1 && n<15); n
    | Global n -> assert (n>=0 && n<=239); (16+n)
 
  let emit_var var = 
    Emit.byte (Byte.of_int_exn (int_of_var var))

  let emit_arg env zversion = function
    | Arg.Byte b -> Emit.byte b
    | Arg.Word w -> Emit.word w
    | Arg.Var var -> emit_var var

    | Arg.String id -> 
      let mark = Env.lookup_text env id in
      Emit.packed_address zversion mark

    | Arg.Target var -> emit_var var
    | Arg.Int i -> Emit.word (Value.to_word (Value.of_int i))
      
    | Arg.Buffer s -> 
      let mark = Env.lookup_buffer env s in
      Emit.byte_address mark

    | Arg.Dict_entry w -> 
      let mark = Env.lookup_dict_entry env w in
      Emit.byte_address mark

    | Arg.Mark mark -> 
      let zversion = Zversion.Z3 in (* HACK - need to thread zvesion here! *)
      Emit.packed_address zversion mark
      
    | Arg.Lab _ ->
      (* TODO: move the label emit code here *)
      Emit.nothing
      (*failwith "emit_arg: Label"*)

  type opkind = 
  | Op0 of int 
  | Op1 of int
  | Op2 of int
  | OpV of int

  type extra = 
  | X
  | T of string
  | V of Var.t
  | B of string (*label*)

  let bits_of_arg_desc = function
    | WordConst -> 0
    | ByteConst -> 1
    | ByteVar -> 2
    | NoArg -> 3

  let assert3 n = assert (n>=0 && n<=3)

  let combine4 : int * int * int * int -> Byte.t =
    fun (a,b,c,d) ->
      assert3 a; assert3 b; assert3 c; assert3 d;
      Byte.of_int_exn ((a lsl 6) lor (b lsl 4) lor (c lsl 2) lor d)

  let mapQuad (a,b,c,d) ~f = (f a, f b, f c, f d)

  let emit_arg_desc_byte quad =
      let quad = mapQuad quad ~f:bits_of_arg_desc in
      combine4 quad |> Emit.byte


  let byte_of_opcode : (opkind -> arg_desc list -> Byte.t * arg_desc list) =
    fun opkind descs ->

      match opkind with
      | Op0 opcode ->
	assert (opcode >=0 && opcode <= 31);
	let byte = Byte.of_int_exn opcode in
	let byte = Byte.set_bitN 7 byte in
	let byte = Byte.set_bitN 5 byte in
	let byte = Byte.set_bitN 4 byte in
	byte, descs

      | Op1 opcode ->
	let arg_desc,descs = 
	  match descs with x::xs -> x,xs | [] -> failwith "Op1"
	in

	assert (opcode >=0 && opcode <= 31);
	let byte = Byte.of_int_exn opcode in
	let byte = Byte.set_bitN 7 byte in
	let byte = Byte.of_int_exn (
	  Byte.to_int byte 
	  lor (bits_of_arg_desc arg_desc lsl 4) (* bits 5/4 *)
	) in
	byte, descs

      | Op2 opcode -> (* always write out in var form. 
			 TODO: do the when short form is possible! *)
	assert (opcode >=0 && opcode <= 31);
	let byte = Byte.of_int_exn opcode in
	let byte = Byte.set_bitN 7 byte in
	let byte = Byte.set_bitN 6 byte in
	let byte = Byte.clear_bitN 5 byte in
	byte, descs

      | OpV opcode ->
	assert (opcode >=0 && opcode <= 63);
	let byte = Byte.of_int_exn opcode in
	let byte = Byte.set_bitN 7 byte in
	let byte = Byte.set_bitN 6 byte in
	let byte = Byte.set_bitN 5 byte in
	byte, descs

  let get_opcode_and_args =
    function
    | Instr.Label _             -> failwith "get_opcode_and_args: Label"
    | Instr.Print string        -> Op0  2 , [], T string
    | Instr.Newline             -> Op0 11 , [], X
    | Instr.Quit                -> Op0 10 , [], X

    | Instr.Print_addr a        -> Op1  7 , [a], X 
    | Instr.Print_paddr a       -> Op1 13 , [a], X
    | Instr.Print_obj a         -> Op1 10 , [a], X
    | Instr.Jump a              -> Op1 12 , [a], X

    | Instr.Jump_eq (a,b,lab)   -> Op2  1 , [a;b], B lab
    | Instr.Jump_neq (a,b,lab)  -> Op2  1 , [a;b], B lab (* sense=false*)

    | Instr.Jump_lt (a,b,lab)   -> Op2  2 , [a;b], B lab
    | Instr.Jump_geq (a,b,lab)  -> Op2  2 , [a;b], B lab (* sense=false *)

    | Instr.Jump_gt (a,b,lab)   -> Op2  3 , [a;b], B lab
    | Instr.Jump_leq (a,b,lab)  -> Op2  3 , [a;b], B lab (* sense=false *)

    | Instr.Store(a,b)          -> Op2 13 , [a;b], X
    | Instr.Load_byte(a,b,var)  -> Op2 16 , [a;b], V var
    | Instr.Load_word(a,b,var)  -> Op2 15 , [a;b], V var
    | Instr.Add(a,b,var)        -> Op2 20 , [a;b], V var
    | Instr.Sub(a,b,var)        -> Op2 21 , [a;b], V var
    | Instr.Mul(a,b,var)        -> Op2 22 , [a;b], V var
    | Instr.Div(a,b,var)        -> Op2 23 , [a;b], V var
    | Instr.Mod(a,b,var)        -> Op2 24 , [a;b], V var

    | Instr.Call0 (arg,var)     -> OpV  0 , [arg], V var
    | Instr.Store_byte(a,b,c)   -> OpV  2 , [a;b;c], X
    | Instr.Push(a)             -> OpV  8 , [a], X
    | Instr.Print_num arg       -> OpV  6 , [arg], X
    | Instr.Print_char arg      -> OpV  5 , [arg], X
    | Instr.Sread(a,b)          -> OpV  4 , [a;b], X


  let get4argdesc = function
    | []            -> None
    | [d1]          -> Some (d1,NoArg,NoArg,NoArg)
    | [d1;d2]       -> Some (d1,d2,NoArg,NoArg)
    | [d1;d2;d3]    -> Some (d1,d2,d3,NoArg)
  (*| [d1;d2;d3;d4] -> Some (d1,d2,d3,d4) *)
    | _             -> failwith "emit_arg_desc_list"

  let emit_arg_desc_list descs =
    match get4argdesc descs with
    | None -> Emit.nothing
    | Some quad -> emit_arg_desc_byte quad



  let emit_instruction env zversion i =
    let opkind,args,extra = get_opcode_and_args i in

    let descs = List.map args ~f:arg_desc_of_arg in

    let byte,descs = byte_of_opcode opkind descs in
    Emit.byte byte >>= fun () ->

    emit_arg_desc_list descs >>= fun () ->
    emit_sequence (emit_arg env zversion) args >>= fun () ->
    begin
      match extra with
      | X           -> Emit.nothing
      | T string    -> Text.String.encode string
      | V var       -> emit_var var
      | B _         -> Emit.nothing (* LATER, TODO: do it here *)
    end

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
  val extend : t -> Instr.label -> Emit.mark -> t
  val lookup_offset : t -> Instr.label -> int
  val lookup_mark : t -> Instr.label -> Emit.mark
end = struct
  type t = (Emit.mark * int) Lab.Map.t
  let empty = Lab.Map.empty
  let increase m n = Lab.Map.map m ~f:(fun (mark,x) -> (mark,x+n))
  let extend m lab mark = Lab.Map.add m ~key:lab ~data:(mark,0)
  let lookup_offset m lab = snd (Lab.Map.find_exn m lab)
  let lookup_mark m lab = fst (Lab.Map.find_exn m lab)
end

module Backwards_marks : sig
  type t
  val empty : t
  val extend : t -> Instr.label -> Emit.mark -> t
  val lookup : t -> Instr.label -> Emit.mark option
end = struct
  type t = Emit.mark Lab.Map.t
  let empty = Lab.Map.empty
  let extend m lab mark = Lab.Map.add m ~key:lab ~data:mark
  let lookup m lab = Lab.Map.find m lab
end

module Code = struct
    
  type t = Code of Instr.t list
  [@@deriving sexp_of]

  let create xs = Code xs

  let emit_label ~(forward_offset:int) ~sense : unit Emit.t =
    assert (forward_offset>=0);
    let forward_offset = forward_offset + 2 in (* 0,1 have special meaning *)
    if (forward_offset > 63) 
    then 
      (*failwithf "todo: emit_label: long form needed, offset=(%d)" 
	forward_offset ()*)
      let h,l = Word.to_high_low (Word.of_int_exn forward_offset) in
      assert (not (Byte.bitN 7 h));
      assert (not (Byte.bitN 6 h));
      let h = if sense then (Byte.set_bitN 7 h) else h in
      Emit.seq [Emit.byte h; Emit.byte l]
    else
      let sense_bit = if sense then 0x1 lsl 7 else 0 in
      let small = 0x1 lsl 6 in
      Emit.byte (
	Byte.of_int_exn (sense_bit lor small lor forward_offset)
      )

  let emit_branch env zversion i ~(forward_offset:int) ~sense : unit Emit.t =
    Emit.seq [
      II.emit_instruction env zversion i;
      emit_label ~forward_offset ~sense
    ]


  let emit_offset_for_jump =
    let f (offset:int) =
      let offset = offset + 2 in (* 0,1 have special meanings *)
      Value.to_word (Value.of_int offset) (* signed, so use Value *)
    in
    fun ~from:m1 ~target:m2 -> 
      Emit.offset f (m1,m2)


  let emit_jump env zversion i ~target : unit Emit.t =
    Emit.reverse_bind 
      ~first_f:(fun ~backwards_flowing_info:from ->
	Emit.seq [
	  II.emit_instruction env zversion i;
	  emit_offset_for_jump ~from ~target;
	])
      ~second:Emit.here

  let emit_i env zversion 
      : Backwards_marks.t -> Forward_offsets.t -> Instr.t
    -> Forward_offsets.t Emit.t =
    fun bm fo (x:Instr.t) ->
      match x with
      | Instr.Label lab -> 
	Emit.here >>= fun mark ->
	return (Forward_offsets.extend fo lab mark)

      | Instr.Jump_eq (_,_,lab) -> 
	let forward_offset = Forward_offsets.lookup_offset fo lab in
	emit_branch env zversion x ~forward_offset ~sense:true >>= fun () ->
	return fo
      | Instr.Jump_neq (_,_,lab) -> 
	let forward_offset = Forward_offsets.lookup_offset fo lab in
	emit_branch env zversion x ~forward_offset ~sense:false >>= fun () ->
	return fo
      | Instr.Jump_lt (_,_,lab) -> 
	let forward_offset = Forward_offsets.lookup_offset fo lab in
	emit_branch env zversion x ~forward_offset ~sense:true >>= fun () ->
	return fo
      | Instr.Jump_gt (_,_,lab) -> 
	let forward_offset = Forward_offsets.lookup_offset fo lab in
	emit_branch env zversion x ~forward_offset ~sense:true >>= fun () ->
	return fo
      | Instr.Jump_leq (_,_,lab) -> 
	let forward_offset = Forward_offsets.lookup_offset fo lab in
	emit_branch env zversion x ~forward_offset ~sense:false >>= fun () ->
	return fo
      | Instr.Jump_geq (_,_,lab) -> 
	let forward_offset = Forward_offsets.lookup_offset fo lab in
	emit_branch env zversion x ~forward_offset ~sense:false >>= fun () ->
	return fo

      | Instr.Jump (Lab lab) ->
	let mark =
	  match Backwards_marks.lookup bm lab with
	  | None -> Forward_offsets.lookup_mark fo lab
	  | Some mark -> mark
	in
	emit_jump env zversion x ~target:mark >>= fun () ->
	return fo

      | Instr.Jump _ -> failwith "jump to non-constant label"

      | _ ->
	II.emit_instruction env zversion x >>= fun () ->
	return fo


  let emit_is env zversion : Instr.t list -> unit Emit.t =
    let rec loop bm : Instr.t list -> Forward_offsets.t Emit.t = function
      | [] -> return Forward_offsets.empty
      | x::xs -> 
	Emit.here >>= fun loc ->
	let bm = 
	  match x with
	  | Instr.Label lab -> Backwards_marks.extend bm lab loc
	  | _ -> bm
	in
	Emit.reverse_bind
	  ~first_f:(fun ~backwards_flowing_info:fo ->
	    Emit.size (emit_i env zversion bm fo x) >>= fun (fo,size) ->
	    return (Forward_offsets.increase fo size)
	  )
	  ~second:(
	    loop bm xs >>= fun fo ->
	    return fo
	  )
    in
    fun xs -> 
      let bm = Backwards_marks.empty in
      loop bm xs >>= fun _fo ->
      return ()


  let __compile zversion env (Code instructions) = 
    (* #args in dummy routine wrapping init code *)
    Emit.byte Byte.zero >>= fun () ->
    Emit.here >>= fun init_pc -> 
    emit_is env zversion instructions >>= fun () ->
    return init_pc


  let compile zversion env (Code instructions) =
    let emit_quit : unit Emit.t =
      II.emit_instruction env zversion Instr.Quit
    in
    let emit_call0 (mark:Emit.mark) : unit Emit.t =
      II.emit_instruction env zversion
	(Instr.Call0 (Arg.Mark mark, Var.Global 0))
    in
    Emit.reverse_bind
      ~first_f:(fun ~backwards_flowing_info:routine_start ->
	Emit.byte Byte.zero >>= fun () ->
	Emit.here >>= fun init_pc -> 
	emit_call0 routine_start >>= fun () ->
	emit_quit >>= fun () ->
	return init_pc
      )
      ~second:(
	Emit.align2 >>= fun () ->
	Emit.here >>= fun routine_start ->
	Emit.byte Byte.zero >>= fun () -> (* zero args in routine *)
	emit_is env zversion instructions >>= fun () ->
	return routine_start
      )


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
		  Emit.byte_address prop_table;
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

module Header = struct

  let emit_of_zversion zversion = Emit.bytes [Zversion.to_byte zversion]

  let emit_of_string s = 
    Emit.bytes (List.map (String.to_list s) ~f:Byte.of_char)
	   
  let emit_zeros n = 
    Emit.bytes (List.init n ~f:(fun _ -> Byte.of_int_exn 0))

  let fixed_size = 64

  let emit zversion ~dict_loc ~objects_loc ~base_globals ~init_pc 
      ~base_high ~base_static =
    Emit.assert_size ~tag:"header" fixed_size (
      Emit.seq [
	emit_of_zversion zversion; (* 1 *)
	emit_zeros 1;
	Emit.word (Word.of_int_exn 42);   (*release*)
	Emit.byte_address base_high;
	Emit.byte_address init_pc;
	Emit.byte_address dict_loc;
	Emit.byte_address objects_loc;
	Emit.byte_address base_globals;
	Emit.byte_address base_static;
	emit_zeros 2;
	emit_of_string "NIFTY!"; (*6*)
	Emit.word (Word.of_int_exn 0);  (*base_abbrev*)
	emit_zeros 38;
      ])

end

module Story = struct

  type t = {
    dict : Dict.t;
    objects : Objects.t;
    buffers : Buffers.t;
    text : Text.store;
    code : Code.t;
  } [@@deriving sexp_of]

  let create dict objects buffers text code = { 
    dict; 
    objects; 
    buffers;
    text;
    code;
  }

  let compile_globals_space n =
    emit_fill_pattern Byte.zero (*bFF*) (2*n)

  let n_globals = 40 (* really 240, indexed 0..239 *)

  let emit1 zversion t = 

    Emit.here >>= fun dict_loc ->
    Dict.encode t.dict >>= fun dict_env ->

    Emit.here >>= fun objects_loc ->
    Objects.encode t.objects >>= fun () ->

    Emit.here >>= fun base_globals ->
    compile_globals_space n_globals >>= fun () ->

    Buffers.encode t.buffers >>= fun buffer_env ->

    Emit.here >>= fun base_static ->

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
	  Text.encode t.text >>= fun text_env ->
	  let env = Env.create text_env buffer_env dict_env in
	  return (text_loc,env)
	)
    end

    >>= fun (text_loc,init_pc) ->
      
    Emit.here >>= fun end_loc ->
    let base_high = end_loc in

    let header = 
      Header.emit zversion
	~dict_loc ~objects_loc ~base_globals ~init_pc 
	~base_high ~base_static 
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
      let __() = I_decoder.disassemble_between(code_start,code_end) in
      let () = I_decoder.disassemble_reachable() in
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
