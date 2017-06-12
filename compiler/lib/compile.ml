open Core 
open Import
module Z = Z_machine
open Z.Numbers
module Arg = Instr.Arg
open Arg

module Buffers : sig (* make up for lack of abstraction in assembler *)

  type t
  type id 
  val empty : t
  val add : t -> size:int -> id * t
  val for_assembler : t -> Assemble.Buffers.t
  val name_for_assembler : id -> string

end = struct

  type id = { name : string }
  type buf = { id : id; size: int }
  type t = { next : int; bufs : buf list; }

  let empty = { next = 1; bufs = [] }
  let add t ~size = 
    let id = { name = sprintf "buffer_%d" t.next } in
    id, { next = t.next + 1; bufs = { id; size } :: t.bufs }

  let for_assembler t =
    Assemble.Buffers.create (List.map t.bufs ~f:(fun b -> b.id.name, b.size))

  let name_for_assembler id = id.name

end

module In_image : sig

  type 'a t
  val exec : 'a t -> 'a * Text.store * Dict.t * Assemble.Buffers.t
  val return : 'a -> 'a t
  val map : 'a t -> ('a -> 'b) -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  val allocate_global : Var.t t
  val allocate_string : string -> Text.id t
  val allocate_label : string t
  val add_dictionary_word : string -> Dict.word t

  val allocate_buffer : size:int -> Buffers.id t

end = struct
    
  type state = {
    global : int;
    text : Text.store;
    label : int;
    dict : Dict.t;
    buffers : Buffers.t;
  }
  let state0 = { 
    global = 3 ; (* leave 0,1,2 for something special *)
    text = Text.empty;
    label = 1;
    dict = Dict.empty;
    buffers = Buffers.empty;
  }  

  type 'a t = state -> 'a * state
    
  let return x s = x,s

  let (>>=) t f = fun s -> let x,s = t s in f x s

  let map t f = t >>= fun x -> return (f x)

  let exec t = 
    let x,s = t state0 in 
    x, s.text, s.dict, Buffers.for_assembler s.buffers

  let allocate_global = fun s -> 
    Var.Global s.global, { s with global = 1 + s.global }

  let allocate_string string = fun s ->
    let id,text = Text.add s.text string in
    id, { s with text }

  let allocate_label = fun s ->
    sprintf "L_%d" s.label, { s with label = s.label+1 }

  let add_dictionary_word string = fun s ->
    let word,dict = Dict.add s.dict string in
    word, { s with dict }

  let allocate_buffer ~size = fun s ->
    let id,buffers = Buffers.add s.buffers ~size in
    id, { s with buffers }
      
end

let return = In_image.return
let (>>=) = In_image.(>>=)
let (>>|) = In_image.map

type num

module Lv = struct

  type _ t = 
  | String : Var.t -> string t
  | Int : Var.t -> num t
  | Bool : Var.t -> bool t
  | Generic : Var.t -> 'a t

  let global var = 
    Generic var

  let global_string = 
    In_image.allocate_global >>= fun var ->
    In_image.return (String var)

  let global_int = 
    In_image.allocate_global >>= fun var ->
    In_image.return (Int var)

  let global_bool = 
    In_image.allocate_global >>= fun var ->
    In_image.return (Bool var)

  let compile_to_var : type a. a t -> Var.t = function
    | Generic var -> var
    | String var -> var
    | Int var -> var
    | Bool var -> var

  let compile t =
    let var = compile_to_var t in
    Var var

  let compile_as_target t =
    let var = compile_to_var t in
    Target var

end

module Global = struct
  let string = Lv.global_string
  let int = Lv.global_int
  let bool = Lv.global_bool
end


let compile_if_then_else = fun ~compile_branch ~compile_arm (cond,a1,a2) ->
  In_image.allocate_label >>= fun lab_true ->
  In_image.allocate_label >>= fun lab_join ->
  compile_branch cond lab_true >>= fun xs0 ->
  compile_arm a1 >>= fun xs_true ->
  compile_arm a2 >>= fun xs_false ->
  let xs = 
    List.concat [
      xs0;
      xs_false;
      [Instr.Jump (Arg.Lab lab_join);
       Label lab_true];
      xs_true;
      [Label lab_join];
    ]
  in
  return xs

module Exp = struct

  module Iop2 = struct
    type t = Add | Sub | Mul | Div | Mod 
    let compile t args = match t with
      | Add -> Instr.Add args
      | Sub -> Instr.Sub args
      | Mul -> Instr.Mul args
      | Div -> Instr.Div args
      | Mod -> Instr.Mod args
  end

  type _ t =
  | String : string -> string t
  | Bool : bool -> bool t
  | Int : int -> num t
  | Read : 'a Lv.t -> 'a t
  | Bin : Iop2.t * num t * num t -> num t

  | EqI : num t * num t -> bool t
  | LeqI : num t * num t -> bool t
  | GeqI : num t * num t -> bool t

  | Is_null_dict_word : Dict.word t -> bool t
  | Eq_dict_word : Dict.word t * Dict.word -> bool t
  | Num_entries_parse_buf : Buffers.id -> num t
  | Dict_word_from_parse_buf : Buffers.id * num t -> Dict.word t

  | Ite : bool t * 'a t * 'a t -> 'a t
    
  | CastInt : 'a t -> num t


  let maybe_invert (exp : bool t) : bool t option =
    match exp with
    | Bool b -> Some (Bool (not b))
    | LeqI (a,b) -> Some (GeqI (a,b))
    | GeqI (a,b) -> Some (LeqI (a,b))
    | _ -> None
  let _ = maybe_invert


  let string x = String x
  let int x = Int x

  let read lv = Read lv
  let add x y = Bin (Iop2.Add,x,y)
  let sub x y = Bin (Iop2.Sub,x,y)
  let mul x y = Bin (Iop2.Mul,x,y)
  let div x y = Bin (Iop2.Div,x,y)
  let mod_ x y = Bin (Iop2.Mod,x,y)

  let eqI x y = EqI (x,y)
  let leqI x y = LeqI (x,y)

  let castInt x = CastInt x

  let not e = 
    match maybe_invert e with
    | Some e -> e
    | None -> failwith "Exp.not"

  let bool x = Bool x
  let ite a b c = Ite (a,b,c)

  let compile_string = function
    | String s -> Some s

    | Read _lv
      -> None
    | Dict_word_from_parse_buf _ 
      -> None
    | Ite _
      -> None

  let rec compile : type a. (
    Instr.t list -> a t -> (Instr.t list * arg) In_image.t 
  ) = fun acc t -> 
    match t with
      
    | CastInt t -> compile acc t

    | Ite (cond,a1,a2) ->
      compile_if_then_else 
	~compile_branch:compile_branch
	~compile_arm:compile_push
	(cond,a1,a2)
      >>= fun xs ->
      let acc = List.rev xs @ acc in
      return (acc, Var Sp)

    | Read lv -> return (acc, Lv.compile lv)
    | String s ->      
      In_image.allocate_string s >>= fun id ->
      return (acc, Arg.String id)
    | Bool b ->
      return (acc, Arg.Int (if b then 1 else 0))
    | Int i ->
      return (acc, Arg.Int i)

    | Bin (iop2,e1,e2) ->
      (* We evaluate e2 before e1 so the sub-results are 
	 on the stack in the correct order for the bin-op instruction *)
      compile acc e2 >>= fun (acc,v2) ->
      compile acc e1 >>= fun (acc,v1) ->
      let acc = Iop2.compile iop2 (v1,v2, Sp) :: acc in 
      return (acc, Var Sp)

    | EqI (_e1,_e2) -> failwith "compile,EqI"
    | LeqI (_e1,_e2) -> failwith "compile,LeqI"
    | GeqI (_e1,_e2) -> failwith "compile,GeqI"
    | Is_null_dict_word (_w) -> failwith "compile,Is_null_dict_word"
    | Eq_dict_word (_w,_kw) -> failwith "compile,Eq_dict_word"

    | Num_entries_parse_buf (b) ->
      let buf = Buffers.name_for_assembler b in
      (* we should really use load byte here, at offset 1 *)
      (* now we are setting max-words in byte, we need to mask it here *)
      let acc = [Instr.Store_byte (Buffer buf,Arg.Int 0,Int 0)] @ acc in
      let acc = [Instr.Load_word (Buffer buf,Arg.Int 0,Sp)] @ acc in
      return (acc,Var Sp)

    | Dict_word_from_parse_buf (b,e1) ->
      compile acc e1 >>= fun (acc,v1) ->
      let acc = Instr.Mul (v1,Arg.Int 2,Sp) :: acc in
      let acc = Instr.Add (Var Sp,Arg.Int 1,Sp) :: acc in
      let buf = Buffers.name_for_assembler b in
      let acc = Instr.Load_word (Buffer buf,Var Sp,Sp) :: acc in
      return (acc,Var Sp)

  and compile_push : type a. (
    a t -> Instr.t list In_image.t
  ) = fun t ->
    let acc = [] in
    compile acc t >>= fun (acc,v) ->
    let acc = Instr.Push (v) :: acc in
    return (List.rev acc)
	 
  and compile_branch : (
    bool t -> Instr.label -> Instr.t list In_image.t
  ) = fun t lab ->
    match t with

    | Read lv -> 
      let arg1 : arg = Lv.compile lv in
      let arg2 = Arg.Int (0) in
      return [Instr.Jump_neq (arg1,arg2,lab)]

    | Bool b ->
      let arg1 = Arg.Int (if b then 1 else 0) in
      let arg2 = Arg.Int (1) in
      (* we could/should determine this jump/or-not at compile time *)
      return [Instr.Jump_eq (arg1,arg2,lab)]

    | EqI (e1,e2) -> 
      let acc = [] in
      compile acc e2 >>= fun (acc,v2) ->
      compile acc e1 >>= fun (acc,v1) ->
      let acc = Instr.Jump_eq (v1,v2,lab) :: acc in
      return (List.rev acc)

    | LeqI (e1,e2) -> 
      let acc = [] in
      compile acc e2 >>= fun (acc,v2) ->
      compile acc e1 >>= fun (acc,v1) ->
      let acc = Instr.Jump_leq (v1,v2,lab) :: acc in
      return (List.rev acc)

    | GeqI (e1,e2) -> 
      let acc = [] in
      compile acc e2 >>= fun (acc,v2) ->
      compile acc e1 >>= fun (acc,v1) ->
      let acc = Instr.Jump_geq (v1,v2,lab) :: acc in
      return (List.rev acc)

    | Is_null_dict_word (ew) ->
      let acc = [] in
      compile acc ew >>= fun (acc,arg1) ->
      let arg2 = Arg.Int (0) in
      let acc = Instr.Jump_eq (arg1,arg2,lab) :: acc in
      return (List.rev acc)

    | Eq_dict_word (ew,kw) ->
      let acc = [] in
      compile acc ew >>= fun (acc,arg1) ->
      let arg2 = Arg.Dict_entry (kw) in
      let acc = Instr.Jump_eq (arg1,arg2,lab) :: acc in
      return (List.rev acc)

    | Dict_word_from_parse_buf (_b,_n) 
      -> failwith "compile_branch,Dict_word_from_parse_buf"

    | Ite _ -> failwith "compile_branch,Ite"
      
end

module Action = struct

  type _ t =
  | Newline : unit t
  | Quit : unit t
  | Print : string Exp.t -> unit t
  | Print_num : num Exp.t -> unit t
  | Assign : 'a Lv.t * 'a Exp.t -> unit t
  | Seq : unit t * 'a t -> 'a t
  | If_ : bool Exp.t * 'a t * 'a t -> 'a t
  | Forever : 'a t -> 'a t
  | Sread : Buffers.id * Buffers.id -> unit t
  | Let_ : 'a Exp.t * ('a Exp.t -> 'b t) -> 'b t

  let newline = Newline
  let quit = Quit
  let print x = Print x
  let print_num x = Print_num x
  let assign lv exp = Assign (lv,exp)
  let (-$$) a b = Seq (a,b)
  let if_ a b c = If_(a,b,c)
  let forever a = Forever(a)
  let let_ bound f = Let_ (bound,f)

  let rec compile0 : unit t -> Instr.t list In_image.t = fun t ->
    compile [] t >>= fun acc ->
    return (List.rev acc)

  and compile acc = function
    | Newline ->
      return (Instr.Newline :: acc)

    | Quit ->
      return (Instr.Quit :: acc)
      
    | Print s_exp -> 
      begin match Exp.compile_string s_exp with
      | Some s -> return (Instr.Print s :: acc)
      | None ->
	Exp.compile acc s_exp >>= fun (acc,arg) ->
	return (Instr.Print_paddr arg :: acc)
      end

    | Print_num i_exp -> 
      Exp.compile acc i_exp >>= fun (acc,arg) ->
      return (Instr.Print_num arg :: acc)

    | Seq (a,b) -> 
      compile acc a >>= fun acc ->
      compile acc b

    | Assign (lv,exp) -> 
      let target = Lv.compile_as_target lv in
      Exp.compile acc exp >>= fun (acc,value) ->
      return (Instr.Store (target,value) :: acc)

    | If_ (cond,a1,a2) ->
      compile_if_then_else
	~compile_branch:Exp.compile_branch
	~compile_arm:compile0
	(cond,a1,a2)
      >>= fun xs ->
      let acc = List.rev xs @ acc in
      return acc

    | Forever (body) ->
      In_image.allocate_label >>= fun lab_start ->
      compile0 body >>= fun xs_body ->
      let xs = 
	List.concat [
	  [Instr.Label lab_start];
	  xs_body;
	  [Instr.Jump (Arg.Lab lab_start)];
	]
      in
      let acc = List.rev xs @ acc in
      return acc

    | Sread (tbuf,pbuf) ->
      let tbuf = Buffers.name_for_assembler tbuf in
      let pbuf = Buffers.name_for_assembler pbuf in
      (* HACK - assume tbug has size 100 *)
      let acc = Instr.Store_byte (Buffer tbuf, Int 0, Int 99) :: acc in 
      (* HACK - and assume we want say max of 7 words *)
      let acc = Instr.Store_byte (Buffer pbuf, Int 0, Int 7) :: acc in 
      let acc = Instr.Sread (Buffer tbuf, Buffer pbuf) :: acc in
      return acc

(*
    | Let_ (bound,f) ->
      (* NO SHARING - COMPUTATION IS REPEATED *)
      compile acc (f bound)
*)

    | Let_ (bound,f) ->
      
      Exp.compile acc bound >>= fun (acc,value) ->
      In_image.allocate_global >>= fun var ->
      let acc = Instr.Store (Target var,value) :: acc in
      let lv = Lv.global var in
      compile acc (f (Exp.read lv))


  let compile1 : unit t -> Instr.t list In_image.t = fun t ->
    (* We accumulate instructions by pushing onto acc. 
       Then reverse at the end *) 
    (* TODO: kill this accumulator stuff, which is just a premature optimization
       & makes it much easier to make a mistake in code-gen phase *)
    compile [] t >>= fun acc ->
    let acc = Instr.Quit :: acc in
    return (List.rev acc)

end
  
module Word = struct

  type t = Dict.word

  let add string = In_image.add_dictionary_word string
  let is_null x = Exp.Is_null_dict_word x
  let eq t_exp t = Exp.Eq_dict_word (t_exp,t)

end

module Text_buf = struct
  type t = TB of Buffers.id
  let create ~size = In_image.allocate_buffer ~size >>| fun x -> TB x
end

module Parse_buf = struct
  type t = PB of Buffers.id
  let create ~size = In_image.allocate_buffer ~size >>| fun x -> PB x
  let num_entries (PB buf) = Exp.Num_entries_parse_buf buf
  let get (PB buf) ie = Exp.Dict_word_from_parse_buf (buf,ie)
end

module Parse = struct
  let sread (Text_buf.TB tbuf) (Parse_buf.PB pbuf) = 
    Action.Sread(tbuf,pbuf)
end


let compile (uam : unit Action.t In_image.t) = 
  let objects = Assemble.Objects.create [] in (* no objects! *)
  
  let instructions,text,dict,buffers = 
    In_image.exec (
      uam >>= fun unit_action ->
      Action.compile1 unit_action
    ) 
  in

  let code = Assemble.Code.create instructions in

  let story = Assemble.Story.create dict objects buffers text code in
  story 

let compile_and_assemble zversion uam = 
  Assemble.assemble zversion (compile uam)
