module Z = Z_machine
open Z.Numbers

module In_image : sig

  type 'a t
  val exec : 'a t -> 'a * Text.store
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  val allocate_global : Var.t t

  val allocate_string : string -> Text.t t

end = struct
    
  type state = {
    global : int;
    text : Text.store;
  }
  let state0 = { 
    global = 3 ; (* leave 0,1,2 for something special *)
    text = Text.empty;
  }  

  type 'a t = state -> 'a * state
    
  let return x s = x,s

  let (>>=) t f = fun s -> let x,s = t s in f x s

  let exec t = let x,s = t state0 in x, s.text

  let allocate_global = fun s -> 
    Var.Global s.global, { s with global = 1 + s.global }

  let allocate_string string = fun s ->
    let id,text = Text.add s.text string in
    id, { s with text }
      
end

let return = In_image.return
let (>>=) = In_image.(>>=)

module Lv : sig 

  type 'a t
  val global_string : string t In_image.t
  val global_int : int t In_image.t
    
  val compile : 'a t -> Instr.arg
  val compile_as_target : 'a t -> Instr.arg

end = struct

  type _ t = 
  | String : Var.t -> string t
  | Int : Var.t -> int t

  let global_string = 
    In_image.allocate_global >>= fun var ->
    In_image.return (String var)

  let global_int = 
    In_image.allocate_global >>= fun var ->
    In_image.return (Int var)

  let compile_to_var : type a. a t -> Var.t = function
    | String var -> var
    | Int var -> var

  let compile t =
    let var = compile_to_var t in
    Instr.Var var

  let compile_as_target t =
    let var = compile_to_var t in
    Instr.Target var

end

module Global = struct
  let string = Lv.global_string
  let int = Lv.global_int
end


module Exp : sig

  type 'a t
  val string : string -> string t
  val int : int -> int t
  val read : 'a Lv.t -> 'a t
  val add : int t -> int t -> int t
  val sub : int t -> int t -> int t
  val mul : int t -> int t -> int t

  val compile_string : string t -> string option

  val compile : 
    Instr.t list ->
    'a t -> 
    (Instr.t list * Instr.arg) In_image.t

end = struct

  module Iop2 = struct
    type t = Add | Sub | Mul
    let compile t args = match t with
      | Add -> Instr.Add args
      | Sub -> Instr.Sub args
      | Mul -> Instr.Mul args
  end

  type _ t =
  | String : string -> string t
  | Int : int -> int t
  | Read : 'a Lv.t -> 'a t
  | Bin : Iop2.t * int t * int t -> int t

  let string x = String x
  let int x = Int x
  let read lv = Read lv
  let add x y = Bin (Iop2.Add,x,y)
  let sub x y = Bin (Iop2.Sub,x,y)
  let mul x y = Bin (Iop2.Mul,x,y)

  let compile_string = function
    | String s -> Some s
    | Read _lv -> None

   let rec compile 
   : type a. Instr.t list -> a t -> (Instr.t list * Instr.arg) In_image.t =
    fun acc -> function
    | Read lv -> return (acc, Lv.compile lv)
    | String s ->      
      In_image.allocate_string s >>= fun id ->
      return (acc, Instr.String id)
    | Int i ->
      return (acc, Instr.Int i)
    | Bin (iop2,e1,e2) ->
      (* We evaluate e2 before e1 so the sub-results are 
	 on the stack in the correct order for the bin-op instruction *)
      compile acc e2 >>= fun (acc,v2) ->
      compile acc e1 >>= fun (acc,v1) ->
      let acc = Iop2.compile iop2 (v1,v2, Var.Sp) :: acc in 
      return (acc, Instr.Var Var.Sp)

end


module Action : sig 

  type 'a t
  val newline : unit t
  val print : string Exp.t -> unit t
  val print_num : int Exp.t -> unit t
  val assign : 'a Lv.t -> 'a Exp.t -> unit t
  val (-$$) : unit t -> 'a t -> 'a t

  val compile1 : unit t -> Instr.t list In_image.t

end = struct

  type _ t =
  | Newline : unit t
  | Print : string Exp.t -> unit t
  | Print_num : int Exp.t -> unit t
  | Assign : 'a Lv.t * 'a Exp.t -> unit t
  | Seq : unit t * 'a t -> 'a t

  let newline = Newline
  let print x = Print x
  let print_num x = Print_num x
  let assign lv exp = Assign (lv,exp)
  let (-$$) a b = Seq (a,b)


  let rec compile acc = function
    | Newline ->
      return (Instr.Newline :: acc)
      
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

  let compile1 t = 
    (* We accumulate instructions by pushing onto acc. 
       Then reverse at the end *) 
    compile [] t >>= fun acc ->
    let acc = Instr.Quit :: acc in
    return (List.rev acc)


end
  
let compile (uam : unit Action.t In_image.t) = 
  let dict = Assemble.Dict.create [] in (* no dictionary! *)
  let objects = Assemble.Objects.create [] in (* no objects! *)
  
  let instructions,text = 
    In_image.exec (
      uam >>= fun unit_action ->
      Action.compile1 unit_action
    ) 
  in

  let code = Assemble.Code.create instructions in

  let story = Assemble.Story.create dict objects text code in
  story 

let compile_and_assemble zversion uam = 
  Assemble.assemble zversion (compile uam)
