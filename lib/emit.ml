open! Core
open Z_machine.Numbers

let (++) = Loc.(+)

module Mark : sig  (* relocatable location *)
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
type mark = Mark.t

type tree =
| Nothing 
| Seq of tree * tree
| Bytes of Byte.t list
| Define of Mark.t
| Location of Mark.t
| Packed_address of Zversion.t * Mark.t
| Align2

let tree_size : tree -> int =
  let rec walk = function
    | Nothing -> 0
    | Seq (tree1,tree2) -> walk tree1 + walk tree2
    | Bytes bs -> List.length bs
    | Define _ -> 0
    | Location _ -> 2
    | Packed_address _ -> 2
    | Align2 -> failwith "tree_size,Align2" (* sad, but what else? *)
  in
  walk
  
let pass1 ~(start:Loc.t) : tree -> (Mark.t * Loc.t) list =
  let rec collect loc acc = function
    | Nothing -> acc,loc
    | Seq (tree1,tree2) ->
      let acc,loc = collect loc acc tree1 in 
      collect loc acc tree2
    | Bytes bs -> acc, loc ++ List.length bs
    | Define d -> (d,loc) :: acc, loc
    | Location _ -> acc, loc ++ 2
    | Packed_address _ -> acc, loc ++ 2
    | Align2 -> 
      if (Loc.to_int loc % 2 = 1)
      then acc, loc ++ 1
      else acc, loc
  in
  fun tree ->
    let pairs,_ = collect start [] tree in
    pairs
  
let pass2 ~(start:Loc.t) ~(locate: mark -> Loc.t) : tree -> Byte.t list =
  let rec flatten loc acc = function
    | Nothing -> acc, loc 
    | Seq (tree1,tree2) -> 
      let acc,loc = flatten loc acc tree1 in 
      flatten loc acc tree2

    | Bytes bs -> List.rev bs @ acc, loc ++ List.length bs

    | Define _ -> acc, loc

    | Location mark -> 
      let h,l = Word.to_high_low (Loc.to_word (locate mark)) in 
      List.rev [h;l] @ acc, loc ++ 2

    | Packed_address (zversion,mark) -> 
      let w = Loc.to_packed_address zversion (locate mark) in
      let h,l = Word.to_high_low w  in 
      List.rev [h;l] @ acc, loc ++ 2

    | Align2 ->
      if (Loc.to_int loc % 2 = 1)
      then [Byte.zero] @ acc, loc ++ 1
      else acc, loc

  in
  fun tree ->
    let bytes,_ = flatten start [] tree in
    List.rev bytes

type 'a t = T of (Mark.t -> Mark.t * tree * 'a)
let deT (T x) = x

let exec t ~start = 
  let _,tree,a = deT t Mark.zero in
  let pairs = pass1 ~start tree in
  let map = Mark.Map.of_alist_exn pairs in
  let locate mark = Map.find_exn map mark in
  let bytes = pass2 ~start ~locate tree in
  a,bytes,locate

let return x  = 
  T (fun m -> m, Nothing, x)

let (>>=) t f = 
  T (fun m ->
    let m,tree1,x = deT t m in
    let m,tree2,y = deT (f x) m in
    m,Seq(tree1,tree2),y)

let reverse_bind ~first_f ~second = 
  T (fun m ->
    (* run second before first...*)
    let m,tree2,x = deT second m in
    let m,tree1,y = deT (first_f ~backwards_flowing_info:x) m in
    m,Seq(tree1,tree2),y) (* ...then flip the order of the sub-trees *)

let nothing =
  T (fun m -> m, Nothing, ())

let bytes xs =
  T (fun m -> m, Bytes xs, ())

let here = 
  T (fun m -> Mark.inc m, Define m, m)

let loc mark = 
  T (fun m -> m, Location mark, ())

let packed_address zversion mark = 
  T (fun m -> m, Packed_address (zversion,mark), ())

let size t =
  T (fun m ->
    let m,tree,x = deT t m in
    let n = tree_size tree in
    m,tree,(x,n))

let align2 =
  T (fun m -> m, Align2, ())

let byte b = bytes [b]
let word w = let h,l = Word.to_high_low w in bytes [h;l]

let rec seq = function
  | [] -> return ()
  | t::ts -> t >>= fun () -> seq ts

let assert_size ?(tag="") (expected:int) t = 
  size t >>= fun (v,actual) ->
  if (Int.(=) actual expected) then return v
  else 
    failwithf "assert_size(%s) failed: actual=%d, expected=%d"
      tag actual expected ()
