open Core
open Z_machine.Numbers

type label = string
[@@deriving sexp, compare]

module Arg = struct
  type arg = 
  | Var of Var.t
  | Byte of Byte.t
  | Word of Word.t
  | Target of Var.t
  | String of Text.id
  | Int of int
  | Buffer of string
  | Dict_entry of Dict.word
  | Mark of Emit.mark
  | Lab of label
  [@@deriving sexp_of]
end
open Arg

type t =
| Load_word of arg * arg * Var.t
| Store of arg * arg
| Store_byte of (arg * arg * arg)
| Push of arg
| Print of string
| Print_addr of arg
| Print_paddr of arg
| Print_obj of arg
| Newline
| Quit 
| Sread of arg * arg
| Print_num of arg
| Add of (arg * arg * Var.t)
| Sub of (arg * arg * Var.t)
| Mul of (arg * arg * Var.t)
| Div of (arg * arg * Var.t)
| Mod of (arg * arg * Var.t)
| Label of label
| Jump_eq of (arg * arg * label)
| Jump_neq of (arg * arg * label)
| Jump_leq of (arg * arg * label)
| Jump_geq of (arg * arg * label)
| Jump of arg
| Call0 of (arg * Var.t)
[@@deriving sexp_of]
