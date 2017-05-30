open Core
open Z_machine.Numbers

type arg = 
| Var of Var.t
| Byte of Byte.t
| Word of Word.t
| Target of Var.t
| String of Text.t
| Int of int
[@@deriving sexp_of]

type t =
| Store of arg * arg
| Print of string
| Print_paddr of arg
| Newline
| Quit 
| Sread of arg * arg
| Print_num of arg
| Add of (arg * arg * Var.t)
| Sub of (arg * arg * Var.t)
| Mul of (arg * arg * Var.t)
 [@@deriving sexp_of]