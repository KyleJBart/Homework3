type bop =
  | Add
  | Mult
  | Leq

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Binop of bop * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr

let string_of_bop = function
  | Add -> "+"
  | Mult -> "*" 
  | Leq -> "<="