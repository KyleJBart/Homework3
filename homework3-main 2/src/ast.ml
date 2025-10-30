type bop = 
  | Add
  | Mult

type expr = 
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr
