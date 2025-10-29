open Ast

let bop_err = "bop_err"
let if_guard_err = "if_guard_err"
let unbound_var_err = "unbound_var_err"

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let string_of_val (e : expr) : string =
  match e with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | _ -> failwith "precondition violated"

let is_value : expr -> bool = function 
  | Int _ | Bool _ -> true
  | Var _ | Let _ | Binop _ | If _ -> false

let rec subst e v x = match e with
  | Var y -> if x = y then v else e
  | Bool _ -> e
  | Int _ -> e
  | Binop (bop, e1, e2) -> Binop (bop, subst e1 v x, subst e2 v x)
  | Let (y, e1, e2) ->
    let e1' = subst e1 v x in
    if x = y
    then Let (y, e1', e2)
    else Let (y, e1', subst e2 v x)
  | If (e1, e2, e3) -> 
    If (subst e1 v x, subst e2 v x, subst e3 v x)

let rec step : expr -> expr = function
  | Int _ | Bool _ -> failwith "Does not step"
  | Var _ -> failwith unbound_var_err
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 -> Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop (bop, step e1, e2)
  | Let (x, e1, e2) when is_value e1 -> subst e2 e1 x
  | Let (x, e1, e2) -> Let (x, step e1, e2)
  | If (Bool true, e2, _) -> e2
  | If (Bool false, _, e3) -> e3
  | If (Int _, _, _) -> failwith if_guard_err
  | If (e1, e2, e3) -> If (step e1, e2, e3)

and step_bop bop e1 e2 = match bop, e1, e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | Add, Int _, Bool _ | Add, Bool _, Int _ | Add, Bool _, Bool _ -> failwith bop_err
  | Mult, Int _, Bool _ | Mult, Bool _, Int _ | Mult, Bool _, Bool _ -> failwith bop_err
  | Leq, Int _, Bool _ | Leq, Bool _, Int _ | Leq, Bool _, Bool _ -> failwith bop_err

let rec eval (e : expr) : expr =
  if is_value e then e
  else eval (step e)

(* This is the function the OUnit tests expect *)
let interp (s : string) : expr =
  let e = parse s in
  eval e

(* Keep the string interpreter for manual testing *)
let interp_string (s : string) : string =
  try
    let e = parse s in
    let v = eval e in
    string_of_val v
  with
  | Failure msg when msg = bop_err -> bop_err
  | Failure msg when msg = if_guard_err -> if_guard_err
  | Failure msg when msg = unbound_var_err -> unbound_var_err
  | _ -> "other error"

(* Manual test function *)
let run_manual_tests () =
  Printf.printf "\n=== Running Manual Tests ===\n";
  
  let test_cases = [
    ("22", "22");
    ("11+11", "22");
    ("let x=22 in x", "22");
    ("if true then 22 else 0", "22");
    ("true", "true");
    ("1<=1", "true");
    ("1 + true", bop_err);
  ] in
  
  List.iter (fun (input, expected) ->
    let result = interp_string input in
    if result = expected then
      Printf.printf "PASS: %s = %s\n" input result
    else
      Printf.printf "FAIL: %s = %s (expected %s)\n" input result expected
  ) test_cases;
  
  Printf.printf "=== Manual Tests Complete ===\n"

let () =
  run_manual_tests ()