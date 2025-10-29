# Homework 3: Extend CalcPL (without Types)

**This is an individual assignment. You must work on this homework alone.**


## Introduction
The goal of this homework is to get you familiar with developing a programming language in OCaml. 
In this homework, you will extend the CalcPL initial version. You need to add the following. 

- A new data type `bool`,
- Less-than-equals (`LEQ` or `<=`) operator
- Support for defining variables with `let` as we can define in OCaml
- Support for `if-then-else`
- Support for errors (e.g., `1 + true ` is not allowed). 

## Testing & Submitting
To test locally, run `make test` from the homework directory. We recommend you write student tests in `test/main.ml`.

You can interactively test your code by doing `make utop` (assuming you have utop). Then you should be able to use any of the functions. All of your commands in utop need to end with two semicolons (i.e. ;;), otherwise it will appear that your terminal is hanging.

## Test cases 

For `bool` types, we expect to pass the following unit tests.

```ocaml
make_b "true" true "true";
make_b "false" false "false";
```

For less-than-equals, we expect the following behavior. 

```ocaml
make_b "leq" true "1<=1";
make_b "leq1" false "2<=1";
```

For `let` syntax, see the following case.

```ocaml
make_i "let" 22 "let x=22 in x";
```

In the case of conditions, the syntax will be like the following. 

```ocaml
make_i "if1" 22 "if true then 22 else 0";
```

Our language should not allow to perform invalid operations. For example, you should not be allowed add integer and boolean. 

```ocaml
make_t "invalid plus" bop_err "1 + true";
make_t "invalid leq" bop_err "true <= 1";
make_t "invalid guard" if_guard_err "if 1 then 2 else 3";
make_t "unbound" unbound_var_err "x";
```

Finally, calcPL should be able to mix and match all the defined operators. Your code should pass all the following tests.
```ocaml
make_i "int" 22 "22";
make_i "add" 22 "11+11";
make_i "adds" 22 "(10+1)+(5+6)";
make_i "let" 22 "let x=22 in x";
make_i "lets" 22 "let x = 0 in let x = 22 in x";
make_i "mul1" 22 "2*11";
make_i "mul2" 22 "2+2*10";
make_i "mul3" 14 "2*2+10";
make_i "mul4" 40 "2*2*10";
make_i "if1" 22 "if true then 22 else 0";
make_b "true" true "true";
make_b "leq" true "1<=1";
make_i "if2" 22 "if 1+2 <= 3+4 then 22 else 0";
make_i "if3" 22 "if 1+2 <= 3*4 then let x = 22 in x else 0";
make_i "letif" 22 "let x = 1+2 <= 3*4 in if x then 22 else 0";
make_t "invalid plus" bop_err "1 + true";
make_t "invalid mult" bop_err "1 * false";
make_t "invalid leq" bop_err "true <= 1";
make_t "invalid guard" if_guard_err "if 1 then 2 else 3";
make_t "unbound" unbound_var_err "x";
```

## Pointers



The names of variables can be alphabetical, hence, the following regex should come in handy (in `lexer.mll`).

```text
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
```
The test cases are built on the following operators of calcPL.

Here is the rule for the lexer:

```text
rule read =
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "<=" { LEQ }
  | "*" { TIMES }
  | "+" { PLUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
```

To add new operators, you will need to modify `ast.ml`:

```ocaml
type bop =
  ... 
  | Leq

type expr =
  ... 
  | Var of string
  | Bool of bool
  | Let of string * expr * expr
  | If of expr * expr * expr
```

Additional lexical *tokens* of our language can be (changes in `parser.mly`). 

```text
...
%token <string> ID
%token TRUE
%token FALSE
%token LEQ
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
```
Also, operators should have the following precedence over each other.

```text
%nonassoc IN
%nonassoc ELSE
%left LEQ
%left PLUS
%left TIMES
```

CalcPL has new productions for all the new expressions. 

```text
expr:
  ...
  | x = ID { Var x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | LPAREN; e=expr; RPAREN {e}
  ;
```
Use the following tips to modify `main.ml`

```ocaml
let string_of_val (e : expr) : string =
  match  e with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | _ -> failwith "precondition violated"
```

```ocaml
let is_value : expr -> bool = function 
  | Int _ | Bool _ -> true
  | Var _ | Let _ | Binop _ | If _ -> false
```

Declared variables should substitute for their values. This new function will come in handy for that.

```ocaml
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
```
You will also need to modify `step` and `step_bop` to support for new features. 

```ocaml
let rec step : expr -> expr = function
  | Int _ | Bool _ -> failwith "Does not step"
  | Var _ -> failwith unbound_var_err
  ...
  
  | Let (x, e1, e2) when is_value e1 -> subst e2 e1 x
  | Let (x, e1, e2) -> Let (x, step e1, e2)
  | If (Bool true, e2, _) -> e2
  | If (Bool false, _, e3) -> e3
  | If (Int _, _, _) -> failwith if_guard_err
  | If (e1, e2, e3) -> If (step e1, e2, e3)

and step_bop bop e1 e2 = match bop, e1, e2 with
  ...
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> failwith bop_err
```

**Good luck!**
