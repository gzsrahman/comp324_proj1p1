(* Ocaml- interpreter.
 *
 * N. Danner
 *)

(* UndefinedFunction f is raised when f is called but not defined.
 *)
exception UndefinedFunction of Ast.Id.t

(* UnboundVariable x is raised when x is used but not declared.
 *)
exception UnboundVariable of Ast.Id.t

(* TypeError s is raised when an operator or function is applied to operands
 * of the incorrect type.  s is any (hopefuly useful) message.
 *)
exception TypeError of string

(* Values.
 *)
module Value = struct
  type t = 
    | V_Int of int
    | V_Bool of bool
    [@@deriving show]

  (* to_string v = a string representation of v (more human-readable than
   * `show`.
   *)
  let to_string (v : t) : string =
    match v with
    | V_Int n -> Int.to_string n
    | V_Bool b -> Bool.to_string b
end

(* Environments.  You must have a module for environments.  All operations
 * involving environments must be encapsulated in this module.
 *
 * I've provided a bare skeleton that defines the concrete type to be a list
 * of (Id.t*Value.t) pairs (i.e., an association list), but a `Map.S`
 * (backed by a balanced binary search tree) would be much more efficient.
 * If you do use a list of pairs, be sure to read the `List`
 * documentation---don't reinvent the wheel!
 *)
module Env = struct
  type t = (Ast.Id.t * Value.t) list

  (* empty = the empty environment.
   *)
  let empty : t = []

  (* lookup ρ x = ρ(x) *)
  let lookup (rho : t) (x : Ast.Id.t) : Value.t = 
      List.assoc x rho

  (* update ρ x v = ρ{x -> v}*)
  let update (rho : t) (x : Ast.Id.t) (v : Value.t) : t =
    (x,v) :: List.remove_assoc x rho
end

(* unop op v = v' where v' is the result of applying the semantic 
* denotation of 'op' to 'v'
*)
let unop (op : Ast.Expr.unop) (v : Value.t) : Value.t =
  match (op,v) with 
    |(Ast.Expr.Neg, Value.V_Int n) -> Value.V_Int(-n)
    |(Ast.Expr.Not, Value.V_Bool b) -> Value.V_Bool(not b)
    |_ -> raise (TypeError "Can't perform this operation on this type")

(* binop op v v' = v'' where v'' is the result of applying the semantic
 *  denotation of `op` to `v` and `v''`.
 *)
let binop (op : Ast.Expr.binop) (v : Value.t) (v' : Value.t) : Value.t = 
  match (op, v, v') with 
  |(Ast.Expr.Plus, Value.V_Int n, Value.V_Int n') -> Value.V_Int(n + n')
  |(Ast.Expr.Minus, Value.V_Int n, Value.V_Int n' ) -> Value.V_Int(n - n')
  |(Ast.Expr.Times, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n * n')
  |(Ast.Expr.Div, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n / n')
  |(Ast.Expr.Mod, Value.V_Int n, Value.V_Int n') -> Value.V_Int(n mod n')
  |(Ast.Expr.And, Value.V_Bool b, Value.V_Bool b') -> Value.V_Bool ( b && b')
  |(Ast.Expr.Or, Value.V_Bool b, Value.V_Bool b') -> Value.V_Bool (b || b')
  |(Ast.Expr.Eq, v, v') -> Value.V_Bool (v == v')
  |(Ast.Expr.Ne, v, v') -> Value.V_Bool (v != v')
  |(Ast.Expr.Lt, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n < n')
  |(Ast.Expr.Le, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n <= n')
  |(Ast.Expr.Gt, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n > n')
  |(Ast.Expr.Ge, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n >= n')
  |_ -> raise (TypeError "You can't perform this operation on this type/types")


(* If v v0 v1 = v2 where v2 is the result of the condition v being true or not,
 * and then evaluating to either v0 or v1 depending on if it's true
 *)
let ifs (v : Value.t) (v0 : Value.t) (v1 : Value.t) : Value.t =
  match (v, v0, v1) with
  |(Value.V_Bool b, Value.t tr, Value.t fa) -> 
    if b == true then tr
    else fa
  |_ -> raise (TypeError "You can't perform an if statement with these types")


(* eval pgm ρ e = v, where pgm, ρ ├ e ↓ v.
 *
 * I have provided this as a bit of starter code, especially to show the
 * branch for `Call` expressions for the core project, which assumes the
 * operator argument is a variable, and to give an example of using the
 * functions in `Failures` for "placeholders."  You can change this function
 * however you like, or even delete it altogether, because all testing will
 * be done via `exec`.  But you will need something like it.  
 *
 * Don't forget that you need to declare recursive functions with `let rec`
 * and non-recursive functions with `let`; our compiler settings are very
 * strict, and report a non-recursive function declared with `let rec` as an
 * error.
 *)
let rec eval
    (funs: Ast.Prog.fundef list)
    (rho : Env.t)
    (e : Ast.Expr.t) : Value.t =
  match e with
  |Ast.Expr.Var x -> Env.lookup rho x
  |Ast.Expr.Num n -> Value.V_Int n
  |Ast.Expr.Bool b -> Value.V_Bool b
  |Ast.Expr.Unop (op, e) -> 
    let v = eval funs rho e in 
    unop op v 
  |Ast.Expr.Binop (op, e, e') -> 
    let v = eval funs rho e in
    let v' = eval funs rho e' in 
    binop op v v'
  |Ast.Expr.If (e, e0, e1) -> 
    let v = eval funs rho e in
    let v0 = eval funs rho e0 in
    let v1 = eval funs rho e1 in 
    ifs v v0 v1
  |Ast.Expr.Let (x, e0, e1) -> Failures.unimplemented (
    Printf.sprintf "eval: %s" (Ast.Expr.show e))
  |Ast.Expr.Call(Var f, xxs) -> Failures.unimplemented (
    Printf.sprintf "eval: %s" (Ast.Expr.show e))
  | _ -> 
    Failures.unimplemented (
      Printf.sprintf "eval: %s" (Ast.Expr.show e)
    )

(* exec p = v, where `v` is the result of executing `p`.
 *)
let exec (Ast.Prog.Pgm(pgm, e) : Ast.Prog.t) : Value.t =
  eval
    pgm
    Env.empty
    e

