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

(* LambdaLegends error addition
 *)
(* DivisionByZero is raised when in Div e_1 e_2 e_2 = 0
 *)
exception DivisionByZero

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

  (* LambdaLegends work on Env starts here*)

  (* recursively iterate through rho list to remove old binding and keep rest
   * necessary when updating binding for existing variable
   *)
  let rec remove (rho : t) (x : Ast.Id.t) : t =
    match rho with
      | [] -> []
      | (y, v) :: rho' -> if x = y then rho' else (y, v) :: remove rho' x

  (* add new variable binding to head. first remove existing binding if
   * applicable, then add binding to the new environment
   *)
  let add (rho : t) (x : Ast.Id.t) (v: Value.t) : t =
    let rho' = remove rho x in
    (x, v) :: rho'

  (* recursively iterate through list to find binding for input variable
   * raise error if variable not bound
   *)
  let rec lookup (rho: t) (x: Ast.Id.t) : Value.t = 
    match rho with 
      | [] -> raise (UnboundVariable x)
      | (y, v) :: rho' -> if x = y then v else lookup rho' x

  (* LambdaLegends work for Env ends here *)

end

(* perform unary operation on a value; V_Ints can be negative and V_Bools
 * can be logically negated
 *)
let unop (op : Ast.Expr.unop) (v : Value.t) : Value.t =
  match (op,v) with 
    |(Ast.Expr.Neg, Value.V_Int n) -> Value.V_Int(-n)
    |(Ast.Expr.Not, Value.V_Bool b) -> Value.V_Bool(not b)
    |_ -> raise (TypeError "Can't perform this operation on this type")

(* perform binary operation on two values v and v' based on whether they
 * are V_Ints or V_Bools; this should cover every possible operation
 *)
let binop (op : Ast.Expr.binop) (v : Value.t) (v' : Value.t) : Value.t =
  match (op, v, v') with
    | (Ast.Expr.Plus, Value.V_Int n1, Value.V_Int n2) -> Value.V_Int (n1 + n2)
    | (Ast.Expr.Minus, Value.V_Int n1, Value.V_Int n2) -> Value.V_Int (n1 - n2)
    | (Ast.Expr.Times, Value.V_Int n1, Value.V_Int n2) -> Value.V_Int (n1 * n2)
    | (Ast.Expr.Div, Value.V_Int n1, Value.V_Int n2) -> 
      if n2 = 0 then raise DivisionByZero
      else Value.V_Int (n1 / n2)
    | (Ast.Expr.Mod, Value.V_Int n1, Value.V_Int n2) ->
      if n2 = 0 then raise DivisionByZero
      else Value.V_Int (n1 mod n2)
    | (Ast.Expr.And, Value.V_Bool b1, Value.V_Bool b2) -> Value.V_Bool (b1 && b2)
    | (Ast.Expr.Or, Value.V_Bool b1, Value.V_Bool b2) -> Value.V_Bool (b1 || b2)
    | (Ast.Expr.Eq, Value.V_Int n1, Value.V_Int n2) -> Value.V_Bool (n1 = n2)
    | (Ast.Expr.Ne, Value.V_Int n1, Value.V_Int n2) -> Value.V_Bool (n1 <> n2)
    | (Ast.Expr.Eq, Value.V_Bool b1, Value.V_Bool b2) -> Value.V_Bool (b1 = b2)
    | (Ast.Expr.Ne, Value.V_Bool b1, Value.V_Bool b2) -> Value.V_Bool (b1 <> b2)
    | (Ast.Expr.Lt, Value.V_Int n1, Value.V_Int n2) -> Value.V_Bool (n1 < n2)
    | (Ast.Expr.Le, Value.V_Int n1, Value.V_Int n2) -> Value.V_Bool (n1 <= n2)
    | (Ast.Expr.Gt, Value.V_Int n1, Value.V_Int n2) -> Value.V_Bool (n1 > n2)
    | (Ast.Expr.Ge, Value.V_Int n1, Value.V_Int n2) -> Value.V_Bool (n1 >= n2)
    | _ -> raise (TypeError "Invalid operands for binary operation")


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
    (pgm : Ast.Prog.fundef list)
    (rho : Env.t)
    (e : Ast.Expr.t) : Value.t =
  match e with

  (* LambdaLegends work starts here *)

  (* return literals as values *)
  | Ast.Expr.Num n -> Value.V_Int n
  | Ast.Expr.Bool b -> Value.V_Bool b

  (* return values corresponding to vars *)
  | Ast.Expr.Var x -> Env.lookup rho x

  (* for binops evaluate arguments then perform operation *)
  | Ast.Expr.Binop (op, e1, e2) ->
    let v1 = eval pgm rho e1 in
    let v2 = eval pgm rho e2 in
    binop op v1 v2

  (* for unops evaluate argument then perform operation *)
  | Ast.Expr.Unop (op, e) ->
    let v = eval pgm rho e in 
    unop op v 

  (* for conditionals, evaluate the antecedent then evaluate and return
   * the consequent or the alternative accordingly
   *
   * did not modularize because we would have to evaluate all args before
   * calling a helper function and functional languages only evaluate
   * the result of conditionals after evaluating the antecedent *)
  | Ast.Expr.If (e1, e2, e3) ->
    let v1 = eval pgm rho e1 in
    (match v1 with
      | Value.V_Bool true -> eval pgm rho e2
      | Value.V_Bool false -> eval pgm rho e3
      | _ -> raise (TypeError "Condition in if expression must be a boolean"))
  
  (* in a let statement, we evaluate the assignment for the variable and then
   * the final statement; did not modularize because unnecessary here
   *)
  | Ast.Expr.Let (x, e1, e2) ->
    let v1 = eval pgm rho e1 in
    let rho' = Env.add rho x v1 in
    eval pgm rho' e2

  (* for function calls, we see if the function is defined in the program; if
   * not we throw an error. we see if the number of parameters matches the
   * number of arguments; if not, error. finally, we evaluate all of the args
   * by mapping the eval function to the list of arguments. then, we create a 
   * new environment rho' where we only have the bindings for parameter to the
   * value of its corresponding argument. we evaluate under rho'.
   *)
  | Ast.Expr.Call (Ast.Expr.Var f, args) ->
    let func = 
      try 
        List.find (fun (Ast.Prog.FunDef (name, _, _)) -> name = f) pgm
      with Not_found -> raise (UndefinedFunction f)
    in
    (match func with
      | Ast.Prog.FunDef (_, params, body) ->
        if List.length params <> List.length args then
          raise (TypeError "Incorrect number of arguments in function call")
        else
          let arg_vals = List.map (eval pgm rho) args in
          (* List.fold_left2 basically iterates through two lists, param and
           * arg_val, and cumulatively adds them to rho' by binding each
           * param to the value of the corresponding argument. confusing, ik *)
          let rho' = List.fold_left2 (fun rho param arg_val -> Env.add rho param arg_val) rho params arg_vals in
          eval pgm rho' body)
    
  (* we should have defined ever possible eval case by now so if none of the 
   * cases match, throw an error *)
  | _ -> raise (TypeError "Unsupported expression")

  (* LambdaLegends work ends here*)

(* exec p = v, where `v` is the result of executing `p`.
 *)
let exec (Ast.Prog.Pgm(pgm, e) : Ast.Prog.t) : Value.t =
  eval
    pgm
    Env.empty
    e

