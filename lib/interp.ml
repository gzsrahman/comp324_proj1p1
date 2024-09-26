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

  (* LambdaLegends work on Env starts here*)

  (* we need to be able to remove existing bindings in the event
   * that we want to assign a new value to an existing variable
   * for example if x has value 2 but we need to update it to 
   * have value 3.
   *
   * we'll do this with a recursive remove function where, for a
   * variable x, we check if each binding pertains to a variable
   * of the same name. if it does, then we return the rest of the
   * list; if it doesn't, we keep the binding and check the rest.
   * in the base case where we're checking an empty environment,
   * we'll return an empty environment since there's nothing to
   * remove
   *)
   let rec remove (x : Ast.Id.t) (env : t) : t =
    match env with
    | [] -> []
    | (y, v) :: env' -> if x = y then env' else (y, v) :: remove x env'

  (* we need to be able to add bindings to the environment. in case
   * the variable in question is already bound, we should remove the
   * existing binding and then add the new one to the environment
   *
   * we'll do this by calling the remove function by default; if the
   * variable is not bound then the remove function won't do anything.
   * then we'll add the binding to env', which will definitely not
   * have an existing binding for the variable, freeing us from having
   * multiple bindings for one variable
   *)
  let add (x : Ast.Id.t) (v: Value.t) (env : t) : t =
    let env' = remove x env in
    (x, v) :: env'

  (* we need to be able to access the value for a given bound variable
   *
   * we'll just use a recursive lookup function where we look at each
   * element in the list, if the variable name is a match, we'll return
   * the corresponding value. if not, we'll look at the rest. we use
   * a base case where, in the event we are either looking at an empty
   * environment and/or we've looked through the entire list and there's
   * nothing remaining, we'll return an UnboundVariable exception since
   * the variable being looked up is not defined as far as we know
   *)
  let rec lookup (x: Ast.Id.t) (env: t) : Value.t = 
    match env with 
    | [] -> raise (UnboundVariable x)
    | (y, v) :: env' -> if x = y then v else lookup x env'

  (* LambdaLegends work for Env ends here *)

end

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
    (env : Env.t)
    (e : Ast.Expr.t) : Value.t =
  match e with
  (* LambdaLegends work starts here *)
  (* we need to be able evaluate literals; we'll return the literals with
   * type value
   *)
  | Num n -> Value.V_Int n
  | Bool b -> Value.V_Bool b
  (* we evaluate vars by looking them up in our environment and returning
   * the corresponding value
   *)
  | Var x -> Env.lookup x env
  (* we need to be able to evaluate every binary operation, including
   * mathematical operations and numerical comparisons. so we need to
   * hardcode every single operation
   *)
  | Binop (op, e1, e2) ->
    (* we need to evaluate each argument in the binary operation before
     * evaluating them in the context of the overarching program
     *)
    let v1 = eval pgm env e1 in
    let v2 = eval pgm env e2 in
    (* hardcoding every binary operation according to the appropriate op
     *)
    (match (op, v1, v2) with
      | (Plus, V_Int n1, V_Int n2) -> V_Int (n1 + n2)
      | (Minus, V_Int n1, V_Int n2) -> V_Int (n1 - n2)
      | (Times, V_Int n1, V_Int n2) -> V_Int (n1 * n2)
      | (Div, V_Int n1, V_Int n2) -> V_Int (n1 / n2)
      | (Mod, V_Int n1, V_Int n2) -> V_Int (n1 mod n2)
      | (And, V_Bool b1, V_Bool b2) -> V_Bool (b1 && b2)
      | (Or, V_Bool b1, V_Bool b2) -> V_Bool (b1 || b2)
      | (Eq, V_Int n1, V_Int n2) -> V_Bool (n1 = n2)
      | (Ne, V_Int n1, V_Int n2) -> V_Bool (n1 <> n2)
      | (Lt, V_Int n1, V_Int n2) -> V_Bool (n1 < n2)
      | (Le, V_Int n1, V_Int n2) -> V_Bool (n1 <= n2)
      | (Gt, V_Int n1, V_Int n2) -> V_Bool (n1 > n2)
      | (Ge, V_Int n1, V_Int n2) -> V_Bool (n1 >= n2)
      (* exception case incase they try to use bools for numerical
       * comparisons or ints for conjunctions/disjunctions etc
       *)
      | _ -> raise (TypeError "Invalid operands for binary operation"))
  (* need coverage of unary operations like negatives and negation
   * i forgot if the parser would recognize -true the same way as !true
   * so i added the exception case
   *)
  | Unop (op, e) ->
    let v = eval pgm env e in
    (match (op, v) with
      | (Neg, V_Int n) -> V_Int (-n)
      | (Not, V_Bool b) -> V_Bool (not b)
      | _ -> raise (TypeError "Invalid operand for unary operation"))
  (* need to be able to evaluate conditionals; i'm basically evaluating
   * the antecedant and then only evaluating the consequent or alternative
   * based on whether its true or false
   *)
  | If (e1, e2, e3) ->
    let v1 = eval pgm env e1 in
    (match v1 with
      | V_Bool true -> eval pgm env e2
      | V_Bool false -> eval pgm env e3
      | _ -> raise (TypeError "Condition in if expression must be a boolean"))
  (* we need let statement coverage; as in class we need to evaluate the
   * assignment, bind it to the variable, and then evaluate the statement
   *)
  | Let (x, e1, e2) ->
    let v1 = eval pgm env e1 in
    let env' = Env.add x v1 env in
    eval pgm env' e2
  (* and finally, we need function call coverage
   *)
  | Call (Var f, args) ->
    let func = List.find (fun (Ast.Prog.FunDef (name, _, _)) -> name = f) pgm in
    (match func with
      | Ast.Prog.FunDef (_, params, body) ->
        if List.length params <> List.length args then
          raise (TypeError "Incorrect number of arguments in function call")
        else
          let arg_vals = List.map (eval pgm env) args in
          let env' = List.fold_left2 (fun env param arg_val -> Env.add param arg_val env) env params arg_vals in
          eval pgm env' body
      | exception Not_found -> raise (UndefinedFunction f))
  | _ -> raise (TypeError "Unsupported expression")

(* exec p = v, where `v` is the result of executing `p`.
 *)
let exec (Ast.Prog.Pgm(pgm, e) : Ast.Prog.t) : Value.t =
  eval
    pgm
    Env.empty
    e

