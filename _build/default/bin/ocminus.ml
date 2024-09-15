open Ocminus

exception ParseError of string

let usage = 
  "ocminus lex e: print lexemes in expression e\n" ^
  "ocminus lexpgm f: print lexemes in program file f\n" ^
  "ocminus parse e:  parse expression e and print parse tree\n" ^
  "ocminus parsepgm f: parse program file f and print parse tree\n" ^
  "ocminus eval e:  evaluate expression e and print result\n" ^
  "ocminus exec f:  execute program file f and print result.\n"

let arg : string option ref = ref None

(* parse prsr lexbuf = the result of parsing `lexbuf` using `prsr`.
 *
 * We catch Parser.Error here and reraise its message as ParseError because
 * otherwise we need to repeat the error handling in each `*_and_show`
 * function, or alternatively, do the error handling in the main block, but
 * then we have to construct the lexer and call `parse` for each `!cmd`
 * variant, which also seems ugly.
 *)
let parse prsr lexbuf =
  try
    prsr Lexer.read_token lexbuf
  with
  | Parser.Error ->
    let pos = Lexing.lexeme_start_p lexbuf in
    raise @@ ParseError (
      Printf.sprintf
        "Parser error near line %d, character %d.\n"
        pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol)
    )

(* lex_and_show fname:  print lexemes in file `fname`, one per line, where
 * each lexeme preceded and proceeded by `.`.
 *)
let lex_and_show (fname : string) : unit =
  In_channel.with_open_text fname (fun inch ->
    let lexbuf = Lexing.from_channel inch in

    let rec show_lexemes () =
      match Lexer.read_token lexbuf with
      | Parser.EOF -> ()
      | _ ->
        print_endline(
          Printf.sprintf
            ".%s."
            (Lexing.lexeme lexbuf)
        ) ;
        show_lexemes ()
    in
    show_lexemes ()
  )

(* lex_exp_and_show s:  print lexemes in `s, one per line, where
 * each lexeme preceded and proceeded by `.`.
 *)
let lex_exp_and_show (s : string) : unit =
  let lexbuf = Lexing.from_string s in

  let rec show_lexemes () =
    match Lexer.read_token lexbuf with
    | Parser.EOF -> ()
    | _ ->
      print_endline(
        Printf.sprintf
          ".%s."
          (Lexing.lexeme lexbuf)
      ) ;
      show_lexemes ()
  in
  show_lexemes ()

(* parse_and_show fname : print a string representation of the result of
 * parsing the program in `fname` to obtain an `Ast.Prog.t` value.
 *)
let parse_and_show (fname : string) : unit =
  In_channel.with_open_text fname (fun inch ->
    inch |> Lexing.from_channel
         |> parse Parser.terminated_pgm
         |> Ast.Prog.show
         |> print_endline
  )

(* parse_exp_and_show s : print a string representation of the result of
 * parsing `s` to obtain an `Ast.Expr.t` value.
 *
 * Precondition:  `s` parses to an expression (not a program, not a program
 * file!).
 *)
let parse_exp_and_show (s : string) : unit =
  s |> Lexing.from_string
    |> parse Parser.terminated_exp
    |> Ast.Expr.show
    |> print_endline

(* eval_and_show s : print a string representation of
 * `Interp.Value.to_string(Interp.exec(Ast.Prog.Pgm([], e)))`, where `e` is
 * the result of parsing `s` as an expression.
 *
 *)
let eval_and_show (s : string) : unit =
  s |> Lexing.from_string
    |> parse Parser.terminated_exp
    |> (fun e -> Interp.exec (Ast.Prog.Pgm([], e)))
    |> Interp.Value.to_string
    |> print_endline

(* exec_and_show f : print a string representation of
 * `Interp.Value.to_string(Interp.exec p)`, where `p` is the result of
 * parsing the program file `f`.
 *)
let exec_and_show (fname : string) : unit =
  In_channel.with_open_text fname (fun inch ->
    inch |> Lexing.from_channel
         |> parse Parser.terminated_pgm
         |> Interp.exec
         |> Interp.Value.to_string
         |> print_endline
  )

let cmd : (string -> unit) option ref = ref None

let arg_parser (s : string) : unit =
    match s with
    | "lex" -> cmd := Some lex_exp_and_show
    | "lexpgm" -> cmd := Some lex_and_show
    | "parse" -> cmd := Some parse_exp_and_show
    | "parsepgm" -> cmd := Some parse_and_show
    | "eval" -> cmd := Some eval_and_show
    | "exec" -> cmd := Some exec_and_show
    | _ -> arg := Some s


let () =
  try
    Arg.parse [] arg_parser usage ;
    match (!cmd, !arg) with
    | (Some f, Some s) -> f s
    | _ -> Arg.usage [] usage
  with
  | ParseError msg -> print_endline ("Parse error: " ^ msg)
  | Interp.UnboundVariable x ->
    print_endline ("Error: variable '" ^ x ^ "' used but not declared.")
  | Interp.UndefinedFunction f ->
    print_endline 
      ("Error: undefined function '" ^ f ^ "' called but not defined.")

