(* OCaml- tests.
 *
 * N. Danner
 *
 * Tests are defined by a test specification in the OCaml- source code
 * files.  Each source code file must have a block comment of the form
 *
 *   (*!tests!
 *    *  spec
 *    *)
 *
 * I.e., the opening comment line must be exactly `(*!tests!`, followed by
 * exactly one test specification, followed by the close comment.  After the
 * first line, the leading characters are defined to be any combination of
 * whitespace and "*" characters, and the closing line must have nothing but
 * whitespace before `*)`.  When the leading characters are stripped from
 * the specification lines, the result must be a valid JSON object.
 *
 * A test specification is a JSON object with exactly one of the following
 * attributes:
 *
 *   - "output": a string list consisting of exactly one expected output
 *     value.
 *   - "exception": an exception that is expected to be raised.
 *
 * There must be exactly one `output` or `exception` attribute.
 *
 * The test is conducted as follows.  The source code file is executed by 
 * parsing the source code file and calling `Interp.exec` on the result.
 * Then:
 *
 * If there is an `output` attribute, then `Interp.exec` must return a value
 * `v` such that `Value.to_string v` is exactly the same string as the value
 * of the `output` attribute.  If they are the same, the test passes;
 * otherwise the test fails.  So, for example, consider
 *
 *   {
 *       "output":   [ "12" ]
 *   }
 *
 * The test passes if `Interp.exec` returns a value `v` such that
 * `Value.to_string v = "12"`, fails otherwise.
 *
 * If there is an `exception` attribute, then the test passes if the program
 * execution raises the given exception, and fails otherwise.  Notice that
 * the exception is specified as a string that is the same as the exception
 * type, and that any parameters to the exception constructor are ignored.
 *
 * If there is an `output` and an `exception` attribute, the test is as if
 * there were just an `exception` attribute (i.e., output is ignored).
 * Later versions of this testing framework could require that the program
 * output match the `output` attribute, then raise the indicated exception.
 *)

open OUnit2

module YJ = Yojson.Basic
module YJU = YJ.Util

(* Raised when there is an error extracting a specification from a test
 * file.
 *)
exception BadSpec of string

(* Directory in which to find team test directories.
 *)
let interp_tests_dir = "suites"

(* iotest test_code expected = a test that executes `test_code` and passes
 * if the output matches `expected`, and fails otherwise.
 *)
let iotest test_code expected =
  fun tc ->
    let actual : string =
      test_code 
        |> Ocminus.Interp.exec
        |> Ocminus.Interp.Value.to_string
    in

    assert_equal ~ctxt:tc ~printer:(fun s -> s) expected actual

(* extest test_code input expected = a test that succeeds when executing
 * `test_code` with `input` raises an exception e such that
 * Printexc_to_string e = `expected`.
 *
 * We don't use assert_raise here, because that expects an exception value,
 * which requires us to know the arguments to the constructor when the
 * exception is raised, which is not something we can rely upon.  So instead
 * we compare to the string representation of the exception, which we expect
 * to be fixed by an appropriate call to `Printexc.register_printer`.
 *)
let extest test_code expected =
  fun tc ->
    let actual : string option =
      try
        let _ = Ocminus.Interp.exec test_code in
        None
      with
      | e -> Some (Printexc.to_string e)
    in

    assert_equal 
      ~ctxt:tc 
      ~printer:(function | Some s -> s | None -> "No exception raised") 
      (Some expected) actual

(* make_test_from_spec fname spec = tf, where `tf` is a test function
 * corresponding to the test defined by `spec` in the file `fname`.
 *)
let make_test_from_spec (test_file : string) (spec : YJ.t) : test_fun =

  (* test_code = the program parsed from `fname`.
   *)
  let test_code = In_channel.with_open_text test_file (
    fun ic ->
      let lexbuf = Lexing.from_channel ic in
      try
        Ocminus.Parser.terminated_pgm Ocminus.Lexer.read_token lexbuf
      with
      | Ocminus.Parser.Error ->
        let pos = Lexing.lexeme_start_p lexbuf in
        failwith @@ Printf.sprintf
          ("Parser error in %s near line %d, character %d.\n")
          test_file
          pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol)

  ) in

  (* Are we testing against expected output or an exception?
   *)
  let keys : string list = YJU.keys spec in

  if List.exists (fun k -> k = "output") keys then
    let expected : string =
      match spec |> YJU.member "output" |> YJU.to_list |> YJU.filter_string
      with
      | [output] -> output
      | _ -> raise @@ BadSpec "Multiple outputs specified"
    in

    iotest test_code expected

  else if List.exists (fun k -> k = "exception") keys then
    let ex : string =
      spec |> YJU.member "exception" |> YJU.to_string in
    extest test_code ex
  else
    raise @@ BadSpec "No output or exception attribute"


(*  is_dir f = true,  f is the name of a directory
 *             false, o/w.
 *)
let is_dir (f : string) : bool =
  match Unix.stat f with
  | {st_kind = S_DIR; _} -> true
  | _ -> false

(* tests_from_file f = ts, where ts is a test suite with name `f`, where the
 * tests are specified as described in the module documentation.  Note that
 * for OCaml- programs, each test file specifies exactly one test, so each
 * test "suite" consists of a single test.
 *)
let tests_from_file (test_file : string) : test list =

  (* read_test_specs = ts, where ts is a list of JSON test specs read from
   * `test_file`.
   *)
  let read_test_specs () : YJ.t list =
    let inch : In_channel.t = In_channel.open_text test_file in

    let spec_start : string = "(*!tests!" in
    let spec_leader_regexp : Str.regexp = Str.regexp {|^\([ \t]\|\*\)*|} in

    (* Read from `inch` until we find the start of the specifications.
     * The start is indicated by a line that begins with `spec_start` and
     * `inch` will be positioned at the line following the first line that
     * starts with `spec_start`.
     *)
    let rec find_spec_start () =
      match In_channel.input_line inch with
      | None ->
        raise @@ BadSpec "No specs found"
      | Some s ->
        if String.starts_with ~prefix:spec_start s then ()
        else find_spec_start ()
    in

    (* read_specs () = the list of lines that are test specifications.
     *)
    let rec read_specs () : string list =
      match In_channel.input_line inch with
      | None -> raise @@ BadSpec "Unterminated spec comment"
      | Some s ->
        if String.trim s = "*)" then []
        else if not (Str.string_match spec_leader_regexp s 0)
             then raise @@ BadSpec ("Bad spec line: " ^ s)
        else Str.replace_first spec_leader_regexp "" s :: read_specs ()
    in

    try
      find_spec_start() ;
      read_specs () |> String.concat "\n" |> YJ.seq_from_string |> List.of_seq 
    with
    | BadSpec msg -> 
      Printf.eprintf "Bad test spec in %s: %s\n" test_file msg ; []

  in

    try
      (* specs = the test specifications.
       *)
      let specs = read_test_specs() in

      List.mapi
        (
          fun n s ->
            try
              Int.to_string n >:: make_test_from_spec test_file s
            with
            | BadSpec msg ->
              raise @@ BadSpec (
                Printf.sprintf "%s(%d): %s" test_file n msg
              )
        )
        specs
    with
    | Yojson.Json_error s ->
      raise @@ BadSpec (
        Printf.sprintf "%s: JSON: %s" test_file s
      )
    | Yojson.Basic.Util.Type_error (s, _) -> 
      raise @@ BadSpec (
        Printf.sprintf "%s: JSON type error: %s" test_file s
      )

let () =
  try

    (* Define the strings by which to identify exceptions that are raised.
     *)
    Printexc.register_printer (
      function
      | Ocminus.Interp.UnboundVariable _ -> Some "UnboundVariable"
      | Ocminus.Interp.UndefinedFunction _ -> Some "UndefinedFunction"
      | Ocminus.Interp.TypeError _ -> Some "TypeError"
      | _ -> None
    ) ;

    (* test_file suite_dir = the list of files in `suite_dir` with suffix
     * `.ml.
     *
     * We sort in reverse order because the tests seem to be run in the
     * reverse order from this list, so this way the tests are executed in
     * alphabetical order.
     *)
    let test_files (suite_dir : string) : string list =
      List.filter
        (fun f -> Filename.check_suffix f "ml")
        (Sys.readdir suite_dir |> Array.to_list)
      |> List.sort Stdlib.compare
    in

    (* suite_dirs = directories that contain test files.
     *)
    let suite_dirs : string list =
      Sys.readdir interp_tests_dir 
      |> Array.to_list 
      |> List.map (Filename.concat interp_tests_dir)
      |> List.filter is_dir 
      |> List.sort Stdlib.compare
    in

    (* suites = the test suites, one per directory in `suite_dirs`.
     *)
    let suites : test list =
      List.map (
        fun suite_dir ->
          suite_dir >:::
            List.map
              (
                fun test_file ->
                  test_file >::: (
                    tests_from_file (
                      Filename.concat suite_dir test_file
                    )
                  )
              )
              (test_files suite_dir)
      ) suite_dirs
    in

  (*  show_and_run s:  If `s` : `OUnitTest.TestLabel` (like a named test
   *  suite), print `s` and the run the tests in `s`.  Otherwise just run
   *  `s`.
   *)
  let show_and_run (s : test) : unit =
    match s with
    | OUnitTest.TestLabel (name, s') ->
      print_endline "" ;
      print_endline @@ "=====" ^ name ^ "=====" ;
      run_test_tt_main s' ;
      print_endline ""
    | _ -> 
      run_test_tt_main s ;
  in

  List.iter show_and_run suites

  with
  | BadSpec msg ->
    Printf.eprintf "%s" msg

