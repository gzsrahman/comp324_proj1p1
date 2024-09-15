(* Failure functions.
 *
 * N. Danner
 *)

let unimplemented (s : string) =
  failwith @@
  Printf.sprintf "Unimplemented: %s" s

let impossible (s : string) =
  failwith @@
  Printf.sprintf "Imposssible: %s" s
