(*!tests!
*
* {"exception": "too many arguments given"}
*
*)

let rec f x y = x + y ;;

f 5 6 7 ;;