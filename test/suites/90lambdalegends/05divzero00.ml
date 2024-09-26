(*!tests!
*
* {"exception": "Ocminus.Interp.DivisionByZero"}
*
*)

let rec f x y = x / y ;;
f 5 0 ;;