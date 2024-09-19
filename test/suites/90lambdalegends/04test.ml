(*!tests!
*
* {"output": ["9"]}
*
*)

let rec f x y = x + y
and g x = f x (2*x) ;;
g 3 ;;