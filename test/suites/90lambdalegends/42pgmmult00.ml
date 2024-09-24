(*!tests!
*
* {"output": ["9"]}
*
*)

let rec f x y = x * y
and g x = f x x ;;
g 3 ;;