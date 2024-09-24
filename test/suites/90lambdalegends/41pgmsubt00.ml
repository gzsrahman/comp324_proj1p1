(*!tests!
*
* {"output": ["0"]}
*
*)

let rec f x y = x - y
and g x = f x x ;;
g 3 ;;