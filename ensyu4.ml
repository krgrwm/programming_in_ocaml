let deriv f = 
  let dx = 0.1e-10 in
  fun x -> (f (x+.dx) -. f(x) ) /. dx
;;

let fixpoint f init =
  let threshold = 0.1e-10 in
  let rec loop x =
    let next = f x in
    if abs_float (x -. next) < threshold then x else loop next
  in loop init
;;

let newton_transform f = fun x-> x -. f(x) /. (deriv f x) ;;

let newton_method f guess =
  fixpoint (newton_transform f) guess;;

let square_root a = newton_method (fun x -> x*. x -. a) 1.0;;
square_root 5.0 ;;

let ($) f g x = f (g x) ;;

let f = (~-.) $ sqrt in f 2.0 ;;



let curry f x y = f(x, y) ;;
let average (x, y) = (x +. y) /. 2.0 ;;

let curried_avg = curry average;;


let uncurry f (x,y) = f x y;;

let avg = uncurry curried_avg in
avg (4.0, 5.3 ) ;;

let rec repeat f n x =
  if n > 0 then repeat f (n-1) (f x) else x ;;

repeat sqrt 2 2.0 ;;

let id x = x ;;

let rec funny f n =
  if n == 0 then id
  else if n mod 2 = 0 then funny (f $ f) (n / 2)
  else funny (f $ f) (n / 2 ) $ f
;;

(funny sqrt 5) 4.0 ;;

(funny sqrt 3) 2.0 ;;

