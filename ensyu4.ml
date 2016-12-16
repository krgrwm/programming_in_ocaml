let ($) f g x = f (g x) ;;

let curry f x y = f(x, y) ;;

let average (x, y) = (x +. y) /. 2.0 ;;
let curried_avg = curry average;;
curried_avg 3.0 3.2;;

(* 4.1 *)
let uncurry f (x,y) = f x y;;

let avg = uncurry curried_avg in
avg (3.0, 3.2 ) ;;

(* 4.2 *)
let rec repeat f n x =
  if n > 0 then repeat f (n-1) (f x) else x ;;

repeat sqrt 2 2.0 ;;

let fib n =
  let (fibn, _) =
    repeat (fun (x,y) -> (x+y, x)) n (0, 1)
  in fibn
;;

fib 6;;

(* 4.3 *)
let id x = x ;;

let rec funny f n =
  if n == 0 then id
  else if n mod 2 = 0 then funny (f $ f) (n / 2)
  else funny (f $ f) (n / 2 ) $ f
;;

(funny sqrt 5) 4.0 ;;

(funny sqrt 3) 2.0 ;;

(*
 * 説明
 * funny func n: funcをn回合成した関数を返す．
 * nが偶数(n=2m)の場合
 * fを2m回合成 -> f$fをm回合成
 * なのでfunnyにf$fとmを渡し，再帰する
 * nが奇数(n=2m+1)の場合
 * fを2m+1回合成 -> f$fをm回合成したものにfを合成
 * なのでfunnyにf$fとmを渡し再帰させ，結果にfを合成
 * ( (2m+1) / 2 は小数点以下が切り捨てられmとなる )
 *)

(* 4.4 *)
let k x y = x;;
let s x y z = x z (y z) ;;
s k k 1 ;;
(*
 * s k k 1 = k 1 (k 1)
 * kは第2引数にかかわらず第一引数を返すため
 * 常に1を返す．
 * よって恒等関数となる．
 *)

let test x y = x + y ;;

(* 4.5 *)
let twice f x = f (f x) ;;
(*
 * twice twice f x 
 * = (twice twice f) x
 * = (twice (twice f)) x
 * = twice (twice f) x
 * = (twice f) ((twice f) x)
 * = (twice f) (f (f x))
 * = f (f (f (f x)))
 *)

(* 4.6 *)
k (s k k) 1 2;;
(*
 * k (s k k) x y 
 * = k I x y
 * = (k I x) y
 * = I y
 * = y
 *)
