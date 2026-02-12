(* 01 *)
let rec last l =
  match l with
  | []      -> None
  | x :: [] -> Some x
  | x :: xs -> last xs
;;

(* 02 *)
let rec last_two l =
  match l with
  | [] -> None
  | _::[] -> None
  | x::x'::[] -> Some (x, x')
  | _::xs -> last_two xs
;;

(* 03*)
let rec at n l =
  match n, l with
  | _, [] -> None
  | 0, x::_ -> Some x
  | n, _::xs -> at (n-1) xs
;;

(* 04 *)
let rec length l =
  match l with
  | [] -> 0
  | _::xs -> 1+ length xs
;;

(* adjusted to use proper tail recursion *)
let rec length' l =
  let rec length'' n = function
  | [] -> n
  | _::xs -> length'' (n+1) xs in
  length'' 0 l
;;

(* 05 *)
let rev l =
  let rec rev' acc = function
  | [] -> acc
  | x::xs -> rev' (x::acc) xs in
  rev' [] l
;;