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

(* 06 *)
let is_palindrone l =
  let l' = rev l in
  let rec eq' l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | _::_, [] -> false
    | [], _::_ -> false
    | x1::xs1, x2::xs2 -> if x1 = x2 then
      eq' xs1 xs2
      else false in
    eq' l l'
;;

(* 07 *)
let encode l =
  let rec line (sum, letter) = function
  | [] -> sum,letter,[]
  | x::xs -> if x = letter then
    line (sum+1, letter) xs
    else sum,letter, x::xs in
  let rec line' acc = function
  | [] -> acc
  | x::xs ->
    let sum, letter, xs' = line (1,x) xs in
    line' (acc @ [sum, letter]) xs' in
  line' [] l
;;

(* 08 *)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let inc = function
| One x -> Many (2,x)
| Many (n,x) -> Many (n+1,x)
;;
let from_rle = function
| One x -> x
| Many (_,x) -> x
;;

let encode2 l =
  let rec line acc = function
  | [] -> acc,[]
  | x::xs -> if x = from_rle acc then
    line (inc acc) xs
    else acc, x::xs in
  let rec line' acc = function
  | [] -> acc
  | x::xs ->
    let rle', xs' = line (One x) xs in
    line' (rle'::acc) xs' in
  line' [] l
    |> rev
;;

(* 09 *)
let duplicate l =
  let rec dupe acc = function
  | [] -> acc
  | x::xs -> dupe (x::x::acc) xs in
  dupe [] l
    |> rev

;;

(* 10 *)
let split l n =
  let rec split' acc n = function
  | [] -> acc, []
  | x::xs -> if n = 0 then
    acc, x::xs
    else split' (x::acc) (n-1) xs in
  let l1, l2 = split' [] n l in
  rev l1, l2
;;

(* 11 *)
let remove_at n l =
  let rec aux acc n = function
  | [] -> rev acc
  | x::xs -> if n = 0 then
    rev acc @ xs
    else aux (x::acc) (n-1) xs in
    aux [] n l
  ;;
  (* i am angry that the solution for 11 is not tail recursive.
   * I would have implemented identically to the solution if I were using haskell >:(
   *)

(* 12 *)
let insert_at e n l =
  let rec aux acc n e = function
  |[] -> e :: acc |> rev
  | x::xs -> if n = 0 then
    (rev acc) @ e::x::xs
  else aux (x::acc) (n-1) e xs in
  aux [] n e l
;;

(* 13 *)
let range from to' =
  let rec aux acc from to' = if from > to' then
    acc
  else aux (from::acc) (from+1) to' in
  aux [] from to' |> rev
;;

(* 14 *)
let lotto_select n to' =
  let numbers = range 1 to' in
  let open Random in
  let () = self_init () in
  let rec aux acc n  = function
  | [] -> acc
  | numbers ->
  if n = 0 then
    acc
  else let selected_index = length numbers |> int in
        (* [at n l] should always succeed, when 0 =< [n] < [length n] *)
        match at selected_index numbers with None -> []
        | Some selected_number ->
          remove_at selected_index numbers
          |> aux (selected_number::acc) (n-1) in
  aux [] n numbers
;;

(* 15 *)
let permutation l =
  let open Random in
  let () = self_init () in
  let rec aux acc = function
    | [] -> acc
    | l' ->
    let selected_index = length l' |> int in
      (* [at n l] should always succeed, when 0 =< [n] < [length n] *)
      match at selected_index l' with None -> []
      | Some selected_number ->
        remove_at selected_index l'
        |> aux (selected_number::acc) in
  aux [] l
;;