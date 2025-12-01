open Base
open Stdio

type direction =
  | Ascending
  | Descending
  | Steady

let get_dir x y =
  match x - y with
  | 0 -> Steady
  | value when value > 0 -> Descending
  | _ -> Ascending
;;

let get_initial_dir = function
  | x :: y :: _ when x > y -> Descending
  | x :: y :: _ when x < y -> Ascending
  | _ -> Steady
;;

let parse_line str = String.split ~on:' ' str |> List.map ~f:Int.of_string
let contents = In_channel.read_lines "inputs/day2.txt" |> List.map ~f:parse_line

(* NOTE: it's insane to me that is is correct, i was just kinda vibing writing this lmao *)
let is_safe lst =
  let rec aux dir = function
    | x :: y :: _ when abs (x - y) > 3 -> false
    | x :: y :: _ when Poly.( <> ) (get_dir x y) dir || Poly.equal dir Steady -> false
    | x :: (y :: _ as tl) when Poly.equal (get_dir x y) dir -> aux (get_dir x y) tl
    | _ -> true
  in
  aux (get_initial_dir lst) lst
;;

(* NOTE: i couldnt get this to work properly and decided on the remove_one approach instead :c *)
(* let is_safe_with_dampener lst =
  let rec aux dir mistakes = function
    | x :: (y :: _ as tl) when abs (x - y) > 3 -> aux (get_dir x y) (mistakes + 1) tl
    | x :: (y :: _ as tl) when Poly.( <> ) (get_dir x y) dir || Poly.equal dir Steady ->
      aux (get_dir x y) (mistakes + 1) tl
    | x :: (y :: _ as tl) when Poly.equal (get_dir x y) dir ->
      aux (get_dir x y) mistakes tl
    | _ -> mistakes
  in
  aux (get_initial_dir lst) 0 lst <= 1
;; *)

let remove_one lst =
  let n = List.length lst in
  let remove_elem elem l =
    let first, second = List.split_n l (elem - 1) in
    let new_list =
      match second with
      | _ :: tl -> tl
      | [] -> []
    in
    List.append first new_list
  in
  let rec aux acc elem = function
    | l -> if elem > n then acc else aux (remove_elem elem l :: acc) (elem + 1) l
  in
  List.rev (aux [] 1 lst)
;;

let part_1 = List.map ~f:is_safe contents |> List.count ~f:(fun x -> x)
let () = part_1 |> Int.to_string |> Stdio.print_endline

let part_2 =
  List.map ~f:remove_one contents
  |> List.map ~f:(List.exists ~f:(fun x -> is_safe x))
  |> List.count ~f:(fun x -> x)
;;

let () = part_2 |> Int.to_string |> Stdio.print_endline
