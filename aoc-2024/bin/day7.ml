open Core
open Stdio

let parse line =
  (* NOTE: little big ugly but should be fine for now*)
  let split = String.split line ~on:':' in
  let expected_value =
    match List.hd split with
    | Some x -> x
    | None -> ""
  in
  let rest =
    match List.tl split with
    | Some xs -> List.map xs ~f:(String.split ~on:' ')
    | None -> []
  in
  ( Int.of_string expected_value
  , List.hd_exn (List.map rest ~f:(List.filter_map ~f:Int.of_string_opt)) )
;;

let input = In_channel.read_lines "inputs/day7.txt" |> List.map ~f:parse
let ( ||| ) a b = String.concat [ Int.to_string a; Int.to_string b ] |> Int.of_string

type 'a tree =
  | Node of 'a node
  | Leaf

and 'a node =
  { value : 'a
  ; left : 'a tree
  ; right : 'a tree
  ; middle : 'a tree
  }

(* IDEA: ITS A TREEEEEEEEEEE (i have literally never used a tree before im like so happy rn) *)
let build_computation_tree operands is_part_one =
  let rec aux prev op = function
    | value :: xs ->
      Node
        { value = op prev value
        ; left = aux (op prev value) Int.( + ) xs
        ; right = aux (op prev value) Int.( * ) xs
        ; middle = (if is_part_one then Leaf else aux (op prev value) ( ||| ) xs)
        }
    | [] -> Leaf
  in
  aux 0 Int.( + ) operands
;;

let rec is_value_in_comp_tree expected_value = function
  | Node node ->
    if
      node.value = expected_value
      && Poly.( = ) node.left Leaf
      && Poly.( = ) node.right Leaf
      && Poly.( = ) node.middle Leaf
    then true
    else
      is_value_in_comp_tree expected_value node.left
      || is_value_in_comp_tree expected_value node.right
      || is_value_in_comp_tree expected_value node.middle
  | Leaf -> false
;;

let can_be_solved (result, operands) is_part_one =
  build_computation_tree operands is_part_one |> is_value_in_comp_tree result
;;

let part_1 =
  List.sum
    (module Int)
    input
    ~f:(fun (expected, operands) ->
      if can_be_solved (expected, operands) true then expected else 0)
;;

let () = part_1 |> Int.to_string |> print_endline

let part_2 =
  List.sum
    (module Int)
    input
    ~f:(fun (expected, operands) ->
      if can_be_solved (expected, operands) false then expected else 0)
;;

let () = part_2 |> Int.to_string |> print_endline
