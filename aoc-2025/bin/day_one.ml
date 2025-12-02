open! Core

type rotation =
  | Left
  | Right

(* NOTE: mod in ocaml can return negative numbers we no want that *)
let pos_mod a b = ((a mod b) + b) mod b
let ( #/ ) = pos_mod

let rotate old_turn_count turn_count direction =
  let aux = function
    | Left -> old_turn_count - turn_count
    | Right -> old_turn_count + turn_count
  in
  let new_pos_no_wrap = aux direction in
  new_pos_no_wrap#/100
;;

let rotate_and_count_wraps old_turn_count turn_count direction =
  let aux = function
    | Left -> old_turn_count - turn_count
    | Right -> old_turn_count + turn_count
  in
  let new_pos_no_wrap = aux direction in
  let new_pos = new_pos_no_wrap#/100 in
  match direction with
  | Left ->
    if old_turn_count = 0
    then new_pos, turn_count / 100
    else if old_turn_count > turn_count
    then new_pos, 0
    else new_pos, 1 + ((turn_count - old_turn_count) / 100)
  | Right -> new_pos, (old_turn_count + turn_count) / 100
;;

let instructions =
  let parse s =
    let len = String.length s in
    let num_str = String.slice s 1 len in
    let num = Int.of_string num_str in
    match String.get s 0 with
    | 'L' -> Left, num
    | 'R' -> Right, num
    | _ -> failwith "it's cooked"
  in
  In_channel.read_lines "inputs/day_one.txt" |> List.map ~f:parse
;;

let part_one instructions =
  let rec aux acc old_turn_count = function
    | (dir, turn_count) :: tl ->
      let new_pos = rotate old_turn_count turn_count dir in
      if new_pos = 0 then aux (acc + 1) new_pos tl else aux acc new_pos tl
    | [] -> acc
  in
  aux 0 50 instructions |> Int.to_string |> Stdio.print_endline
;;

let () = part_one instructions

let part_two instructions =
  let rec aux acc old_turn_count = function
    | (dir, turn_count) :: tl ->
      let new_pos, wraps = rotate_and_count_wraps old_turn_count turn_count dir in
      aux (acc + wraps) new_pos tl
    | [] -> acc
  in
  aux 0 50 instructions |> Int.to_string |> Stdio.print_endline
;;

let () = part_two instructions
