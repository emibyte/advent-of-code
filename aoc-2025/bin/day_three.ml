open Core
open Stdio

let input = In_channel.read_lines "inputs/day_three.txt"

let max_joltage n line =
  let to_int ch = Char.to_int ch - Char.to_int '0' in
  let max_idx digits_needed length = length - digits_needed in
  let int_line = String.to_list line |> List.map ~f:to_int in
  (* find largest element with smallest index from 0 to max_idx*)
  let get_max_and_idx list digits_needed =
    let max_idx = max_idx digits_needed (List.length list) in
    let rec aux max idx_max cur_idx = function
      | x :: y when x > max && cur_idx <= max_idx -> aux x cur_idx (cur_idx + 1) y
      | _ :: _ when cur_idx > max_idx -> max, idx_max
      | _ :: y -> aux max idx_max (cur_idx + 1) y
      | [] -> max, idx_max
    in
    aux 0 0 0 list
  in
  let rec aux acc digits_remaining line =
    let max, idx = get_max_and_idx line digits_remaining in
    let new_line =
      if idx + 1 < List.length line then List.slice line (idx + 1) 0 else []
    in
    if digits_remaining > 0 then aux (max :: acc) (digits_remaining - 1) new_line else acc
  in
  let result = List.rev (aux [] n int_line) in
  let len = List.length result in
  List.foldi
    ~init:0
    ~f:(fun idx acc elt -> (Int.pow 10 (len - idx - 1) * elt) + acc)
    result
;;

let part_one = List.map ~f:(max_joltage 2) input |> List.fold ~init:0 ~f:( + )
let () = part_one |> Int.to_string |> print_endline
let part_two = List.map ~f:(max_joltage 12) input |> List.fold ~init:0 ~f:( + )
let () = part_two |> Int.to_string |> print_endline
