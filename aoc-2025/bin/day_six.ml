open Core
open Stdio

type op =
  | Add
  | Mult
  | Noop

let apply = function
  | Add -> ( + )
  | Mult -> ( * )
  | Noop -> fun _ _ -> 0
;;

let parse_homework =
  let is_op = function
    | "*" | "+" -> true
    | _ -> false
  in
  let get_op = function
    | "+" -> Add
    | "*" -> Mult
    | _ -> failwith "it's cooked"
  in
  let rec aux acc = function
    | hd :: tl ->
      let acc_lst = snd acc in
      let op = fst acc in
      if is_op hd
      then aux (get_op hd, acc_lst) tl
      else aux (op, Int.of_string hd :: acc_lst) tl
    | [] -> acc
  in
  aux (Noop, [])
;;

let homework_list =
  let input_opt =
    In_channel.read_lines "inputs/day_six.txt"
    |> List.map ~f:(String.split ~on:' ')
    |> List.map ~f:(fun lst -> List.filter ~f:(fun s -> String.is_empty s |> not) lst)
    |> List.transpose
  in
  let input =
    match input_opt with
    | Some x -> x
    | None -> []
  in
  List.map ~f:parse_homework input
;;

let solve_homework homework =
  let get_init_val = function
    | Add -> 0
    | Mult -> 1
    | Noop -> 0
  in
  let op = fst homework in
  let nums = snd homework in
  let func = apply op in
  List.fold ~init:(get_init_val op) ~f:func nums
;;

let part_one = List.map ~f:solve_homework homework_list |> List.fold ~init:0 ~f:( + )
let () = part_one |> Int.to_string |> print_endline
