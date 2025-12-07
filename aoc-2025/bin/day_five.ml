open Core
open Stdio

let fresh_ranges, available =
  let two_list_to_int_tup = function
    | x :: y :: _ -> Int.of_string x, Int.of_string y
    | _ -> assert false
  in
  let fresh_str, avail_str =
    In_channel.read_lines "inputs/day_five.txt"
    |> List.split_while ~f:(fun x -> String.equal x "" |> not)
    |> Tuple2.map ~f:(List.filter ~f:(fun s -> String.length s > 0))
  in
  ( List.map ~f:(fun str -> String.split ~on:'-' str |> two_list_to_int_tup) fresh_str
  , List.filter_map ~f:Int.of_string_opt avail_str )
;;

let is_fresh fresh_ranges id =
  (* TODO: probably can make this a little faster by only going over the relevant ranges (filtering something out out of fresh_ranges beforehand) *)
  let is_in_range low high = id >= low && id <= high in
  let rec aux = function
    | (low, high) :: tl -> if is_in_range low high then true else aux tl
    | [] -> false
  in
  aux fresh_ranges
;;

let part_one = List.filter ~f:(is_fresh fresh_ranges) available |> List.length
let () = part_one |> Int.to_string |> print_endline

let merge_ranges fresh_ranges =
  let sorted_ranges =
    List.sort fresh_ranges ~compare:(fun (low, _) (low2, _) -> Int.compare low low2)
  in
  let rec aux acc = function
    | (low, high) :: (low2, high2) :: tl ->
      if high >= low2
      then aux acc ((low, max high high2) :: tl)
      else aux ((low, high) :: acc) ((low2, high2) :: tl)
    | (low, high) :: [] -> (low, high) :: acc
    | [] -> acc
  in
  aux [] sorted_ranges
;;

let part_two =
  let get_count_in_range (low, high) = high - low + 1 in
  let unique_ranges = merge_ranges fresh_ranges in
  List.map ~f:get_count_in_range unique_ranges |> List.fold ~init:0 ~f:( + )
;;

let () = part_two |> Int.to_string |> print_endline
