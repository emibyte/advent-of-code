open Core
open Stdio
open Aoc_2025

let intervals =
  In_channel.read_all "inputs/day_two.txt"
  |> String.strip
  |> String.split ~on:','
  |> List.map ~f:(fun s -> String.split ~on:'-' s |> List.map ~f:Int.of_string)
;;

let get_range start last =
  let rec aux acc cur = if cur > last then acc else aux (cur :: acc) (cur + 1) in
  List.rev (aux [] start)
;;

let ranges =
  let aux = function
    | x :: y :: _ -> x, y
    | _ -> failwith "it's cooked"
  in
  List.map
    ~f:(fun lst ->
      let start, last = aux lst in
      get_range start last)
    intervals
;;

let is_invalid num =
  let len = Int.to_string num |> String.length in
  let divisor = Int.pow 10 (len / 2) in
  let even = len mod 2 = 0 in
  even && num / divisor = num mod divisor
;;

let part_one =
  List.map ~f:(List.filter ~f:is_invalid) ranges
  |> My_list.flatten
  |> List.fold ~init:0 ~f:( + )
  |> Int.to_string
  |> print_endline
;;

let () = part_one

(* TODO: this is probably very suboptimal, should try to look where this can be optimized *)
let contains_pattern num =
  let digits = Int.to_string num |> String.to_list in
  let stop = List.length digits / 2 in
  let rec prefixes acc running_prefix idx = function
    | c :: tl ->
      if idx <= stop
      then prefixes (running_prefix :: acc) (c :: running_prefix) (idx + 1) tl
      else List.map ~f:List.rev acc
    | [] -> acc
  in
  let prefs = prefixes [] [] 0 digits in
  let prefs = List.drop_last_exn prefs in
  let possible_prefs =
    List.filter ~f:(fun pref -> List.length digits mod List.length pref = 0) prefs
  in
  let construct_number pref =
    let pattern_number = List.length digits / List.length pref in
    let pattern = String.of_char_list pref in
    let rec aux n acc = if n = 1 then acc else aux (n - 1) (String.append acc pattern) in
    aux pattern_number pattern
  in
  List.map ~f:construct_number possible_prefs
  |> List.exists ~f:(fun el -> String.equal el (String.of_char_list digits))
;;

let part_two =
  List.map ~f:(List.filter ~f:contains_pattern) ranges
  |> My_list.flatten
  |> List.fold ~init:0 ~f:( + )
  |> Int.to_string
  |> print_endline
;;

let () = part_two
