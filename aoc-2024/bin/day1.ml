open Stdio
open Base

let parse_line str =
  let open String in
  let splitted = split ~on:' ' str in
  let trimmed = List.map ~f:strip splitted in
  let ints = List.filter_map ~f:Int.of_string_opt trimmed in
  match ints with
  | [ x; y ] -> x, y
  (* theres always 2 numbers in every line so every other case virtually cant happen and doesnt need to be handled*)
  | _ -> assert false
;;

let contents =
  let raw = In_channel.read_lines "inputs/day1.txt" in
  List.map ~f:parse_line raw |> List.unzip
;;

let part_1 (first, second) =
  let sort = List.sort ~compare:Poly.compare in
  let sorted_fst, sorted_snd = sort first, sort second in
  let sorted_zip =
    match List.zip sorted_fst sorted_snd with
    | List.Or_unequal_lengths.Unequal_lengths -> []
    | Ok l -> l
  in
  let distances = List.map ~f:(fun (x, y) -> abs (x - y)) sorted_zip in
  List.sum (module Int) distances ~f:(fun x -> x)
;;

let () = contents |> part_1 |> Int.to_string |> Stdio.print_endline

let map_add_handle_dupe map ~key =
  let count =
    match Map.find map key with
    | None -> 0
    | Some x -> x
  in
  Map.set map ~key ~data:(count + 1)
;;

(* TODO: write this with a recursive auxiliary function instead of a fold
   i tried it at first but sth was wrong, and the fold worked fine, weird *)
let build_occurance_table lst =
  let occ_table = Map.empty (module Int) in
  List.fold ~init:occ_table ~f:(fun acc x -> map_add_handle_dupe acc ~key:x) lst
;;

let part_2 (first, second) =
  let occ_map = build_occurance_table second in
  List.fold first ~init:0 ~f:(fun acc x ->
    acc + (x * Option.value_map (Map.find occ_map x) ~default:0 ~f:(fun y -> y)))
;;

let () = contents |> part_2 |> Int.to_string |> Stdio.print_endline
