open Core
open Stdio

let rules_input, updates_input =
  In_channel.read_lines "inputs/day5.txt"
  |> List.split_while ~f:(fun s -> String.( <> ) s "")
  |> Tuple2.map ~f:(fun x ->
    if String.equal (Option.value ~default:"" (List.hd x)) "" then List.tl x else Some x)
  |> Tuple2.map ~f:(Option.value ~default:[])
;;

(* IDEA: Map from one page number to a list of page numbers that need to come after, 
a sort of kinda mini graph type thingy i guess i wouldnt know tho bcs idk anything about data structures*)

let parse_rule lst =
  let list_to_tuple = function
    | x :: y :: _ -> x, y
    | _ -> 0, 0
  in
  String.split ~on:'|' lst |> List.filter_map ~f:Int.of_string_opt |> list_to_tuple
;;

let parse_update lst = String.split ~on:',' lst |> List.filter_map ~f:Int.of_string_opt

let rules, updates =
  List.map rules_input ~f:parse_rule, List.map updates_input ~f:parse_update
;;

let build_rule_map rules =
  let graph = Map.empty (module Int) in
  List.fold rules ~init:graph ~f:(fun acc (x, y) -> Map.add_multi acc ~key:x ~data:y)
;;

let is_valid rule_map page_number page_numbers_before =
  let should_be_before_number =
    match Map.find rule_map page_number with
    | Some l -> l
    | None -> []
  in
  (* NOTE: this is kinda whacky, and prob a giant performance sink, i did it bcs its funny, probably fold instead *)
  List.append page_numbers_before should_be_before_number
  |> List.contains_dup ~compare:Int.compare
  |> not
;;

let rec is_update_valid rule_map acc = function
  | x :: tl ->
    if is_valid rule_map x acc then is_update_valid rule_map (x :: acc) tl else false
  | [] -> true
;;

(* i think we go over the update and for every element we get the things that are 
supposed to be after that element and check if weve already passed one of them *)
let part_1 rule_map updates =
  List.filter updates ~f:(is_update_valid rule_map [])
  |> List.fold ~init:0 ~f:(fun acc l ->
    acc + Option.value ~default:0 (List.nth l (List.length l / 2)))
;;

let () = part_1 (build_rule_map rules) updates |> Int.to_string |> print_endline

let reorder_invalid_updates rule_map updates =
  let num_needs_to_be_before_cur cur num =
    match Map.find rule_map num with
    | None -> false
    | Some l -> List.exists l ~f:(fun x -> x = cur)
  in
  let rec fix_invalid = function
    | [] -> []
    | x :: tl ->
      (match List.find tl ~f:(num_needs_to_be_before_cur x) with
       | None -> x :: fix_invalid tl
       | Some num -> num :: x :: List.filter ~f:(( <> ) num) tl |> fix_invalid)
  in
  List.map updates ~f:fix_invalid
;;

let part_2 rule_map updates =
  let updates_to_order =
    List.filter updates ~f:(fun x -> is_update_valid rule_map [] x |> not)
  in
  reorder_invalid_updates rule_map updates_to_order
  |> List.fold ~init:0 ~f:(fun acc l ->
    acc + Option.value ~default:0 (List.nth l (List.length l / 2)))
;;

let () = part_2 (build_rule_map rules) updates |> Int.to_string |> print_endline
