open Core
open Stdio

let coords =
  let coords_from_list = function
    | [ x; y ] -> Int.of_string x, Int.of_string y
    | _ -> failwith "not a vaild coord"
  in
  In_channel.read_lines "inputs/day_nine.txt"
  |> List.map ~f:(fun str -> String.split ~on:',' str |> coords_from_list)
;;

let area (x1, y1) (x2, y2) =
  let a = max x1 x2 - min x1 x2 + 1 in
  let b = max y1 y2 - min y1 y2 + 1 in
  a * b
;;

let rectangles coords =
  List.foldi
    ~init:[]
    ~f:(fun i acc x ->
      let rest = List.drop coords (i + 1) in
      let combs = List.map ~f:(fun y -> x, y) rest in
      List.fold ~init:acc ~f:(fun acc' elt -> elt :: acc') combs)
    coords
;;

let areas coords =
  let rects = rectangles coords in
  List.map ~f:(fun x -> area (fst x) (snd x)) rects
;;

let part_one =
  let max_area =
    match areas coords |> List.max_elt ~compare:Int.compare with
    | Some area -> area
    | None -> 0
  in
  Int.to_string max_area |> print_endline
;;

let () = part_one
