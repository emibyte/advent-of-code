open Core
open Stdio

let grid =
  In_channel.read_lines "inputs/day_four.txt"
  |> List.to_array
  |> Array.map ~f:String.to_array
;;

let get_pos grid y x =
  let y_max = Array.length grid - 1 in
  let x_max = Array.length grid.(1) - 1 in
  let is_out_of_bounds = y > y_max || x > x_max || y < 0 || x < 0 in
  if is_out_of_bounds then '.' else grid.(y).(x)
;;

let can_be_accessed grid y x =
  let is_blocking ch = Char.equal ch '@' in
  let positions =
    [ y - 1, x
    ; y + 1, x
    ; y, x + 1
    ; y, x - 1
    ; y - 1, x + 1
    ; y - 1, x - 1
    ; y + 1, x + 1
    ; y + 1, x - 1
    ]
  in
  let count_rolls_surrounding =
    List.map ~f:(fun (y, x) -> get_pos grid y x) positions
    |> List.filter ~f:is_blocking
    |> List.fold ~init:0 ~f:(fun acc _ -> acc + 1)
  in
  count_rolls_surrounding < 4
;;

let get_coords_removables grid =
  Array.foldi
    ~init:[]
    ~f:(fun y acc row ->
      Array.foldi
        ~init:acc
        ~f:(fun x acc' ch ->
          if can_be_accessed grid y x && Char.equal ch '@' then (y, x) :: acc' else acc')
        row)
    grid
;;

(* NOTE: lowkey not sure why this works anymore, think through it again *)
let get_count_removables grid = get_coords_removables grid |> List.length
let part_one = get_count_removables grid
let () = part_one |> Int.to_string |> print_endline

let remove_removables grid coords =
  let remove_removable y x = grid.(y).(x) <- '.' in
  List.iter ~f:(fun (y, x) -> remove_removable y x) coords
;;

let part_two grid =
  let rec sum acc =
    let coords = get_coords_removables grid in
    let count = List.length coords in
    if count > 0
    then (
      remove_removables grid coords;
      sum (acc + count))
    else acc
  in
  sum 0
;;

let () = part_two grid |> Int.to_string |> print_endline
