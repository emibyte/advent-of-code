open Core
open Stdio

let grid =
  In_channel.read_lines "inputs/day4.txt" |> List.to_array |> Array.map ~f:String.to_array
;;

let get_grid_point row col =
  if row >= Array.length grid || col >= Array.length grid.(0) || row < 0 || col < 0
  then None
  else Some grid.(row).(col)
;;

let find_xmas row col =
  let get_next c =
    match c with
    | 'X' -> 'M'
    | 'M' -> 'A'
    | 'A' -> 'S'
    | _ -> assert false
  in
  let directions = [ 1, 0; 0, 1; 1, 1; -1, 0; 0, -1; 1, -1; -1, 1; -1, -1 ] in
  let rec aux first prev (x, y) (dx, dy) =
    match get_grid_point x y with
    | Some c when Char.equal c 'X' ->
      if first then aux false c (x + dx, y + dy) (dx, dy) else false
    | Some c when Char.equal c (get_next prev) ->
      if Char.equal c 'S' then true else aux false c (x + dx, y + dy) (dx, dy)
    | Some _ -> false
    | None -> false
  in
  List.map directions ~f:(aux true 'X' (row, col)) |> List.count ~f:(fun x -> x)
;;

let count_xmases row col char = if Char.equal char 'X' then find_xmas row col else 0

let inner_fold row acc line =
  Array.foldi ~init:acc ~f:(fun col acc char -> acc + count_xmases row col char) line
;;

let part_1 = Array.foldi ~init:0 ~f:(fun row acc line -> inner_fold row acc line) grid
let () = part_1 |> Int.to_string |> print_endline

let find_x_mas row col =
  let directions = [ 1, 1; 1, -1 ] in
  let check_point_for_equality x y c =
    match get_grid_point x y with
    | Some ch -> Char.equal c ch
    | None -> false
  in
  let check_diagonal (x, y) (dx, dy) =
    match get_grid_point (x + dx) (y + dy) with
    | Some c when Char.equal c 'M' -> check_point_for_equality (x - dx) (y - dy) 'S'
    | Some c when Char.equal c 'S' -> check_point_for_equality (x - dx) (y - dy) 'M'
    | Some _ -> false
    | None -> false
  in
  List.for_all directions ~f:(check_diagonal (row, col))
;;

let count_x_mases row col char =
  if Char.equal char 'A' && find_x_mas row col then 1 else 0
;;

let inner_fold_part_2 row acc line =
  Array.foldi ~init:acc ~f:(fun col acc char -> acc + count_x_mases row col char) line
;;

let part_2 =
  Array.foldi ~init:0 ~f:(fun row acc line -> inner_fold_part_2 row acc line) grid
;;

let () = part_2 |> Int.to_string |> print_endline
