open Core
open Stdio

module Coordinate = struct
  module T = struct
    type t = int * int

    let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
    let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t
    let t_of_sexp = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp
  end

  include T
  include Comparable.Make (T)
end

let grid =
  In_channel.read_lines "inputs/day8.txt" |> List.to_array |> Array.map ~f:String.to_array
;;

let is_inside_grid (x, y) =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  x < height && x > -1 && y < width && y > -1
;;

let parse_char map coords = function
  | '.' -> map
  | c when Char.is_alphanum c -> Map.add_multi map ~key:c ~data:coords
  | _ -> failwith "we are cooked"
;;

let build_frequency_map grid =
  grid
  |> Array.foldi
       ~init:(Map.empty (module Char))
       ~f:(fun i acc line ->
         Array.foldi ~init:acc ~f:(fun j acc char -> parse_char acc (i, j) char) line)
;;

(* let p = (xa, ya) be an antinode *)
(* and a1 = (x1, y1), a2 = (x2, y2) be two antennas of the same frequency *)
(* that means: 2 * |p - a1| = |p - a2| has to be true *)
(* 2xa - 2x1 = xa - x2 <-> xa = 2x1 - x2 *)
(* 2ya - 2y1 = ya - y2 <-> ya = 2y1 - y2 *)
(* the second possible should then just be a1 and a2 swapped in the equation(?) *)

let get_antinodes_part1 (x1, y1) (x2, y2) =
  [ (2 * x1) - x2, (2 * y1) - y2; (2 * x2) - x1, (2 * y2) - y1 ]
  |> List.filter ~f:is_inside_grid
;;

let get_antinodes_part2 (x1, y1) (x2, y2) =
  let dx, dy = x1 - x2, y1 - y2 in
  let rec aux acc (prev_x, prev_y) dir =
    let potential_antinode = dir prev_x dx, dir prev_y dy in
    match is_inside_grid potential_antinode with
    | true -> aux (potential_antinode :: acc) potential_antinode dir
    | false -> acc
  in
  let forward = aux [ x1, y1 ] (x1, y1) Int.( + ) in
  let backwards = aux [ x1, y1 ] (x1, y1) Int.( - ) in
  List.append forward backwards
;;

let build_antinode_position_set get_antinodes frequency_map =
  let antinodes_for_one_freq antennas =
    let aux x =
      List.fold antennas ~init:[] ~f:(fun acc antenna ->
        if Coordinate.( <> ) x antenna then get_antinodes x antenna :: acc else acc)
    in
    List.fold antennas ~init:[] ~f:(fun acc x -> aux x :: acc)
    (* pls dont look at this, i know im terrible at programming okay? *)
    |> List.concat
    |> List.concat
  in
  Map.fold
    frequency_map
    ~init:(Set.empty (module Coordinate))
    ~f:(fun ~key:_ ~data:v acc ->
      List.fold ~init:acc ~f:(fun acc x -> Set.add acc x) (antinodes_for_one_freq v))
;;

let part_1 =
  grid
  |> build_frequency_map
  |> build_antinode_position_set get_antinodes_part1
  |> Set.length
;;

let () = part_1 |> Int.to_string |> print_endline

let part_2 =
  grid
  |> build_frequency_map
  |> build_antinode_position_set get_antinodes_part2
  |> Set.length
;;

let () = part_2 |> Int.to_string |> print_endline
