open Core
open Stdio

type direction =
  | Up
  | Right
  | Down
  | Left

module Coordinate = struct
  module T = struct
    type t = int * int

    let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
    let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t
    let t_of_sexp = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp
    let ( + ) (a1, a2) (b1, b2) = a1 + b1, a2 + b2
  end

  include T
  include Comparable.Make (T)
end

module CoordinateSet = Set.Make (Coordinate)
module CoordinateMap = Map.Make (Coordinate)

type collision_state =
  { direction : direction
  ; position : Coordinate.t
  ; obs_position : Coordinate.t
  }

let grid =
  In_channel.read_lines "inputs/day6.txt" |> List.to_array |> Array.map ~f:String.to_array
;;

let get_grid_point row col grid =
  if row >= Array.length grid || col >= Array.length grid.(0) || row < 0 || col < 0
  then None
  else Some grid.(row).(col)
;;

let get_direction = function
  | '^' -> Up
  | '>' -> Right
  | 'v' -> Down
  | '<' -> Left
  | _ -> failwith "non valid guard"
;;

type guard =
  { position : Coordinate.t
  ; direction : direction
  ; traversed : CoordinateSet.t
  ; hit_obstacles :
      (Coordinate.t, collision_state list, Coordinate.comparator_witness) Map.t
  ; is_looping : bool
  }

type cell =
  | Guard of guard
  | Empty
  | Obstacle
  | Outside

let turn_at_obstacle guard =
  match guard.direction with
  | Up -> { guard with direction = Right }
  | Right -> { guard with direction = Down }
  | Down -> { guard with direction = Left }
  | Left -> { guard with direction = Up }
;;

let get_step_destination guard = function
  | Up -> Coordinate.( + ) guard.position (-1, 0)
  | Right -> Coordinate.( + ) guard.position (0, 1)
  | Down -> Coordinate.( + ) guard.position (1, 0)
  | Left -> Coordinate.( + ) guard.position (0, -1)
;;

let walk_in_dir guard = function
  (* TODO: not sure if guard.position is already the updated one for the set update (i think not) *)
  | Up ->
    { guard with
      position = get_step_destination guard Up
    ; traversed = Set.add guard.traversed (get_step_destination guard Up)
    }
  | Right ->
    { guard with
      position = get_step_destination guard Right
    ; traversed = Set.add guard.traversed (get_step_destination guard Right)
    }
  | Down ->
    { guard with
      position = get_step_destination guard Down
    ; traversed = Set.add guard.traversed (get_step_destination guard Down)
    }
  | Left ->
    { guard with
      position = get_step_destination guard Left
    ; traversed = Set.add guard.traversed (get_step_destination guard Left)
    }
;;

let get_cell_type (row, col) grid =
  match get_grid_point row col grid with
  | Some c ->
    (match c with
     | '#' -> Obstacle
     | '^' | '>' | 'v' | '<' ->
       Guard
         { position = row, col
         ; direction = get_direction c
         ; traversed = Set.add CoordinateSet.empty (row, col)
         ; hit_obstacles = Map.empty (module Coordinate)
         ; is_looping = false
         }
     | _ -> Empty)
  | None -> Outside
;;

let take_a_step guard grid =
  let step_row, step_col = get_step_destination guard guard.direction in
  match get_cell_type (step_row, step_col) grid with
  | Empty -> walk_in_dir guard guard.direction
  | Guard _ -> walk_in_dir guard guard.direction
  | Obstacle ->
    let obs_position = step_row, step_col in
    let has_collision_happened_before =
      match Map.find guard.hit_obstacles (step_row, step_col) with
      | Some xs ->
        List.exists xs ~f:(fun cs ->
          Coordinate.( = ) cs.obs_position obs_position
          && Coordinate.( = ) cs.position guard.position
          && Poly.( = ) cs.direction guard.direction)
      | None -> false
    in
    if has_collision_happened_before
    then { guard with is_looping = true }
    else
      turn_at_obstacle
        { guard with
          hit_obstacles =
            Map.add_multi
              guard.hit_obstacles
              ~key:(step_row, step_col)
              ~data:
                { position = guard.position; direction = guard.direction; obs_position }
        }
  | Outside -> guard
;;

let rec walk_maze guard grid =
  let guard_after_step = take_a_step guard grid in
  if
    Poly.( = ) guard_after_step.direction guard.direction
    && Poly.( = ) guard_after_step.position guard.position
  then guard
  else walk_maze guard_after_step grid
;;

let init_guard grid =
  let is_guard = function
    | '^' | 'v' | '>' | '<' -> true
    | _ -> false
  in
  let row, col_and_dir_opt =
    Array.mapi grid ~f:(fun _ row -> Array.findi row ~f:(fun _ el -> is_guard el))
    |> Array.findi ~f:(fun _ x -> Option.is_some x)
    |> Option.value_exn
  in
  let row, (col, dir) = row, Option.value_exn col_and_dir_opt in
  let position, dir = (row, col), dir in
  { position = row, col
  ; direction = get_direction dir
  ; traversed = Set.add CoordinateSet.empty position
  ; hit_obstacles = Map.empty (module Coordinate)
  ; is_looping = false
  }
;;

let part_1 =
  let final = init_guard grid |> walk_maze in
  Set.length (final grid).traversed
;;

let () = part_1 |> Int.to_string |> print_endline

let traversed_without_start =
  let guard = init_guard grid in
  let start_point = guard.position in
  let traversed_with_start_point = walk_maze guard grid |> fun g -> g.traversed in
  Set.remove traversed_with_start_point start_point
;;

let get_grids_with_new_obstacle grid traversed =
  let copy_grid_and_place_obstacle row col =
    let copy = Array.copy_matrix grid in
    copy.(row).(col) <- '#';
    copy
  in
  Set.fold traversed ~init:[] ~f:(fun acc (row, col) ->
    copy_grid_and_place_obstacle row col :: acc)
;;

let rec is_looping grid guard =
  let guard_after_step = take_a_step guard grid in
  let did_not_move =
    Poly.( = ) guard_after_step.direction guard.direction
    && Poly.( = ) guard_after_step.position guard.position
  in
  if did_not_move then guard_after_step.is_looping else is_looping grid guard_after_step
;;

let part_2 =
  let base_grid = grid in
  traversed_without_start
  |> get_grids_with_new_obstacle base_grid
  |> List.map ~f:(fun grid -> is_looping grid (init_guard base_grid))
  |> List.count ~f:(fun b -> b)
;;

let () = part_2 |> Int.to_string |> print_endline
