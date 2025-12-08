open Core
open Stdio

type cell =
  | Empty
  | Splitter
  | Origin

let point_to_cell = function
  | '^' -> Splitter
  | '.' -> Empty
  | 'S' -> Origin
  | _ -> assert false
;;

let grid =
  In_channel.read_lines "inputs/day_seven.txt"
  |> List.to_array
  |> Array.map ~f:String.to_array
  |> Array.map ~f:(fun arr -> Array.map ~f:point_to_cell arr)
;;

let get_start grid =
  let first_row = grid.(0) in
  let is_origin = function
    | Origin -> true
    | _ -> false
  in
  let res = Array.findi ~f:(fun _ cell -> is_origin cell) first_row in
  match res with
  | Some (col, _) -> 0, col
  | None -> failwith "Origin not found"
;;

let count_splits grid =
  let _, col_start = get_start grid in
  let beams = Hash_set.create (module Int) in
  Hash_set.add beams col_start;
  let splits = ref 0 in
  Array.iter
    ~f:(fun row ->
      let find_splitters idx acc cell =
        if Poly.( = ) cell Splitter then idx :: acc else acc
      in
      let splitter_cols = Array.foldi ~init:[] ~f:find_splitters row in
      List.iter
        ~f:(fun col ->
          if Hash_set.mem beams col then incr splits;
          Hash_set.add beams (col - 1);
          Hash_set.add beams (col + 1);
          Hash_set.remove beams col)
        splitter_cols)
    grid;
  splits
;;

let part_one =
  let splits = count_splits grid in
  !splits |> Int.to_string |> print_endline
;;

let () = part_one
