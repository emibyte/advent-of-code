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
  !splits
;;

let part_one = count_splits grid |> Int.to_string |> print_endline
let () = part_one

(* TODO: look at this again, it produces the correct output but i feel like sth is still wrong *)
let count_timelines grid =
  let _, col_start = get_start grid in
  let beam_count_per_col = Hashtbl.create (module Int) in
  Hashtbl.add_exn beam_count_per_col ~key:col_start ~data:1;
  Array.iter
    ~f:(fun row ->
      let find_splitters idx acc cell =
        if Poly.( = ) cell Splitter then idx :: acc else acc
      in
      let splitter_cols = Array.foldi ~init:[] ~f:find_splitters row in
      List.iter
        ~f:(fun col ->
          let update_or_insert key count =
            Hashtbl.update beam_count_per_col key ~f:(function
              | None -> count
              | Some n -> n + count)
          in
          match Hashtbl.find_and_remove beam_count_per_col col with
          | Some count ->
            update_or_insert (col - 1) count;
            update_or_insert (col + 1) count
          | None -> ())
        splitter_cols)
    grid;
  Hashtbl.data beam_count_per_col |> List.fold ~init:0 ~f:( + )
;;

let part_two = count_timelines grid |> Int.to_string |> print_endline
let () = part_two
