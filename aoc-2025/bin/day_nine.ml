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

module Rectangle = struct
  type t =
    { r1 : int
    ; c1 : int
    ; r2 : int
    ; c2 : int
    }
  (* [@@deriving show] *)
  (* To use [@@deriving show], you need to add the ppx_deriving.show preprocessor in your dune file,
     like this: (preprocess (pps ppx_deriving.show)). *)
  (* https://ocaml.org/cookbook/debug-print-a-value/ppx_deriving *)

  let create row1 col1 row2 col2 =
    { r1 = min row1 row2; c1 = min col1 col2; r2 = max row1 row2; c2 = max col1 col2 }
  ;;

  let area rect =
    let a = rect.r2 - rect.r1 + 1 in
    let b = rect.c2 - rect.c1 + 1 in
    a * b
  ;;
end

module CompressedGrid = struct
  type tile =
    { row : int
    ; col : int
    }

  type tile_state =
    | Border
    | Inside
    | Outside

  type t =
    { rows : int array
    ; cols : int array
    ; tiles : tile_state array array (* compressed version of the grid *)
    ; min : tile
    ; max : tile
    }

  let create_tile r c = { row = r; col = c }
  let index tile grid = grid.tiles.(tile.row).(tile.col)

  let _print_grid grid =
    let tile_to_str = function
      | Inside -> "Inside"
      | Outside -> "Outside"
      | Border -> "Border"
    in
    Array.map grid.tiles ~f:(Array.map ~f:tile_to_str)
  ;;

  let neighbors_in_bounds tile min max =
    let ( + ) t1 t2 = { row = t1.row + t2.row; col = t1.col + t2.col } in
    let inside_bounds tile =
      tile.row >= min.row
      && tile.row <= max.row
      && tile.col >= min.col
      && tile.col <= max.col
    in
    let deltas =
      [ { row = 0; col = 1 }
      ; { row = 0; col = -1 }
      ; { row = 1; col = 0 }
      ; { row = -1; col = 0 }
      ]
    in
    List.filter_map
      ~f:(fun dt -> if inside_bounds (tile + dt) then Some (tile + dt) else None)
      deltas
  ;;

  let get_axis coords =
    let dedup_int_array arr =
      let seen = Hash_set.create (module Int) in
      Array.filter
        ~f:(fun x ->
          if Hash_set.mem seen x
          then false
          else (
            Hash_set.add seen x;
            true))
        arr
    in
    let min_opt = Array.min_elt ~compare:Int.compare coords in
    let max_opt = Array.max_elt ~compare:Int.compare coords in
    let new_coords =
      match min_opt, max_opt with
      | Some min, Some max -> Array.append coords [| min - 1; max + 1 |]
      | _ -> coords
    in
    Array.sort new_coords ~compare:Int.compare;
    dedup_int_array new_coords
  ;;

  let compressed_rect grid tile1 tile2 =
    let r1, c1 =
      ( Array.binary_search ~compare:Int.compare grid.rows `First_equal_to tile1.row
      , Array.binary_search ~compare:Int.compare grid.cols `First_equal_to tile1.col )
    in
    let r2, c2 =
      ( Array.binary_search ~compare:Int.compare grid.rows `First_equal_to tile2.row
      , Array.binary_search ~compare:Int.compare grid.cols `First_equal_to tile2.col )
    in
    match r1, c1, r2, c2 with
    | Some row1, Some col1, Some row2, Some col2 -> Rectangle.create row1 col1 row2 col2
    | _ -> failwith "oopsie"
  ;;

  let is_compressed_valid (grid : t) ({ r1; c1; r2; c2 } : Rectangle.t) : bool =
    let row_range = List.range ~stop:`inclusive r1 r2 in
    let col_range = List.range ~stop:`inclusive c1 c2 in
    List.cartesian_product row_range col_range
    |> List.for_all ~f:(fun (r, c) ->
      match grid.tiles.(r).(c) with
      | Outside -> false
      | _ -> true)
  ;;

  let add_boundaries grid original_tiles =
    let circular_windows arr =
      let n = Array.length arr in
      if n < 2
      then []
      else
        List.init n ~f:(fun i ->
          let a = arr.(i) in
          let b = arr.((i + 1) mod n) in
          a, b)
    in
    let points = circular_windows original_tiles in
    List.iter
      ~f:(fun (a, b) ->
        let rect = compressed_rect grid a b in
        if rect.r1 = rect.r2
        then
          Array.fill grid.tiles.(rect.r1) ~pos:rect.c1 ~len:(rect.c2 - rect.c1 + 1) Border
        else
          for i = rect.r1 to rect.r2 do
            let row = grid.tiles.(i) in
            row.(rect.c1) <- Border
          done)
      points
  ;;

  let mark_outside grid =
    let frontier = Deque.create () in
    let start = { row = 0; col = 0 } in
    grid.tiles.(0).(0) <- Outside;
    Deque.enqueue_back frontier start;
    let min = grid.min in
    let max = grid.max in
    let rec loop () =
      let tile_opt = Deque.dequeue_front frontier in
      match tile_opt with
      | Some t ->
        let neighbors = neighbors_in_bounds t min max in
        List.iter
          ~f:(fun pos ->
            match index pos grid with
            | Inside ->
              grid.tiles.(pos.row).(pos.col) <- Outside;
              Deque.enqueue_back frontier pos
            | _ -> ())
          neighbors;
        loop ()
      | None -> ()
    in
    loop ()
  ;;

  let create original_tiles =
    let rows = get_axis (Array.map ~f:(fun t -> t.row) original_tiles) in
    let cols = get_axis (Array.map ~f:(fun t -> t.col) original_tiles) in
    let tiles =
      Array.init (Array.length rows) ~f:(fun _ ->
        Array.init (Array.length cols) ~f:(fun _ -> Inside))
    in
    let min = { row = 0; col = 0 } in
    let max = { row = Array.length tiles - 1; col = Array.length tiles.(0) - 1 } in
    let grid = { rows; cols; tiles; min; max } in
    add_boundaries grid original_tiles;
    mark_outside grid;
    grid
  ;;
end

let part_two =
  let tiles =
    let parse_tile = function
      | [ x; y ] -> CompressedGrid.create_tile (Int.of_string x) (Int.of_string y)
      | _ -> failwith "not a vaild coord"
    in
    In_channel.read_lines "inputs/day_nine.txt"
    |> List.map ~f:(fun str -> String.split ~on:',' str |> parse_tile)
    |> List.to_array
  in
  let grid = CompressedGrid.create tiles in
  (* produce a list with (area, compressed_rect) tuples that i can then filter and get the max of *)
  let tile_pairs =
    Array.foldi
      ~init:[]
      ~f:(fun i acc t1 ->
        let rest = Array.slice tiles (i + 1) 0 in
        let comb = Array.map ~f:(fun t2 -> t1, t2) rest |> Array.to_list in
        List.fold ~init:acc ~f:(fun acc' rect -> rect :: acc') comb)
      tiles
  in
  let area_and_compressed_rect =
    List.map
      ~f:(fun (t1, t2) ->
        let rect = Rectangle.create t1.row t1.col t2.row t2.col in
        ( Rectangle.area rect
        , CompressedGrid.compressed_rect grid t1 t2
          |> CompressedGrid.is_compressed_valid grid ))
      tile_pairs
  in
  List.filter ~f:(fun (_, valid) -> valid) area_and_compressed_rect
  |> List.max_elt ~compare:(fun (area, _) (area2, _) -> Int.compare area area2)
  |> Option.value_exn
  |> Tuple2.get1
;;

let () = part_two |> Int.to_string |> print_endline
