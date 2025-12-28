open Core
open Stdio

module Machine : sig
  type t =
    { lights : int
    ; buttons : int list list
    ; joltages : int list
    }

  val from_str : string -> t
end = struct
  type t =
    { lights : int
    ; buttons : int list list
    ; joltages : int list
    }

  let from_str line =
    let parts = String.split ~on:' ' line in
    let is_lights s = Char.equal (String.nget s 0) '[' in
    let is_button s = Char.equal (String.nget s 0) '(' in
    let is_joltages s = Char.equal (String.nget s 0) '{' in
    let strip_lights_delimiters =
      String.strip ~drop:(function
        | '[' | ']' -> true
        | _ -> false)
    in
    let strip_button_delimiters =
      String.strip ~drop:(function
        | '(' | ')' -> true
        | _ -> false)
    in
    let strip_joltages_delimiters =
      String.strip ~drop:(function
        | '{' | '}' -> true
        | _ -> false)
    in
    let parse_lights s =
      (* NOTE: reversed to make the checks later easier for [.#.#] we thus get 0101 reversed which is 1010 *)
      let stripped = strip_lights_delimiters s |> String.to_list |> List.rev in
      List.fold
        ~init:0
        ~f:(fun acc c -> (acc lsl 1) lor if Char.equal c '#' then 1 else 0)
        stripped
    in
    let parse_button s =
      String.split ~on:',' (strip_button_delimiters s)
      |> List.filter_map ~f:Int.of_string_opt
    in
    let parse_joltages s =
      String.split ~on:',' (strip_joltages_delimiters s)
      |> List.filter_map ~f:Int.of_string_opt
    in
    let rec aux lights buttons joltages = function
      | x :: y when is_lights x -> aux (parse_lights x) buttons joltages y
      | x :: y when is_button x ->
        let button = parse_button x in
        aux lights (button :: buttons) joltages y
      | x :: y when is_joltages x -> aux lights (List.rev buttons) (parse_joltages x) y
      | _ :: _ | [] -> { lights; buttons; joltages }
    in
    aux 0 [] [] parts
  ;;
end

let parse () = In_channel.read_lines "inputs/day_ten.txt" |> List.map ~f:Machine.from_str

(* NOTE: could probably be faster im not sure how tho :c, nvm i didnt cache properly LOL *)
let part_one () =
  let input = parse () in
  List.map
    ~f:(fun machine ->
      let queue = Queue.create () in
      Queue.enqueue queue (0, 0);
      let seen = Hash_set.create (module Int) in
      Hash_set.add seen 0;
      let rec loop () =
        match Queue.dequeue queue with
        | Some (lights, distance) ->
          if lights = machine.lights
          then distance
          else (
            List.iter
              ~f:(fun button ->
                let new_lights =
                  List.fold ~init:lights ~f:(fun acc n -> acc lxor (1 lsl n)) button
                in
                if not (Hash_set.mem seen new_lights)
                then (
                  Hash_set.add seen new_lights;
                  Queue.enqueue queue (new_lights, distance + 1)))
              machine.buttons;
            loop ())
        (* NOTE: we should never get here, we should have found a distance first *)
        | None -> assert false
      in
      loop ())
    input
  |> List.sum (module Int) ~f:(fun x -> x)
;;

let () = part_one () |> Int.to_string |> print_endline

(** Tried to do part two with a similar approach to part one, sadly it was so slow it didn't finish :c *)

module Matrix : sig
  type t =
    { data : float array array
    ; rows : int
    ; cols : int
    ; mutable dependents : int array
    ; mutable free : int array
    }

  val from_machine : Machine.t -> t
  val valid : t -> int array -> int option
end = struct
  type t =
    { data : float array array
    ; rows : int
    ; cols : int
    ; mutable dependents : int array
    ; mutable free : int array
    }

  let epsilon = 1e-9

  let gaussian_elimination matrix =
    let pivot = ref 0 in
    let col = ref 0 in
    while !pivot < matrix.rows && !col < matrix.cols do
      let best_row, best_value =
        let candidates =
          let list = matrix.data |> Array.to_list in
          let dropped = List.drop list !pivot in
          List.mapi ~f:(fun i row -> !pivot + i, Float.abs row.(!col)) dropped
        in
        match
          List.max_elt candidates ~compare:(fun (_, a) (_, b) -> Float.compare a b)
        with
        | Some (r, v) -> r, v
        | None -> failwith "this shouldnt happen"
      in
      if Float.( < ) best_value epsilon
      then (
        matrix.free <- Array.append matrix.free [| !col |];
        incr col)
      else (
        Array.swap matrix.data !pivot best_row;
        matrix.dependents <- Array.append matrix.dependents [| !col |];
        let pivot_value = matrix.data.(!pivot).(!col) in
        for i = !col to matrix.cols do
          matrix.data.(!pivot).(i) <- matrix.data.(!pivot).(i) /. pivot_value
        done;
        for r = 0 to matrix.rows - 1 do
          if r <> !pivot
          then (
            let factor = matrix.data.(r).(!col) in
            if Float.( > ) (Float.abs factor) epsilon
            then (
              let pivot_row = Array.slice matrix.data.(!pivot) !col (matrix.cols + 1) in
              for i = !col to matrix.cols do
                matrix.data.(r).(i)
                <- matrix.data.(r).(i) -. (factor *. pivot_row.(i - !col))
              done))
        done;
        incr pivot;
        incr col)
    done;
    if matrix.cols > !col
    then
      matrix.free
      <- Array.append matrix.free (Array.init (matrix.cols - !col) ~f:(fun i -> !col + i))
  ;;

  let _debug_matrix matrix =
    Printf.printf "MATRIX rows=%d cols=%d\n" matrix.rows matrix.cols;
    Printf.printf
      "dependents: [%s]\n"
      (String.concat
         ~sep:"; "
         (Array.to_list matrix.dependents |> List.map ~f:Int.to_string));
    Printf.printf
      "free: [%s]\n"
      (String.concat ~sep:"; " (Array.to_list matrix.free |> List.map ~f:Int.to_string));
    Array.iteri matrix.data ~f:(fun r row ->
      Printf.printf
        "row %d: %s | %f\n"
        r
        (String.concat
           ~sep:"; "
           (Array.to_list row |> List.map ~f:(Printf.sprintf "%.6f")))
        row.(matrix.cols))
  ;;

  (* given the machine '[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}'
     we produce the matrix:
     rows = 4
     cols = 6
     | 0.0 | 0.0 | 0.0 | 0.0  | 1.0  | 1.0  | | 3
     | 0.0 | 1.0 | 0.0 | 0.0  | 0.0  | 1.0  | | 5
     | 0.0 | 0.0 | 1.0 | 1.0  | 1.0  | 0.0  | | 4
     | 1.0 | 1.0 | 0.0 | 1.0  | 0.0  | 0.0  | | 7
  *)

  (** we are producing a matrix that represents a set of linear equations where the right side of the equation is
      the joltage of the machine and the right side is represent by the values of the buttons *)
  let from_machine (machine : Machine.t) : t =
    let rows = List.length machine.joltages in
    let cols = List.length machine.buttons in
    let data = Array.init rows ~f:(fun _ -> Array.init (cols + 1) ~f:(fun _ -> 0.0)) in
    List.iteri machine.buttons ~f:(fun c button ->
      List.iter button ~f:(fun r -> if r < rows then data.(r).(c) <- 1.0));
    List.iteri machine.joltages ~f:(fun r joltage ->
      data.(r).(cols) <- Int.to_float joltage);
    let matrix = { data; rows; cols; dependents = [||]; free = [||] } in
    gaussian_elimination matrix;
    (* _debug_matrix matrix; *)
    matrix
  ;;

  (** Figure out if the given values are a valid solution to the equations
      (only whole number, non negative button presses) *)
  let valid (matrix : t) (values : int array) : int option =
    let total = Array.sum (module Int) values ~f:(fun x -> x) in
    let result =
      Array.foldi matrix.dependents ~init:(Some total) ~f:(fun row acc _ ->
        if Option.is_none acc
        then None
        else (
          let value =
            Array.foldi
              matrix.free
              ~init:matrix.data.(row).(matrix.cols)
              ~f:(fun i acc' col ->
                acc' -. (matrix.data.(row).(col) *. (values.(i) |> Int.to_float)))
          in
          if Float.( < ) value (-.epsilon)
          then None
          else (
            let rounded = Float.round value in
            if Float.( > ) (Float.abs (value -. rounded)) epsilon
            then None
            else (
              match acc with
              | Some acc' -> Some (acc' + Float.to_int rounded)
              | None -> None))))
    in
    result
  ;;
end

let rec dfs
          (matrix : Matrix.t)
          (idx : int)
          (values : int array)
          (min : int ref)
          (max : int)
  =
  if idx = Array.length matrix.free
  then (
    match Matrix.valid matrix values with
    | Some total -> min := Int.min !min total
    | None -> ())
  else (
    let total = Array.sum (module Int) (Array.slice values 0 idx) ~f:(fun x -> x) in
    let rec loop i =
      if i >= max
      then ()
      else if total + i >= !min
      then ()
      else (
        values.(idx) <- i;
        dfs matrix (idx + 1) values min max;
        loop (i + 1))
    in
    loop 0)
;;

(* for i = 0 to (max - 1) do *)
(*   if total + i >= !min *)
(*   then () *)
(*   else ( *)
(*     values.(idx) <- i; *)
(*     dfs matrix (idx + 1) values min max) *)
(* done) *)

let part_two () : int =
  let machines = parse () in
  let mins =
    List.map machines ~f:(fun machine ->
      let matrix = Matrix.from_machine machine in
      let max =
        (match List.max_elt ~compare:Int.compare machine.joltages with
         | Some m -> m
         | None -> 0)
        + 1
      in
      let min = ref Int.max_value in
      let values = Array.init (Array.length matrix.free) ~f:(fun _ -> 0) in
      dfs matrix 0 values min max;
      min)
  in
  List.sum (module Int) mins ~f:(fun min -> !min)
;;

let () = part_two () |> Int.to_string |> print_endline
