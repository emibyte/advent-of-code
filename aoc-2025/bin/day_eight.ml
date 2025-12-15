open Core
open Stdio

type part =
  | Part_one
  | Part_two

module Vec3 : sig
  type t =
    { x : float
    ; y : float
    ; z : float
    }

  val dist : t -> t -> float
  val from_str : string -> t
end = struct
  type t =
    { x : float
    ; y : float
    ; z : float
    }

  let create x y z = { x; y; z }

  let dist t1 t2 =
    let dx2 = Float.square (t1.x -. t2.x) in
    let dy2 = Float.square (t1.y -. t2.y) in
    let dz2 = Float.square (t1.z -. t2.z) in
    Float.sqrt (dx2 +. dy2 +. dz2)
  ;;

  let from_str str =
    let nums =
      String.split ~on:',' str
      |> List.map ~f:String.strip
      |> List.filter_map ~f:Float.of_string_opt
    in
    match nums with
    | [ x; y; z ] -> create x y z
    | _ -> failwith "String cannot be parsed into vec3"
  ;;

  let _pp ppf v = Format.fprintf ppf "{x = %f; y = %f; z = %f}" v.x v.y v.z
end

let points =
  In_channel.read_lines "inputs/day_eight.txt"
  |> List.to_array
  |> Array.map ~f:Vec3.from_str
;;

let edges =
  let size = Array.length points in
  List.init size ~f:(fun i -> Array.init (size - i - 1) ~f:(fun j -> i, i + 1 + j))
  |> Array.concat
;;

let sort_by_distance edges =
  Array.sort
    ~compare:(fun (p1, p2) (q1, q2) ->
      let dist1 = Vec3.dist points.(p1) points.(p2) in
      let dist2 = Vec3.dist points.(q1) points.(q2) in
      Float.to_int (dist1 -. dist2))
    edges
;;

(* Disjoint set stuff *)
(* TODO: implement as proper module later *)
(* This is also really slow (and ugly) *)
(* Also i did not handle mutability well at all here, but thats partly also bcs i didnt write a proper module for the Disjoint set stuff *)
(* TODO: actually come back to later pls *)
let parent = Array.init (Array.length points) ~f:(fun x -> x)

let rec root index =
  if parent.(index) = index
  then index
  else (
    let root_of_set = root parent.(index) in
    (* path compression -> flatten the tree so path to root is as small as possible *)
    parent.(index) <- root_of_set;
    parent.(index))
;;

let connect a b = parent.(root a) <- root b

let part_one limit =
  sort_by_distance edges;
  Array.iter ~f:(fun (x, y) -> connect x y) (Array.slice edges 0 limit);
  let acc = Array.init (Array.length points) ~f:(fun _ -> 0) in
  let indices = Array.init (Array.length points) ~f:(fun x -> x) in
  Array.iter ~f:(fun x -> acc.(root x) <- acc.(root x) + 1) indices;
  acc |> Array.sort ~compare:(fun x y -> Int.compare y x);
  Array.slice acc 0 3 |> Array.fold ~init:1 ~f:( * )
;;

let part_two () =
  sort_by_distance edges;
  let join_and_decr x y rf =
    connect x y;
    decr rf
  in
  let circuits = ref (Array.length points) in
  let last_one_idx, last_two_idx =
    Array.fold_until
      ~init:(0, 0)
      ~f:(fun _ (x, y) ->
        let same_root = root x = root y in
        if not same_root then join_and_decr x y circuits;
        match !circuits with
        | 1 when not same_root -> Stop (x, y)
        | 1 when same_root -> Continue (x, y)
        | _ -> Continue (x, y))
      ~finish:(fun fin -> fin)
      edges
  in
  let last_one, last_two = points.(last_one_idx), points.(last_two_idx) in
  Int.of_float (last_one.x *. last_two.x)
;;

let run = function
  | Part_one -> part_one 1000 |> Int.to_string |> print_endline
  | Part_two -> part_two () |> Int.to_string |> print_endline
;;

let () =
  let args = Sys.get_argv () in
  if Array.length args < 2
  then failwith "pls provide which part to run \"one\" or \"two\""
  else (
    match args.(1) with
    | "one" -> run Part_one
    | "two" -> run Part_two
    | _ -> failwith "not a vaild part")
;;
