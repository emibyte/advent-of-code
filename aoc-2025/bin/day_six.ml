open Core
open Stdio

type op =
  | Add
  | Mult
  | Noop

let apply = function
  | Add -> ( + )
  | Mult -> ( * )
  | Noop -> fun _ _ -> 0
;;

let parse_homework =
  let is_op = function
    | "*" | "+" -> true
    | _ -> false
  in
  let get_op = function
    | "+" -> Add
    | "*" -> Mult
    | _ -> failwith "it's cooked"
  in
  let rec aux acc = function
    | hd :: tl ->
      let acc_lst = snd acc in
      let op = fst acc in
      if is_op hd
      then aux (get_op hd, acc_lst) tl
      else aux (op, Int.of_string hd :: acc_lst) tl
    | [] -> acc
  in
  aux (Noop, [])
;;

let homework_list =
  let input_opt =
    In_channel.read_lines "inputs/day_six.txt"
    |> List.map ~f:(String.split ~on:' ')
    |> List.map ~f:(fun lst -> List.filter ~f:(fun s -> String.is_empty s |> not) lst)
    |> List.transpose
  in
  let input =
    match input_opt with
    | Some x -> x
    | None -> []
  in
  List.map ~f:parse_homework input
;;

let solve_homework homework =
  let get_init_val = function
    | Add -> 0
    | Mult -> 1
    | Noop -> 0
  in
  let op = fst homework in
  let nums = snd homework in
  let func = apply op in
  List.fold ~init:(get_init_val op) ~f:func nums
;;

let part_one = List.map ~f:solve_homework homework_list |> List.fold ~init:0 ~f:( + )
let () = part_one |> Int.to_string |> print_endline

let homework_list_p2 =
  let input =
    In_channel.read_lines "inputs/day_six.txt" |> List.map ~f:String.to_list
  in
  let op_line = Option.value ~default:[] (List.last input) in
  let get_col_lengths ops =
    let is_op ch =
      let open Char in
      ch = '+' || ch = '*'
    in
    let rec aux acc cur = function
      | _ :: (y :: _ as tl) when is_op y -> aux (cur :: acc) 0 tl
      | _ :: (_ :: _ as tl) -> aux acc (cur + 1) tl
      | _ :: [] -> aux acc (cur + 1) []
      | [] -> cur :: acc
    in
    List.rev (aux [] 0 ops)
  in
  let col_lengths = get_col_lengths op_line in
  let rec aux acc inp = function
    | x :: y ->
      let col, rst = List.split_n inp x in
      let rest =
        match rst with
        | _ :: xs -> xs
        | [] -> []
      in
      aux (col :: acc) rest y
    | [] -> List.rev acc
  in
  List.map ~f:(fun lst -> aux [] lst col_lengths) input
  |> List.transpose
  |> Option.value ~default:[]
;;

let parse_homework_p2 homework =
  let get_op = function
    | '+' -> Add
    | '*' -> Mult
    | _ -> failwith "it's cooked"
  in
  let nums = List.drop_last_exn homework in
  let op =
    match List.last_exn homework with
    | x :: _ -> get_op x
    | [] -> Noop
  in
  let chars_to_str chs =
    let buf = Buffer.create (List.length chs) in
    List.iter ~f:(fun c -> Buffer.add_char buf c) chs;
    Buffer.contents buf in
  let numbers =
    List.transpose nums
    |> Option.value ~default:[]
    |> List.map
      ~f:(List.filter_map ~f:(fun x -> if Char.( <> ) ' ' x then Some x else None))
    |> List.map ~f:(fun lst -> chars_to_str lst |> Int.of_string)
  in (op, numbers)
;;

let part_two =
  let parsed = List.map ~f:parse_homework_p2 homework_list_p2 in
  List.map ~f:solve_homework parsed |> List.fold ~init:0 ~f:( + )
