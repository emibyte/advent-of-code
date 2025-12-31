open Core
open Stdio

module Present = struct
  type t = bool array array

  let from_string_list (lines : string list) : t =
    let lines = List.to_array (List.drop lines 1) in
    Array.map lines ~f:(fun line ->
      Array.map (String.to_array line) ~f:(fun c -> Char.equal c '#'))
  ;;
end

module Area = struct
  type t =
    { width : int
    ; height : int
    ; presents : int array
    }

  let from_string (str : string) : t =
    let parse_area s =
      match String.split ~on:'x' s with
      | [ width; height ] -> Int.of_string width, Int.of_string height
      | _ -> failwith "not a vaild area"
    in
    let parse_indices s =
      let arr = Array.init 6 ~f:(fun _ -> 0) in
      List.iteri (String.split ~on:' ' s) ~f:(fun i x -> arr.(i) <- Int.of_string x);
      arr
    in
    match String.split ~on:':' str |> List.map ~f:String.strip with
    | [ x; y ] ->
      let width, height = parse_area x in
      let indices = parse_indices y in
      { width; height; presents = indices }
    | _ :: _ -> assert false
    | [] -> assert false
  ;;
end

let parse input =
  let lines = In_channel.read_lines input in
  let rec aux acc lst =
    let is_present = function
      | s :: _ -> String.length s >= 2 && Char.equal (String.get s 1) ':'
      | _ -> failwith "we're cooked"
    in
    let is_area = function
      | s :: _ -> Option.is_some (String.index s 'x')
      | _ -> failwith "we're cooked"
    in
    let rest =
      match lst with
      | "" :: y -> y
      | _ -> lst
    in
    match List.split_while rest ~f:(fun x -> String.is_empty x |> not) with
    | [], [] -> List.rev (fst acc), snd acc
    | l1, l2 when is_present l1 ->
      aux (Present.from_string_list l1 :: fst acc, snd acc) l2
    | l1, l2 when is_area l1 ->
      let areas = List.map l1 ~f:Area.from_string in
      aux (fst acc, areas) l2
    | _ -> assert false
  in
  aux ([], []) lines
;;

let part_one () =
  let _, areas = parse "inputs/day_twelve.txt" in
  List.filter areas ~f:(fun area ->
    let available = area.width / 3 * (area.height / 3) in
    let needed = Array.sum (module Int) area.presents ~f:(fun x -> x) in
    available >= needed)
  |> List.length
;;
