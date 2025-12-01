open Core
open Stdio
open Str

let input = In_channel.read_all "inputs/day3.txt"

let part_1 input =
  let regex = regexp {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))|} in
  let rec aux acc pos =
    (* NOTE: this is not exactly beautiful (using an exception for control flow) *)
    try
      let position = search_forward regex input pos in
      let lhs = matched_group 1 input |> Int.of_string in
      let rhs = matched_group 2 input |> Int.of_string in
      aux ((lhs, rhs) :: acc) (position + 1)
    with
    | _ -> acc
  in
  aux [] 0 |> List.fold_left ~init:0 ~f:(fun acc (x, y) -> acc + (x * y))
;;

let () = part_1 input |> Int.to_string |> print_endline

let part_2 input =
  let regex =
    regexp {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))\|do()\|don't()|}
  in
  let rec aux acc allowed pos =
    try
      let position = search_forward regex input pos in
      let new_search_start = position + 1 in
      match matched_string input with
      | "do()" -> aux acc true new_search_start
      | "don't()" -> aux acc false new_search_start
      | _ ->
        if allowed
        then
          aux
            (* should try refactoring this into a function *)
            (( matched_group 1 input |> Int.of_string
             , matched_group 2 input |> Int.of_string )
             :: acc)
            allowed
            new_search_start
        else aux acc allowed new_search_start
    with
    | _ -> acc
  in
  aux [] true 0 |> List.fold ~init:0 ~f:(fun acc (x, y) -> acc + (x * y))
;;

let () = part_2 input |> Int.to_string |> print_endline
