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
