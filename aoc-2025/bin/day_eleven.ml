open Core
open Stdio

module Node = struct
  type t =
    { name : string
    ; outputs : string list
    }

  let from_str (line : string) : t =
    let two_parts =
      String.split ~on:':' line |> List.map ~f:(fun word -> String.strip word)
    in
    match List.split_n two_parts 1 with
    | x :: [], y :: [] -> { name = x; outputs = String.split ~on:' ' y }
    | _ -> failwith "not a valid node"
  ;;
end

module Graph = struct
  (** Map a nodes name to a node
      equivalent to: type t = (string, Node.t, Map.comparator_witness) Map.t
  *)
  type t = Node.t Map.M(String).t

  let empty : t = Map.empty (module String)

  let create (alist : (string * Node.t) list) : t option =
    match Map.of_alist (module String) alist with
    | `Ok map -> Some map
    | `Duplicate_key _ -> None
  ;;

  let add (node : Node.t) (node_map : t) : t =
    match Map.add node_map ~key:node.name ~data:node with
    | `Ok new_node_map -> new_node_map
    | `Duplicate -> node_map
  ;;
end

let parse input =
  In_channel.read_lines input
  |> List.map ~f:(fun line ->
    let node = Node.from_str line in
    node.name, node)
  |> Graph.create
  |> Option.value_exn
;;

let part_one () =
  let graph = parse "inputs/day_eleven.txt" in
  (* this is a hack i increased it until the value didnt change (not good :c)
     probably could also use a seen hashset?
  *)
  let max_steps = 2 * (Map.length graph + 1) in
  let start_point = Map.find_exn graph "you" in
  let queue = Queue.create () in
  Queue.enqueue queue start_point;
  let rec aux paths_to_out dist =
    if dist = max_steps
    then paths_to_out
    else (
      let neighbors =
        match Queue.dequeue queue with
        | Some x -> x.outputs
        | None -> []
      in
      let ended = List.count ~f:(fun name -> String.equal name "out") neighbors in
      let to_explore = List.filter ~f:(fun name -> String.( <> ) name "out") neighbors in
      List.iter to_explore ~f:(fun name ->
        let node =
          match Map.find graph name with
          | Some x -> x
          | None -> assert false
        in
        Queue.enqueue queue node);
      aux (paths_to_out + ended) (dist + 1))
  in
  aux 0 0
;;

(* NOTE: as opposed to part one we actually need to find three segments that need to be together
         1. svr -> dac
         2. dac -> fft
         3. fft -> out

         OR
         1. svr -> fft
         2. fft -> dac
         3. dac -> out
*)
let part_two () =
  let graph = parse "inputs/day_eleven.txt" in
  let rec aux (cur : string) (seen : (string, int) Hashtbl.t) (goal : string) : int =
    if Hashtbl.mem seen cur
    then Hashtbl.find_exn seen cur
    else if String.equal cur goal
    then 1
    else (
      let neighbors =
        match Map.find graph cur with
        | Some nodes -> nodes.outputs
        | None -> []
      in
      let res =
        List.sum
          (module Int)
          (List.map ~f:(fun n -> aux n seen goal) neighbors)
          ~f:(fun x -> x)
      in
      Hashtbl.add_exn seen ~key:cur ~data:res;
      res)
  in
  let svr_to_dac = aux "svr" (Hashtbl.create (module String)) "dac" in
  let dac_to_fft = aux "dac" (Hashtbl.create (module String)) "fft" in
  let fft_to_out = aux "fft" (Hashtbl.create (module String)) "out" in
  let first_total = svr_to_dac * dac_to_fft * fft_to_out in
  let svr_to_fft = aux "svr" (Hashtbl.create (module String)) "fft" in
  let fft_to_dac = aux "fft" (Hashtbl.create (module String)) "dac" in
  let dac_to_out = aux "dac" (Hashtbl.create (module String)) "out" in
  let second_total = svr_to_fft * fft_to_dac * dac_to_out in
  first_total + second_total
;;
