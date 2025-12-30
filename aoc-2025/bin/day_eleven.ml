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

let () = print_endline "Hello World"
