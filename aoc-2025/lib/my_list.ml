let flatten lst =
  let rec aux acc = function
    | (x :: y) :: tl -> aux (x :: acc) (y :: tl)
    | [] :: tl -> aux acc tl
    | [] -> acc
  in
  List.rev (aux [] lst)
;;

let%test _ = flatten [ [ 1; 2; 3 ]; [ 1 ]; [ 2; 4 ] ] = [ 1; 2; 3; 1; 2; 4 ]
