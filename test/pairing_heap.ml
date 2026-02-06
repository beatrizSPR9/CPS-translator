(* type heap = Empty | Node of tree *)
(* and tree = T of int * tree list *)

let empty = Empty

let is_empty h =
  match h with
  | Empty -> true
  | Node _ -> false

let get_min h =
  match h with
  | Empty -> assert false
  | Node T (x, _) -> x

let merge h1 h2 =
  match h1 with
  | Empty -> h2
  | Node T (x, t1) ->
    match h2 with
    | Empty -> h1
    | Node T (y, t2) ->
      if x <= y then
        Node (T (x, T (y, t2) :: t1))
      else
        Node (T (y, T (x, t1) :: t2))

let insert x h =
  merge (Node (T (x, []))) h

let rec merge_pairs t =
  match t with
  | [] -> Empty
  | t :: [] -> Node t
  | t1 :: t2 :: r ->
    let rm = merge_pairs r in
    let t = merge (Node t1) (Node t2) in
    merge t rm

let remove_min h =
  match h with
  | Empty -> assert false
  | Node T (_, l) -> merge_pairs l
