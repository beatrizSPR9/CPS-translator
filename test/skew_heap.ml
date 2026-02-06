(* type 'a tree = Empty | Node of 'a tree * 'a * 'a tree *)

let create = Empty

let get_min t =
  match t with
  | Empty -> assert false
  | Node (_, x, _) -> x

let rec merge t1 t2 =
  match t1 with
  | Empty -> t2
  | Node (l1, x1, r1) ->
    match t2 with
    | Empty -> t1
    | Node (l2, x2, r2) ->
      if x1 <= x2 then
        let ll = merge r1 t2 in
        Node (ll, x1, l1)
      else
        let ll = merge r2 t1 in
        Node (ll, x2, l2)

let add x t =
  merge (Node (Empty, x, Empty)) t

let remove_min t =
  match t with
  | Empty -> assert false
  | Node (l, _, r) -> merge l r

let main =
  let h = create in
  let h = add 0 h in
  let h = add 1 h in
  remove_min h
