let rec height t k =
  match t with
  | Empty -> k 0
  | Node (l, _, r) ->
      height l (fun o1 ->
      height r (fun o2 ->
      k (1 + max o1 o2)))