let rec h t k =
  match t with
  | Empty -> k 0
  | Node (l, _, r) ->
      h l (fun o1 ->
      h r (fun o2 ->
      k (1 + max o1 o2)))
