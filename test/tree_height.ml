let rec h t = 
  match t with
  | Empty -> 0
  | Node (l, _, r) ->
    let o1 = h l in
    let o2 = h r in
     1 + max o1 o2