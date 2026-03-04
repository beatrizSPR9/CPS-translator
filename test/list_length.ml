let rec length l = 
  match l with
  | [] -> 0
  | _ :: r -> 
      let o = length r in
        1 + o1