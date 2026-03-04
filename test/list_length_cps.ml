let rec length_cps l k =
  match l with
  | [] -> k 0
  | _ :: r -> 
      length_cps r (fun o ->k (1 + o))