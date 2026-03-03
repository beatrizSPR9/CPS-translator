let rec fact n k =
  if n = 0 then k 1
  else fact (n-1) (fun x -> k (n * x))


let rec fact_exc n k =
  if n <= 0 then fail
  else if n = 0 then k 1
  else fact_exc (n-1) (fun x -> k (n * x))