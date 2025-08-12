let successor n x f = f (n x f)

let rec parigot n x f =
  if n = 0
    then x
    else successor (parigot (n - 1)) x f

let eval n = n 0 (fun x -> x + 1)
