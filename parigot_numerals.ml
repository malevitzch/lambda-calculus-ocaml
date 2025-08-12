let successor n x f = f (n x f)

let rec parigot n x f =
  if n = 0
    then x
    else f ((parigot (n - 1)) x f)

let eval n = n 0 (fun x -> x + 1)


let pred n x f =
  let g = fun p -> (true, if fst p then f x else x) in
    snd (n (false, x) g)
