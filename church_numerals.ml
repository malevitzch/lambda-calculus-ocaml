(*
  Recursive definition of a church numeral
*)
let rec church n =
  if n = 0 
    then fun f x -> x
    else fun f x -> f (church (n - 1) f x)

(* 
  Conversion from church numerals to integers
  is done by simply counting how many times a numeral applies
  its function (we do it by applying step() to 0 n times)
*)
let eval n = n (fun x -> x + 1) 0


(*
  Addition subsitutes x with f applied m times for x in n,
  meaning that the function will be applied a total of n + m times
*)
let add n m = fun f x -> n f ( m f x )

(*
  Multiplication substitutes the function f^m for x in n,
  meaning that the function is applied a total of n * m times
*)
let mul n m = fun f x -> n (m f) x


(*
  This one just straight up applies the function lambda x. mul n x
  to "1" m times, resulting in n^m
*)
let exp1 n m = fun f x -> m (mul n) (church 1) f x

(*
  This one is the canonical and "simple" form that the version above
  is equivalent to, at least for terms that are actually church numerals
  (those 2 have different beta-normal forms, I'm pretty sure)
*)
let exp n m = fun f x -> m n f x
(*
  The zero test used here returns a the canonical
  "true" or "false" lambda calculus combinators
  as a result of checking
  How it works is applying the function "lambda x. false"
  a total of n times to "true", meaning that the result will be
  "true" only if the function has been applied 0 times 
*)
let is_zero n = n (fun a -> (fun x y -> y)) (fun x y -> x)

