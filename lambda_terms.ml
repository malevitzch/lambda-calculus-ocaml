module Lambda = struct

  let lambda = "λ"

  type term =
    | Var of string
    | App of term * term
    | Abs of string * term

    let rec term_to_str (t: term) : string =
      match t with
        | Var x -> x
        | App (t1, t2) ->
          (match t1 with 
            | Var y -> y
            | _ -> "(" ^ (term_to_str t1) ^ ")")
          ^
          (match t2 with
            | Var y -> y
            | _ -> "(" ^ (term_to_str t2) ^ ")")
        | Abs (x, t1) -> lambda ^ x ^ "." ^ (term_to_str t1)

let identity : term = Abs ("x", Var "x")
end
