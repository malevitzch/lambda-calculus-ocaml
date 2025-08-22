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
            | App _ -> term_to_str t1
            | Abs _ -> "(" ^ (term_to_str t1) ^ ")")
          ^
          (match t2 with
            | Var y -> y
            | _ -> "(" ^ (term_to_str t2) ^ ")")
        | Abs (x, t1) -> lambda ^ x ^ "." ^ (term_to_str t1)

  type 'a maybe_changed = Changed of 'a | Unchanged of 'a

  let rec substitute (target: term) (var: string) (t: term) : term = 
    match t with 
      | Var x -> if x = var then target else t
      | App (t1, t2) -> App (substitute target var t1, substitute target var t2)
      | Abs (x, t1) -> if x = var then t else Abs (x, substitute target var t1)

let identity : term = Abs ("x", Var "x")
end
