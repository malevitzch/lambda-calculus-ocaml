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

  let get (mc: 'a maybe_changed) : 'a =
    match mc with
      | Unchanged c | Changed c -> c

  let rec substitute (target: term) (var: string) (t: term) : term = 
    match t with 
      | Var x -> if x = var then target else t
      | App (t1, t2) -> App (substitute target var t1, substitute target var t2)
      | Abs (x, t1) -> if x = var then t else Abs (x, substitute target var t1)

  let is_beta_redex (t: term) : bool = 
    match t with
      | App (Abs (_, _), _) -> true
      | _ -> false

  let rec standard_reduction (t: term) : term maybe_changed = 
    match t with
    | Var _ -> Unchanged t
    | App (Abs (x, t1), t2) -> Changed (substitute t2 x t1)
    | Abs (x, t1) -> 
      (match (standard_reduction t1) with 
      | Unchanged t2 -> Unchanged (Abs (x, t2))
      | Changed t2 -> Changed (Abs (x, t2)))
    | App (t1, t2) ->
      let rt1 = (standard_reduction t1) in 
      (match rt1 with
      | Changed t1 -> Changed (App (t1, t2))
      | Unchanged t1 ->
        let rt2 = (standard_reduction t2) in
        (match rt2 with
        | Changed t2 -> Changed (App (t1, t2))
        | Unchanged t2 -> Unchanged (App (t1, t2))))


let identity : term = Abs ("x", Var "x")
let omega : term = Abs ("x", App (Var "f", App (Var "x", Var "x")))
let bigomega : term = App (omega, omega)
end
