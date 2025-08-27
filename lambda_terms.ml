module Lambda = struct

  let lambda = "λ"

  (*
    A term is either a variable, an application of one term to another,
    or an abstraction in the form of λx.M, which basically reads:
    "for any x give M", where M might contain x inside itself
  *)
  type term =
    | Var of string
    | App of term * term
    | Abs of string * term


  let (@>) (x: string) (t: term) : term = Abs (x, t)
  let (&@) (t1: term) (t2: term) : term = App (t1, t2)

  (*
    Conversion of lambda terms to strings
    Avoids printing unnecessary parentheses
  *)
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


  module StringSet = Set.Make(String)

  (*
    Function for calculating the set of free variables of a term
  *)
  let rec free_variables (t: term) : StringSet.t = 
    match t with
    | Var x -> StringSet.singleton x
    | App (t1, t2) -> StringSet.union (free_variables t1) (free_variables t2)
    | Abs (x, t) -> StringSet.remove x (free_variables t)
  type 'a maybe_changed =
    | Changed of 'a
    | Unchanged of 'a

  let rec all_variables (t: term) : StringSet.t =
    match t with
    | Var x -> StringSet.singleton x
    | App (t1, t2) -> StringSet.union (all_variables t1) (all_variables t2)
    | Abs (x, t) -> StringSet.add x (all_variables t)

  let is_changed (mc: 'a maybe_changed) : bool =
    match mc with
    | Changed c -> true
    | Unchanged uc -> false

  type mterm = term maybe_changed

  let get (mc: 'a maybe_changed) : 'a =
    match mc with
      | Unchanged c | Changed c -> c

  (*
    Function for performing lambda calculus substitutions.
    We denote subsitutions as M[x := P], which is read as:
    replace every "free" x in M with P. A free variable is one that is not bound
    by an abstraction.

    The arguments are (in order):
      - var: the name of the variable you want to substitute for
      - target: the term that you substitute for said variable
      - t: the term you want to perform the substitution on
  *)
  let rec substitute (var: string) (target: term) (t: term) : term =
    match t with
      | Var x -> if x = var then target else t
      | App (t1, t2) -> substitute var target t1 &@ substitute var target t2
      | Abs (x, t1) -> if x = var then t else x @> substitute var target t1

  let rec alpha_convert (variable: string) (new_variable: string) (t: term) : term =
    match t with 
    | Var x -> if x = variable then Var new_variable else Var x
    | App (t1, t2) -> alpha_convert variable new_variable t1 &@ alpha_convert variable new_variable t2
    | Abs (x, t1) -> if x = variable 
      then new_variable @> alpha_convert variable new_variable (substitute variable (Var new_variable) t1) 
      else x @> alpha_convert variable new_variable t1

  (*
    Auxiliary function to check whether a term is a beta-redex,
    basically a term of form (λx.M)P which, by beta-reduction,
    we treat as nothing else than function application, thus
    (λx.M)P -> M[x := P] (substitute x in M for P).
  *)
  let is_beta_redex (t: term) : bool =
    match t with
      | App (Abs (_, _), _) -> true
      | _ -> false

  (*
    This function performs a single step of the normalizing
    leftmost reduction strategy on term t and returns the result
    along with the information whether any redex has been reduced.

    As a result of the standariztion theorem, repeatedly applying
    this function until no changes are made yields 
    the normal form of the term t (if one exists).

    But beware: the problem of finding whether an untyped lambda calculus term
    has a normal form is undecideable (belongs to RE but not to CORE).

    Therefore repeatedly applying this function is not always a good idea
    since the term might not have a normal form and the program would run forever.
    It's suggested to put an upper limit on the number of reductions you allow.

    And no, there is no "universal" limit that you can calculate based on
    size of the term. This is closely related to the undecideable problem of
    whether a program is a "busy beaver champion".
    For more information, look here: https://en.wikipedia.org/wiki/Busy_beaver

    This function takes a single argument: the term t that you want to perform
    the reduction on.
  *)
  let rec standard_reduction (t: term) : mterm =
    match t with
    | Var _ -> Unchanged t
    | App (Abs (x, t1), t2) -> Changed (substitute x t2 t1) (* beta-reduction *)
    | Abs (x, t1) ->
      (match (standard_reduction t1) with
      | Unchanged t2 -> Unchanged (x @> t1)
      | Changed t2 -> Changed (x @> t2))
    | App (t1, t2) ->
      let rt1 = (standard_reduction t1) in
      (match rt1 with
      | Changed t1 -> Changed (t1 &@ t2)
      | Unchanged t1 ->
        let rt2 = (standard_reduction t2) in
        (match rt2 with
        | Changed t2 -> Changed (t1 &@ t2)
        | Unchanged t2 -> Unchanged (t1 &@ t2)))


  type maybe_normalized =
    | Normalized of term
    | NotNormalized of term

  (*
    Function that attempts to normalize a term using
    the leftmost reduction strategy, but limiting
    the number of beta-reductions to max_steps to avoid endless reductions
    on terms without a normal form.

    The arguments are (in order):
      - max_steps: the maximum number of reductions
      - t: the term you want to normalize

    The function returns:
      - Normalized term - if the function managed to normalize the given term,
        then "term" holds the normal form of t
      - NotNormalized term - if the function failed to normalize the given term,
        then "term" holds the result of applying max_steps reductions to t
  *)
  let normalize (max_steps: int) (t: term) : maybe_normalized =
    let rec normalize_helper (limit: int) (i: int) (t: mterm) : mterm =
      (if i = limit then t else
        match t with
        | Changed t1 -> normalize_helper limit (i+1) (standard_reduction t1)
        | Unchanged t1 -> t) in
    let mt = normalize_helper max_steps 0 (Changed t) in
      match standard_reduction (get mt) with
      | Changed _ -> NotNormalized (get mt)
      | Unchanged _ -> Normalized (get mt)

  (*
    An attempt at making reductions faster, it simultaneously reduces every redex
    but doesn't really run faster than regular standard reduction and still blows
    up quadratically in some cases, so it's not really useful in practice, although
    in specific cases of very "horizontal" terms it might provide a performance benefit
    by doing the reductions in parallel when possible and avoiding unnecessary traversals
    of the whole syntax tree of the term.
  *)
  let rec multi_reduction (t: term) : mterm =
    match t with
    | Var _ -> Unchanged t
    | App (Abs (x, t1), t2) -> Changed (get (multi_reduction (substitute x t2 t1)))
    | Abs(x, t1) ->
      (match (multi_reduction t1) with
      | Changed t1 -> Changed (x @> get (multi_reduction t1))
      | Unchanged t1 -> let rt1 = multi_reduction t1 in 
        if is_changed rt1
          then Changed (x @> get rt1)
          else Unchanged (x @> get rt1))
    | App (t1, t2) -> 
      let a = multi_reduction t1 in
      let b = multi_reduction t2 in
      if is_changed a || is_changed b
        then Changed (get a &@ get b)
        else Unchanged (get a &@ get b)

  (*
    The unsafe version of normalize, doesn't enforce any limits as to the
    maximal number of reduction steps. Use with caution!
  *)
  let normalize_unsafe (t: term) : term =
    let rec normalize_helper (t: mterm) : term =
      (match t with
      | Changed t -> normalize_helper (multi_reduction t)
      | Unchanged t -> t) in
    normalize_helper (Changed t)


  (*
    Some basic lambda-terms
  *)
  module Terms = struct
    let identity : term = "x" @> Var "x"
    let omega : term = "x" @> (Var "x" &@ Var "x")
    let bigomega : term = omega &@ omega

    let church_succ : term =
      "n" @> "f" @> "x" @> (Var "f" &@ ( Var "n" &@ Var "f" &@ Var "x" ))

    let rec church (n: int) : term =
      normalize_unsafe (if n = 0
        then "f" @> ( "x" @> Var "x")
        else church_succ &@ ( church (n - 1)))

    let church_add : term = 
      "n" @> "m" @> "f" @> "x" @> (Var "n" &@ Var "f" &@ (Var "m" &@ Var "f" &@ Var "x") )
  end

end
