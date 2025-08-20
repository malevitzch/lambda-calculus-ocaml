module Lambda = struct

  let lambda = "λ"

  type term =
    | Var of string
    | App of term * term
    | Abs of string * term

    let rec print_term (t: term) : unit =
      match t with
        | Var x ->
          print_string x;
        | App (t1, t2) ->
          print_string "(";
          print_term t1;
          print_string ")";
          print_string "(";
          print_term t2;
          print_string ")";
        | Abs (x, t1) ->
          print_string lambda;
          print_string x;
          print_string ".";
          print_term t1

let identity : term = Abs ("x", Var "x")
end
