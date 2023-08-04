exception TypeError of string
exception UnboundVariableError of string
exception OccurCheck of string

let type_error t1 t2 =
  TypeError
    (Format.sprintf "Expected %s but got %s."
       (Ast.Typed.string_of_typ t1)
       (Ast.Typed.string_of_typ t2))
  |> raise

let occur_error str = OccurCheck str |> raise

let unbound_variable var_name =
  TypeError (Format.sprintf "Unbound variable %s." var_name) |> raise
