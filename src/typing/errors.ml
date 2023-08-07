open Ast.Typed
open Ast.Common

exception TypeError of string
exception OccurCheck of string

let type_error t1 t2 pos =
  TypeError
    (Format.sprintf "Expected `%s` but got `%s`\n       Character: %d"
       (string_of_typ t1) (string_of_typ t2) pos.start)
  |> raise

let unify_error t1 pos =
  TypeError
    (Format.sprintf
       "Type `%s` is not a function. It cannot be applied\n       Character: %d"
       (Ast.Typed.string_of_typ t1)
       pos.start)
  |> raise

let occur_error str = OccurCheck str |> raise

let unbound_variable var_name pos =
  TypeError
    (Format.sprintf "Unbound variable `%s`\n       Character: %d" var_name
       pos.start)
  |> raise
