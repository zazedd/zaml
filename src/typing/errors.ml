open Ast.Typed
open Ast.Common

exception TypeError of string
exception OccurCheck of string

let which_chars start ending =
  if start = ending then Format.sprintf "Character %d" start
  else Format.sprintf "Characters %d-%d" start ending

let type_error t1 t2 pos =
  TypeError
    (Format.sprintf "Expected `%s` but got `%s`\n       %s" (string_of_typ t1)
       (string_of_typ t2)
       (which_chars pos.start pos.ending))
  |> raise

let op_error t1 t2 pos =
  TypeError
    (Format.sprintf
       "Expected `%s` but got `%s`. Operators require the same type on both \
        sides\n\
       \       %s" (string_of_typ t1) (string_of_typ t2)
       (which_chars pos.start pos.ending))
  |> raise

let unify_error t1 pos =
  TypeError
    (Format.sprintf
       "Type `%s` is not a function. It cannot be applied\n       %s"
       (Ast.Typed.string_of_typ t1)
       (which_chars pos.start pos.ending))
  |> raise

let occur_error str = OccurCheck str |> raise

let unbound_variable var_name pos =
  TypeError
    (Format.sprintf "Unbound variable `%s`\n       %s" var_name
       (which_chars pos.start pos.ending))
  |> raise
