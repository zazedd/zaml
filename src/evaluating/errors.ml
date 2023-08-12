exception RuntimeError of string

let not_a_value () = RuntimeError "Not a value" |> raise

let unbound_variable x =
  RuntimeError (Format.sprintf "Unbound variable `%s`" x) |> raise

let if_guard_error () = RuntimeError "If guard must be a boolean" |> raise

let op_error () =
  RuntimeError "Operator requires the same type on both sides" |> raise

let too_many_args num =
  RuntimeError
    (Format.sprintf
       "Too many arguments, the function only requires %d arguments." num)
  |> raise

let app_error () =
  RuntimeError "First parameter of application is not a function" |> raise

let range_error () =
  RuntimeError "Range can't start with the biggest item" |> raise
