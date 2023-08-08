open Ast.Parsed

type variable = string

module ECtx = Map.Make (String)

type ectx = value ECtx.t

and value =
  | VUnit
  | VInt of int
  | VBool of bool
  | Closure of { vars : variable list; body : expr; context : ectx }

module GenCtx = Map.Make (struct
  type t = string * int

  let compare = compare
end)

type genctx = value GenCtx.t
