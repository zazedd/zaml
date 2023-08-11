open Ast.Parsed

type variable = string

module ECtx = Map.Make (String)

type ectx = value ECtx.t

and value =
  | VUnit
  | VInt of int
  | VChar of char
  | VString of string
  | VBool of bool
  | VList of value list
  | Closure of { vars : variable list; body : expr; context : ectx }
