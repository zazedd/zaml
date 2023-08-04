open Common

type t =
  | Unit
  | Int of int
  | Bool of bool
  | Var of variable
  | Let of variable * t * t option
  | Lambda of variable * t
  | Fun of variable * variable * t * t option
  | App of t * t

(* type t = *)
(*   | Unit *)
(*   | Int of int *)
(*   | Bool of bool *)
(*   | Var of variable *)
(*   | Let of { name : variable; binding : t; in_body : t option } *)
(*   | Lambda of { vars : variable list; body : t } *)
(*   | Fun of { *)
(*       name : variable; *)
(*       vars : variable list; *)
(*       binding : t; *)
(*       in_body : t option; *)
(*     } *)
(*   | App of t * t *)
