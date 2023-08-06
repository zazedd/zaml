open Common

type t =
  | Unit
  | Int of int
  | Bool of bool
  | Var of variable
  | If of t * t * t
  | Let of { name : variable; binding : t; in_body : t option }
  | Lambda of { vars : variable list; body : t }
  | Fun of {
      name : variable;
      vars : variable list;
      binding : t;
      in_body : t option;
    }
  | App of t * t

let rec string_of_ast = function
  | Unit -> "unit"
  | Int i -> "int : " ^ string_of_int i
  | Bool b -> "bool : " ^ string_of_bool b
  | Var s -> "var name : " ^ s
  | Let { name; binding; in_body } -> (
      "let : name : " ^ name ^ "\n" ^ string_of_ast binding
      ^ match in_body with None -> "" | Some b -> string_of_ast b)
  | Lambda { vars; body } ->
      List.fold_left (fun acc a -> acc ^ " " ^ a) "Lambda :\nvars : \n" vars
      ^ "\nbody : " ^ string_of_ast body
  | Fun { name; vars; binding; in_body } -> (
      "Fun name : " ^ name
      ^ List.fold_left (fun acc a -> acc ^ " " ^ a) "\nvars : \n" vars
      ^ "\nbinding : " ^ string_of_ast binding ^ "\nbody : "
      ^ match in_body with None -> "" | Some b -> string_of_ast b)
  | App (e1, e2) -> "App : " ^ string_of_ast e1 ^ " and " ^ string_of_ast e2
  | _ -> assert false
