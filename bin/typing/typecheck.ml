open Ast.Parsed
open Ast.Typed
open Unify
open Env

let rec typeof ctx = function
  | Unit -> TUnit
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var x -> inst (List.assoc x ctx)
  | Lambda { vars; body } ->
      let tx = newvar () in
      let ty_e = typeof ((x, ty_x) :: ctx) e in
      new_arrow ty_x ty_e
  | App (e1, e2) ->
      let ty_fun = typeof ctx e1 in
      let ty_arg = typeof ctx e2 in
      let ty_res = newvar () in
      unify ty_fun (new_arrow ty_arg ty_res);
      ty_res
  | Let (x, e, e2) ->
      enter_level ();
      let ty_e = typeof ctx e in
      leave_level ();
      gen ty_e;
      typeof ((x, ty_e) :: ctx) e2
