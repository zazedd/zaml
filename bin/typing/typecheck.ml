open Env
open Unify
open Common
open Ast.Parsed
open Ast.Typed

let rec typeof ctx = function
  | Unit -> TUnit
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var x -> inst (List.assoc x ctx)
  | Lambda (x, e) ->
      let ty_x = newvar () in
      let ty_e = typeof ((x, ty_x) :: ctx) e in
      new_arrow ty_x ty_e
  | App (e1, e2) ->
      let ty_fun = typeof ctx e1 in
      let ty_arg = typeof ctx e2 in
      let ty_res = newvar () in
      unify ty_fun (new_arrow ty_arg ty_res);
      ty_res
  | Let (x, e, e2) -> (
      enter_level ();
      let ty_e = typeof ctx e in
      leave_level ();
      gen ty_e;
      match e2 with
      | Some e2 -> typeof ((x, ty_e) :: ctx) e2
      | None -> assert false)
  | _ -> assert false
