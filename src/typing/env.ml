open Common
open Errors
open Ast.Typed
open Ast.Common

(* The type environment Gamma *)
module Ctx = Map.Make (String)

(*
  Sound generalization: Quantify free TVars with higher levels only from dead regions.
  Traverse parts with higher-level TVars.
  Components with level <= current_level don't need traversal.
  After generalization, type gets generic_level if any component is quantified.
  Perform pending level updates for potential quantification.
*)
let lazy_adjust () =
  let rec loop acc level t =
    match head t with
    | TVar ({ contents = Unbound (name, l) } as var) when l > level ->
        var := Unbound (name, level);
        acc
    | TArrow (_, _, ls) when ls.new_level = marked_level ->
        occur_error "Variable occurs inside its definition"
    | TArrow (_, _, ls) as t ->
        if ls.new_level > level then ls.new_level <- level;
        adjust_one acc t
    | _ -> acc
  (* only deals with composite types *)
  and adjust_one acc = function
    | TArrow (_, _, ls) as t when ls.old_level <= !current_level ->
        t :: acc (* update later *)
    | TArrow (_, _, ls) when ls.old_level = ls.new_level ->
        acc (* already updated *)
    | TArrow (t1, t2, ls) ->
        let level = ls.new_level in
        ls.new_level <- marked_level;
        let acc = loop acc level t1 in
        let acc = loop acc level t2 in
        ls.new_level <- level;
        ls.old_level <- level;
        acc
    | _ -> assert false
  in
  to_be_level_adjusted := List.fold_left adjust_one [] !to_be_level_adjusted

let gen t =
  lazy_adjust ();
  let rec loop t =
    match head t with
    | TVar ({ contents = Unbound (name, l) } as var) when l > !current_level ->
        var := Unbound (name, generic_level)
    | TArrow (t1, t2, ls) when ls.new_level > !current_level ->
        let t1 = head t1 in
        let t2 = head t2 in
        loop t1;
        loop t2;
        let l = max (get_level t1) (get_level t2) in
        ls.old_level <- l;
        ls.new_level <- l (* set the exact level upper bound *)
    | _ -> ()
  in
  loop t

(*
   instantiation: replace schematic variables with fresh TVars.
   Only the components at generic_level are traversed, since only
   those may contain quantified type variables.
*)
let inst t =
  let rec loop ctx = function
    | TVar { contents = Unbound (name, l) } when l = generic_level -> (
        try (Ctx.find name ctx, ctx)
        with Not_found ->
          let var = newvar () in
          let ctx' = Ctx.add name var ctx in
          (var, ctx'))
    | TVar { contents = Link t } -> loop ctx t
    | TArrow (t1, t2, ls) when ls.new_level = generic_level ->
        let t1, subst = loop ctx t1 in
        let t2, subst = loop subst t2 in
        (new_arrow t1 t2, subst)
    | t -> (t, ctx)
  in
  loop Ctx.empty t |> fst
