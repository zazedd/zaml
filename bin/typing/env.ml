open Common
open Ast.Typed
open Ast.Common

(* The type environment *)
type env = (variable * typ) list

(*
  Sound generalization: Quantify free TVars with higher levels only from dead regions.
  Traverse parts with higher-level TVars.
  Components with level <= current_level don't need traversal.
  After generalization, type gets generic_level if any component is quantified.
  Perform pending level updates for potential quantification.
*)
let force_delayed_adjustments () =
  let rec loop acc level ty =
    match head ty with
    | TVar ({ contents = Unbound (name, l) } as tvr) when l > level ->
        tvr := Unbound (name, level);
        acc
    | TArrow (_, _, ls) when ls.new_level = marked_level ->
        failwith "occurs check"
    | TArrow (_, _, ls) as ty ->
        if ls.new_level > level then ls.new_level <- level;
        adjust_one acc ty
    | _ -> acc
  (* only deals with composite types *)
  and adjust_one acc = function
    | TArrow (_, _, ls) as ty when ls.old_level <= !current_level ->
        ty :: acc (* update later *)
    | TArrow (_, _, ls) when ls.old_level = ls.new_level ->
        acc (* already updated *)
    | TArrow (ty1, ty2, ls) ->
        let level = ls.new_level in
        ls.new_level <- marked_level;
        let acc = loop acc level ty1 in
        let acc = loop acc level ty2 in
        ls.new_level <- level;
        ls.old_level <- level;
        acc
    | _ -> assert false
  in
  to_be_level_adjusted := List.fold_left adjust_one [] !to_be_level_adjusted

let gen ty =
  force_delayed_adjustments ();
  let rec loop ty =
    match head ty with
    | TVar ({ contents = Unbound (name, l) } as tvr) when l > !current_level ->
        tvr := Unbound (name, generic_level)
    | TArrow (ty1, ty2, ls) when ls.new_level > !current_level ->
        let ty1 = head ty1 and ty2 = head ty2 in
        loop ty1;
        loop ty2;
        let l = max (get_level ty1) (get_level ty2) in
        ls.old_level <- l;
        ls.new_level <- l (* set the exact level upper bound *)
    | _ -> ()
  in
  loop ty

(* instantiation: replace schematic variables with fresh TVars.
   Only the components at generic_level are traversed, since only
   those may contain quantified type variables.
*)
let inst =
  let rec loop subst = function
    | TVar { contents = Unbound (name, l) } when l = generic_level -> (
        try (List.assoc name subst, subst)
        with Not_found ->
          let tv = newvar () in
          (tv, (name, tv) :: subst))
    | TVar { contents = Link ty } -> loop subst ty
    | TArrow (ty1, ty2, ls) when ls.new_level = generic_level ->
        let ty1, subst = loop subst ty1 in
        let ty2, subst = loop subst ty2 in
        (new_arrow ty1 ty2, subst)
    | ty -> (ty, subst)
  in
  fun ty -> fst (loop [] ty)
