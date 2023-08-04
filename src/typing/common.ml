open Ast.Typed

let to_be_level_adjusted : typ list ref = ref []
let reset_level_adjustment () = to_be_level_adjusted := []

(* Chase the links of bound variables, returning either a free variable or a constructed type *)
let rec head = function
  | TVar ({ contents = Link t } as var) ->
      let t = head t in
      var := Link t;
      t
  | t -> t

(* get the level of a normalized type, which is not a bound TVar *)
let get_level = function
  | TVar { contents = Unbound (_, l) } -> l
  | TArrow (_, _, ls) -> ls.new_level
  | _ -> assert false

let genname_count = ref 0

let genname () =
  let n = !genname_count in
  incr genname_count;
  if n < 26 then "'" ^ (Char.code 'a' + n |> Char.chr |> String.make 1)
  else "t" ^ string_of_int n

(* Determining the let nesting level *)
let current_level = ref 1

let reset_type_variables () =
  genname_count := 0;
  current_level := 1

let enter_level () = incr current_level
let leave_level () = decr current_level

(* Make a fresh type variable and an arrow type *)
let newvar () = TVar (Unbound (genname (), !current_level) |> ref)

let new_arrow t1 t2 =
  TArrow (t1, t2, { new_level = !current_level; old_level = !current_level })
