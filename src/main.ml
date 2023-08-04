open Typing.Env

let handle_args () =
  match Array.length Sys.argv with
  | 1 -> Repl.Cli.run Ctx.empty
  | 2 ->
      let _ = Sys.argv.(1) in
      assert false
  | _ -> failwith "Too many arguments"

let () = handle_args ()
