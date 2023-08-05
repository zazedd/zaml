open Typing.Env

let handle_args () =
  match Array.length Sys.argv with
  | 1 -> Repl.Cli.run Ctx.empty
  | 2 -> (
      let arg = Sys.argv.(1) in
      match Filename.extension arg with
      | ".zml" -> Interpreter.Interp.run arg
      | _ -> failwith "Please only provide .zml files")
  | _ -> failwith "Too many arguments"

let _ = handle_args ()
