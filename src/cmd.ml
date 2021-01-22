
let refactor (init_path : string) =
  let _ = Printf.sprintf "cd %s" init_path |> Sys.command in
  let () = Describe.iter_module_descrs ~f:Refactor.refactor in
  ignore (Sys.command "cd -")

open Cmdliner

let init_path =
  let doc = "Path to the directory to refactor." in
  Arg.(value & pos 0 string "" & info [] ~docv:"PATH" ~doc)

let refactor_t = Term.(const refactor $ init_path)

let info =
  let doc = "Refactor float array to floatarray in the given dune directory." in
  Term.info "refactor" ~version "0.1" ~doc ~exits:Term.default_exits

let () = Term.exit @@ Term.eval (refactor_t, info)

