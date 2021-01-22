let refactor (init_path : string) (ignored : string list) =
  let _ = Printf.sprintf "cd %s" init_path |> Sys.command in
  let () = Describe.iter_module_descrs ~f:Refactor.refactor ~ignored in
  ignore (Sys.command "cd -")

let annotate (init_path : string) (typ_str : string) (ignored : string list) =
  let _ = Printf.sprintf "cd %s" init_path |> Sys.command in
  let typ_match = Typ_utils.parse_typ typ_str in
  let () = Describe.iter_module_descrs ~f:(Annotate.annotate ~typ_match) ~ignored in
  ignore (Sys.command "cd -")

open Cmdliner

let init_path =
  let doc = "Path to the directory to refactor." in
  Arg.(value & pos 0 string "" & info [] ~docv:"PATH" ~doc)

let ignored =
  let doc = "List of files to ignore." in
  Arg.(value & opt (list string) [] & info [ "i"; "ignore" ] ~docv:"IGNORE" ~doc)

let refactor_t = Term.(const refactor $ init_path $ ignored)

let info =
  let doc = "Refactor float array to floatarray in the given dune directory." in
  Term.info "refactor" ~version:"0.1" ~doc ~exits:Term.default_exits

let () = Term.exit @@ Term.eval (refactor_t, info)
