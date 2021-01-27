let () =
  Load_path.init [ "/usr/lib/ocaml/" ]

let refactor (path : string) (ignored : string list) =
  Describe.iter_module_descrs ~path ~f:Refactor.refactor ~ignored

let annotate (path : string) (typ_str : string) (ignored : string list) =
  let typ_match = Typ_utils.parse_typ typ_str in
  Describe.iter_module_descrs ~path ~f:(Annotate.annotate ~typ_match) ~ignored

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
