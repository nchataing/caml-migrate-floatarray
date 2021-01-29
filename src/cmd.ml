(******************************************************************)
(* Copyright (C) 2020-2021 Nicolas Chataing. All rights reserved. *)
(*                                                                *)
(* This software may be modified and distributed under the terms  *)
(* of the BSD license.  See the LICENSE file for details.         *)
(******************************************************************)

let () = Load_path.init [ Config.standard_library ]

let build (path : string) =
  Io.with_chdir path (fun () -> Sys.command (Filename.quote_command "dune" [ "build"; "@check" ]))
  |> ignore

let refactor (path : string) (use_get_set : string option) (ignored : string list) =
  build path;
  Describe.iter_module_descrs ~path ~f:(Refactor.refactor ~use_get_set) ~ignored

let annotate (path : string) (typ_str : string) (ignored : string list) : bool =
  build path;
  let typ_match = Typ_utils.parse_typ typ_str in
  let has_changed = ref false in
  Describe.iter_module_descrs ~path ~f:(Annotate.annotate ~typ_match ~has_changed) ~ignored;
  !has_changed

let create_floatarray_library (path : string) (lib_name : string) : unit =
  Io.with_chdir path (fun () ->
      let ok = ref false in
      try
        if Sys.is_directory lib_name then
          Printf.printf "%s does already exists in target repository.\n" lib_name
        else ok := true
      with Sys_error _ ->
        ok := true;
        if !ok then (
          Unix.mkdir lib_name 0o755;
          Io.with_chdir lib_name (fun () ->
              Io.write (lib_name ^ ".ml") Floatarray_lib.impl;
              Io.write (lib_name ^ ".mli") Floatarray_lib.intf;
              Io.write "dune" (Printf.sprintf "(library (name %s))\n" lib_name);
              Printf.printf
                "Library %s created at the root of the repository. Update the dune files to link \
                 when necessary.\n"
                lib_name) ))

let command (path : string) (ignored : string list) (annot : int) (not_refactor : bool)
    (use_get_set : string option) =
  if annot = -1 then (
    let n = ref 0 in
    while annotate path "float array" ignored do
      incr n
    done;
    Printf.printf "Reached fixpoint in %d annotation passes.\n%!" !n )
  else
    for _ = 1 to annot do
      ignore (annotate path "float array" ignored)
    done;
  if not not_refactor then refactor path use_get_set ignored;
  create_floatarray_library path "floatarray"

(* TODO add option to change the name *)

open Cmdliner

let init_path =
  let doc = "Path to the directory to refactor." in
  Arg.(value & pos 0 string "" & info [] ~docv:"PATH" ~doc)

let ignored =
  let doc = "List of files to ignore." in
  Arg.(value & opt (list string) [] & info [ "i"; "ignore" ] ~docv:"FILE,...,FILE" ~doc)

let dont_refactor =
  let doc = "Do not refactor." in
  Arg.(value & flag & info [ "dont-refactor" ] ~doc)

let annotate_times =
  let doc = "Annotate <n> times (-1 by default to loop until a fixpoint is reached)." in
  Arg.(value & opt int (-1) & info [ "a"; "annotate" ] ~docv:"<N>" ~doc)

let get_set_op_descr =
  let doc =
    "Operator used for get and set functions, of the format \"!()\"(see \
     https://caml.inria.fr/pub/docs/manual-ocaml/indexops.html)"
  in
  Arg.(value & opt string "!()" & info [ "op-descr" ] ~docv:"OP_DESCR" ~doc)

let disable_ghost_get_set =
  let doc = "Disable indexing operator syntax for get and set functions." in
  Arg.(value & flag & info [ "disable-op-syntax" ] ~doc)

let use_get_set_t =
  Term.(
    const (fun op_descr disable -> if disable then None else Some op_descr)
    $ get_set_op_descr $ disable_ghost_get_set)

let refactor_t =
  Term.(const command $ init_path $ ignored $ annotate_times $ dont_refactor $ use_get_set_t)

let info =
  let doc = "Refactor float array to floatarray in the given dune directory." in
  Term.info "refactor" ~version:"0.1" ~doc ~exits:Term.default_exits

let () = Term.exit @@ Term.eval (refactor_t, info)
