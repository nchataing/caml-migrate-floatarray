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

let refactor (path : string) (ignored : string list) =
  build path;
  Describe.iter_module_descrs ~path ~f:Refactor.refactor ~ignored

let annotate (path : string) (typ_str : string) (ignored : string list) : bool =
  build path;
  let typ_match = Typ_utils.parse_typ typ_str in
  let has_changed = ref false in
  Describe.iter_module_descrs ~path ~f:(Annotate.annotate ~typ_match ~has_changed) ~ignored;
  !has_changed

let command (path : string) (ignored : string list) (annot : int) (not_refactor : bool) =
  if annot = -1 then (
    let n = ref 0 in
    while annotate path "float array" ignored do incr n done;
    Printf.printf "Reached fixpoint in %d annotation passes.\n%!" !n;
  ) else (
  for _ = 1 to annot do
    ignore (annotate path "float array" ignored)
  done
  );
  if not not_refactor then refactor path ignored

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

type annotate_times = Fixpoint | FixedNb of int

let annotate_times =
  let doc = "Annotate <n> times (-1 by default to loop until a fixpoint is reached)." in
  Arg.(value & opt int (-1) & info [ "a"; "annotate" ] ~docv:"<N>" ~doc)

let refactor_t = Term.(const command $ init_path $ ignored $ annotate_times $ dont_refactor)

let info =
  let doc = "Refactor float array to floatarray in the given dune directory." in
  Term.info "refactor" ~version:"0.1" ~doc ~exits:Term.default_exits

let () = Term.exit @@ Term.eval (refactor_t, info)
