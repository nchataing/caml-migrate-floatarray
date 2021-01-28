(******************************************************************)
(* Copyright (C) 2020-2021 Nicolas Chataing. All rights reserved. *)
(*                                                                *)
(* This software may be modified and distributed under the terms  *)
(* of the BSD license.  See the LICENSE file for details.         *)
(******************************************************************)

type descr_item = { name : string; impl : string; intf : string; cmt : string; cmti : string }

type container = { items : descr_item list; include_dirs : string list }

type token = RPAR | LPAR | STR of string

let lex txt =
  let tokens = ref [] in
  let rec loop off =
    if off >= String.length txt then ()
    else
      match txt.[off] with
      | '(' ->
          tokens := LPAR :: !tokens;
          loop (off + 1)
      | ')' ->
          tokens := RPAR :: !tokens;
          loop (off + 1)
      | '\n' | '\t' | ' ' | '\r' -> loop (off + 1)
      | _ ->
          let off_end = ref (off + 1) in
          while not (List.mem txt.[!off_end] [ '('; ')'; '\n'; '\t'; ' '; '\r' ]) do
            incr off_end
          done;
          tokens := STR (String.sub txt off (!off_end - off)) :: !tokens;
          loop !off_end
  in
  loop 0;
  List.rev !tokens

type sexp = Atom of string | List of sexp list

let rec parse_ acc toks =
  match toks with
  | [] -> (List (List.rev acc), [])
  | LPAR :: t -> (
      let exp, t' = parse_ [] t in
      match t' with [] -> (exp, []) | _ -> parse_ (exp :: acc) t' )
  | RPAR :: t -> (List (List.rev acc), t)
  | STR s :: t -> parse_ (Atom s :: acc) t

let parse t = parse_ [] t |> fst

let get_opt_string = function List [] -> "" | List [ Atom s ] -> s | _ -> assert false

let parse_module_descrs sexp =
  let process_descr exp =
    match exp with
    | List
        [
          List [ Atom "name"; Atom name ];
          List [ Atom "impl"; impl ];
          List [ Atom "intf"; intf ];
          List [ Atom "cmt"; cmt ];
          List [ Atom "cmti"; cmti ];
        ] ->
        let impl = get_opt_string impl in
        let intf = get_opt_string intf in
        let cmt = get_opt_string cmt in
        let cmti = get_opt_string cmti in
        { name; impl; intf; cmt; cmti }
    | _ -> assert false
  in
  List.map
    (function
      | List
          [
            Atom "executables";
            List
              [
                _;
                _;
                List [ Atom "modules"; List descrs ];
                List [ Atom "include_dirs"; List include_dirs ];
              ];
          ]
      | List
          [
            Atom "library";
            List
              [
                _;
                _;
                _;
                _;
                _;
                List [ Atom "modules"; List descrs ];
                List [ Atom "include_dirs"; List include_dirs ];
              ];
          ] ->
          let items = List.map process_descr descrs in
          let include_dirs = List.map (function Atom s -> s | _ -> assert false) include_dirs in
          { items; include_dirs }
      | _ -> assert false)
    (match sexp with List l -> l | _ -> assert false)

let iter_module_descrs ~path ~f ~ignored =
  (* Get module descriptions *)
  let tmp = Filename.temp_file "_" "_" in
  let _ =
    Io.with_chdir path (fun () ->
        Sys.command (Filename.quote_command ~stdout:tmp "dune" [ "describe" ]))
  in
  let descrs = Io.read tmp |> lex |> parse |> parse_module_descrs in
  (* helper functions to remove the _build/default prefix *)
  let add_path = Filename.concat path in
  let rm_prefix s =
    let n = String.length "_build/default/" in
    String.sub s n (String.length s - n)
  in
  List.iter
    (fun { items; include_dirs } ->
      List.iter Load_path.add_dir include_dirs;
      let process src_file annot_file =
        if src_file <> "" then
        let src_file = rm_prefix src_file in
            if List.mem src_file ignored then
              Printf.printf "Ignoring %s.\n%!" src_file
            else (
              let src_file = add_path src_file in
              if not @@ Sys.file_exists src_file then
              Printf.printf "File %s is generated... ignoring it.\n" src_file
            else
              f src_file (add_path annot_file)
            )
      in
      List.iter
        (fun { impl; cmt; intf; cmti; _ } -> (process impl cmt; process intf cmti))
        items)
    descrs
