type descr_item = { name : string; impl : string; intf : string; cmt : string; cmti : string }

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

let parse_module_descrs = function
  | List [ List [ Atom "executables"; List [ _; _; List [ Atom "modules"; List descrs ]; _ ] ] ] ->
      List.map
        (fun exp ->
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
          | _ -> assert false)
        descrs
  | _ -> assert false

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
    (fun { impl; cmt; intf; cmti; _ } ->
      ( if impl <> "" then
        let impl = rm_prefix impl in
        if not (List.mem impl ignored) then f (add_path impl) (add_path cmt) );
      if intf <> "" then
        let intf = rm_prefix intf in
        if not (List.mem intf ignored) then f (add_path intf) (add_path cmti))
    descrs
