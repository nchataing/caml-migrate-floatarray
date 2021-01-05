type descr_item = {
  name: string;
  impl: string;
  intf: string;
  cmt: string;
  cmti: string;
}

type token = RPAR | LPAR | STR of string

let lex txt =
  let tokens = ref [] in
  let rec loop off =
    if off >= String.length txt then () else
      match txt.[off] with
      | '(' -> tokens := LPAR :: !tokens; loop (off + 1)
      | ')' -> tokens := RPAR :: !tokens; loop (off + 1)
      | '\n' | '\t' | ' ' | '\r' -> loop (off + 1)
      | _ -> let off_end = ref (off + 1) in
        while not (List.mem txt.[!off_end] ['('; ')'; '\n'; '\t'; ' '; '\r']) do incr off_end done;
        tokens := STR (String.sub txt off (!off_end - off)) :: !tokens;
        loop (!off_end)
  in
  loop 0;
  List.rev !tokens

let print_tokens = List.iter (fun tok ->
    let str = match tok with RPAR -> "RPAR" | LPAR -> "LPAR" | STR s -> Printf.sprintf "STR %s" s in
    Printf.printf "%s\n" str)

type sexp = Atom of string | List of sexp list

let rec parse_ acc toks = match toks with
  | [] -> List (List.rev acc), []
  | LPAR :: t ->
    let exp, t' = parse_ [] t in
    (match t' with [] -> exp, [] | _ -> parse_ (exp :: acc) t')
  | RPAR :: t -> List (List.rev acc), t
  | STR s :: t -> parse_ (Atom s :: acc) t

let parse t = parse_ [] t |> fst

let get_opt_string = function
  | List [] -> ""
  | List [Atom s] -> s
  | _ -> assert false

let parse_module_descrs = function
  | List [List [Atom "executables"; List [ _ ; _ ; List [Atom "modules"; List descrs]; _ ]]] ->
    List.map (fun exp -> match exp with
        | List [
            List [Atom "name"; Atom name];
            List [Atom "impl"; impl];
            List [Atom "intf"; intf];
            List [Atom "cmt"; cmt];
            List [Atom "cmti"; cmti]
            ] ->
          let impl = get_opt_string impl in
          let intf = get_opt_string intf in
          let cmt = get_opt_string cmt in
          let cmti = get_opt_string cmti in
          { name; impl; intf; cmt; cmti }
        | _ -> assert false) descrs
  | _ -> assert false

let print_descr_item { name; impl; intf; cmt; cmti } =
  Printf.printf "name : %s\nimpl : %s\nintf : %s\ncmt : %s\ncmti : %s\n" name impl intf cmt cmti

let rec sexp_to_string = function
  | Atom s -> Printf.sprintf "{A:%s}" s
  | List l -> List.map sexp_to_string l |> String.concat ";" |> Printf.sprintf "L[%s]"

let sexp_of_string s = parse @@ lex s

let read filename =
  let ic = open_in_bin filename in
  Fun.protect ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let iter_module_descrs ~f () =
  let _ = Sys.command "dune describe > tmp" in
  let descrs = read "tmp" |> lex |> parse |> parse_module_descrs in
  let _ = Sys.command "rm tmp" in
  List.iter (fun {impl; cmt; intf; cmti; _} ->
    if impl <> "" then f (Filename.chop "_build/default/" impl) cmt;
    if intf <> "" then f (Filename.chop "_build/default/" intf) cmti;
  ) descrs
  
