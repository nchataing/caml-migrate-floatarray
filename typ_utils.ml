
open Typedtree

let rec print_path p =
  let open Path in
  match p with
  | Pident id -> Ident.name id
  | Pdot (p,s) ->
    let s' = print_path p in
    if s' = "Stdlib" then s else Printf.sprintf "%s.%s" s' s
  | _ -> ""

let print_typ t =
  let rec aux t = match t.Types.desc with
    | Tlink t -> aux t
    | Tarrow (label, t_arg, t_ret, _) ->
      let prec = 0 in
      let label = match label with
        | Nolabel -> ""
        | Labelled s -> Printf.sprintf "%s:" s
        | Optional s -> Printf.sprintf "?%s:" s
      in
      let prec_arg, s_arg = aux t_arg in
      let prec_ret, s_ret = aux t_ret in
      let s_arg = if prec >= prec_arg then "(" ^ s_arg ^ ")" else s_arg in
      let s_ret = if prec > prec_ret then "(" ^ s_ret ^ ")" else s_ret in
      prec, Printf.sprintf "%s%s -> %s" label s_arg s_ret
    | Tconstr (p,args,_) ->
      let prec = 2 in
      let args_str = match args with
        | [] -> ""
        | [x] -> (let child_prec, s = aux x in if prec > child_prec then "(" ^ s ^ ")" else s) ^ " "
        | _ -> "(" ^ (args |> List.map (fun t -> snd (aux t)) |> String.concat ", ") ^ ") "
      in
      prec, Printf.sprintf "%s%s" args_str (print_path p)
    | Ttuple ts ->
      let prec = 1 in
      prec, (match ts with
          | [] -> ""
          | [x] -> snd (aux x)
          | _ -> List.map (fun t -> let child_prec, s = aux t in
                            if prec >= child_prec then "(" ^ s ^ ")" else s
                          ) ts |> String.concat " * "
        )
    | Tvar Some _ | Tvar None -> 3,"_"
    | _ -> 0, "_"
  in
  snd (aux t)


let memo f =
  let tbl = Hashtbl.create 10 in
  (fun x ->
     match Hashtbl.find_opt tbl x with
     | Some res -> res
     | None ->
       let res = f x in
       Hashtbl.add tbl x res;
       res
  )

let parse_typ s =
  let ctype = Parse.core_type (Lexing.from_string s) in
  let env = Compmisc.initial_env () in
  try (Typetexp.transl_type_scheme env ctype).ctyp_type
  with Env.Error e -> Env.report_error Format.std_formatter e; failwith "Failed to parse type"

let parse_typ = memo parse_typ

let ctyp_path_of_string s =
  let ctype = Parse.core_type (Lexing.from_string s) in
  let env = Compmisc.initial_env () in
  try (
    match (Typetexp.transl_type_scheme env ctype).ctyp_desc with
    | Ttyp_constr (path, _, _) -> path
    | _ -> failwith "."
  ) with Env.Error e -> Env.report_error Format.std_formatter e; failwith "."

let typ_var_n = ref 0

(* rewrite the type with fresh type variables *)
let rec rewrite_typ_var t =
  let open Types in
  let desc = match t.desc with
    | Tvar _ -> let s = string_of_int !typ_var_n in incr typ_var_n; Tvar (Some ("_" ^ s))
    | Tarrow (l,t1,t2,c) -> Tarrow (l,rewrite_typ_var t1, rewrite_typ_var t2, c)
    | Ttuple ts -> Ttuple (List.map rewrite_typ_var ts)
    | Tconstr (p, ts, m) -> Tconstr (p, List.map rewrite_typ_var ts, m)
    | Tlink t -> (rewrite_typ_var t).desc
    | _ -> t.desc
  in
  { t with desc }

let uni_trace_contains_escape =
  List.exists
    (fun x -> match x with Ctype.Unification_trace.Escape _ -> true | _ -> false)

let moregeneral env t_ref t_to_test =
  try
    Ctype.moregeneral env false t_ref t_to_test ||
    (
      try
        let t_to_test = t_to_test |> Ctype.instance |> rewrite_typ_var in
        Ctype.unify env t_ref t_to_test;
        false
      with Ctype.Unify trace ->
        if uni_trace_contains_escape trace then
          let env =
            try Env.env_of_only_summary Envaux.env_from_summary env
            with Envaux.Error _ -> env
          in Ctype.moregeneral env false t_ref t_to_test
        else false
    )
  with Assert_failure _ -> false

let exp_type_match texp t_ref_str = moregeneral (texp.exp_env) (parse_typ t_ref_str) (texp.exp_type)

let moregeneral_expand_env env t_ref t_to_test =
  try
    let env =
      try Env.env_of_only_summary Envaux.env_from_summary env
      with Envaux.Error _ -> env
    in Ctype.moregeneral env false t_ref t_to_test
  with Assert_failure _ -> false

let rec split_arrow_typ t = match t.Types.desc with
  | Tlink t -> split_arrow_typ t
  | Tarrow (l, t_arg, t_ret, _) -> (l, t_arg) :: (split_arrow_typ t_ret)
  | _ -> [Nolabel, t] (* true return type *)
