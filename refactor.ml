
let () =
  Load_path.init ["../mlfi/mlfi-ins/lib"]

let () = Clflags.strict_deps := false

let read filename =
  let ic = open_in_bin filename in
  Fun.protect ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let write filename txt =
  let oc = open_out_bin filename in
  Fun.protect ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc txt)


type result = Ok | Nop | Err of string

module Log = struct

  let log : Buffer.t = Buffer.create 0

  let add_file_entries file entries =
    if List.length entries <> 0 then begin
      Buffer.add_string log (Printf.sprintf "In file %s\n" file);
      entries
      |> List.sort (fun x y -> compare (snd x) (snd y))
      |> List.iter
        (fun (msg,loc) ->
           Buffer.add_string log (Printf.sprintf "[%d] %s\n" loc msg)
        )
    end

  let write file = write file (Buffer.contents log)
end

module Patch = struct

  type patch = (int * int) * patch_kind * bool

  and patch_kind =
    | NoPatch
    | PatchList of patch list (* patches in this list should not overlap *)
    | Rewrite of string
    | Get of patch * patch
    | Set of patch * patch * patch
    | ArrayConstructor of patch list

  let make_named_patch loc txt = (loc,txt,false)

  let rec remove_dupl = function
    | [] | [_] as xx -> xx
    | (((xs,xe),_,_) as x) :: (((ys,ye),_,_) :: xx as yy) ->
      if xs <= ys && ye <= xe then
        (* we remove y *)
        remove_dupl (x :: xx)
      else if ys <= xs && xe <= ye then
        (* we remove x *)
        remove_dupl yy
      else
        x :: remove_dupl yy

  let par_start_end_match s =
    let n = String.length s in
    s.[0] = '(' && s.[n-1] = ')' &&
    (
      let par_count = ref 0 in
      let res = ref true in
      for i = 1 to String.length s - 2 do
        if s.[i] = '(' then incr par_count
        else if s.[i] = ')' then decr par_count;
        if !par_count = -1 then res := false
      done;
      !res
    )

  let rec patch_to_str txt ((s,e),kind,par) =
    let par = ref par in
    let patch_to_str = patch_to_str txt in
    let str = match kind with
      | NoPatch -> String.sub txt s (e - s)
      | PatchList ps ->
        let ps = List.sort (fun ((s1,_),_,_) ((s2,_),_,_) -> compare s1 s2) ps |> remove_dupl in
        let (off, str) = List.fold_left
            (fun (off,x) (((s,e),_,_) as p) ->
               let str = patch_to_str p in
               (* Printf.printf "current off : %d | modif : %d - %d -- %s\n%!" off s e str; *)
               (e,Printf.sprintf "%s%s%s" x (String.sub txt off (s - off)) str)
            ) (s,"") ps
        in str ^ (String.sub txt off (e - off))
      | Rewrite str -> str
      | Get (arr,i) ->
        Printf.sprintf "%s.!(%s)" (patch_to_str arr) (patch_to_str i)
      | Set (arr,i,v) ->
        Printf.sprintf "%s.!(%s) <- %s"
          (patch_to_str arr) (patch_to_str i) (patch_to_str v)
      | ArrayConstructor elts_p -> begin
          match elts_p with
          | [] -> par := false; "Floatarray.empty"
          | [x1] ->
            Printf.sprintf "Floatarray.make1 %s" (patch_to_str x1)
          | [x1;x2] ->
            Printf.sprintf "Floatarray.make2 %s %s"
              (patch_to_str x1) (patch_to_str x2)
          | [x1;x2;x3] ->
            Printf.sprintf "Floatarray.make3 %s %s %s"
              (patch_to_str x1) (patch_to_str x2) (patch_to_str x3)
          | [x1;x2;x3;x4] ->
            Printf.sprintf "Floatarray.make4 %s %s %s %s"
              (patch_to_str x1) (patch_to_str x2) (patch_to_str x3) (patch_to_str x4)
          | _ ->
            let elts_p = List.map (fun (pos,kind,_) -> (pos,kind,false)) elts_p in
            let str = patch_to_str ((s,e), PatchList elts_p, false) in
            Printf.sprintf "Floatarray.from_array %s" str
        end
    in
    if !par && not (par_start_end_match str) then "(" ^ str ^ ")" else str
end

let rec remove_dupl = function
  | [] | [_] as xx -> xx
  | (((xs,xe),_) as x) :: (((ys,ye),_) :: xx as yy) ->
    if xs <= ys && ye <= xe then
      (* we remove y *)
      remove_dupl (x :: xx)
    else if ys <= xs && xe <= ye then
      (* we remove x *)
      remove_dupl yy
    else
      x :: remove_dupl yy

let patch locs sources =
  let b = Buffer.create (String.length sources) in
  let locs = locs
             |> List.sort (fun ((start1, _), _) ((start2, _), _) -> compare start1 start2)
             |> remove_dupl
  in
  let rec loop off = function
    | ((start, end_), txt) :: rest ->
      (* Printf.printf "current off : %d | modif : %d - %d -- %s\n%!" off start end_ txt; *)
      Buffer.add_substring b sources off (start - off);
      Buffer.add_string b txt;
      loop end_ rest
    | [] ->
      Buffer.add_substring b sources off (String.length sources - off);
      Buffer.contents b
  in
  loop 0 locs

module TypUtils = struct
  (* Type related code *)
  open Typedtree

  let rec print_path p =
    let open Path in
    match p with
    | Pident id -> Ident.name id
    | Pdot (p,s) ->
      let s' = print_path p in
      if s' = "Stdlib" then s else Printf.sprintf "%s.%s" s' s
    | _ -> ""

  let typ_var_n = ref 0

  let typ_of_string s =
    let ctype = Parse.core_type (Lexing.from_string s) in
    let env = Compmisc.initial_env () in
    try (Typetexp.transl_type_scheme env ctype).ctyp_type
    with Env.Error e -> Env.report_error Format.std_formatter e; failwith "Failed to parse type"

  let ctyp_path_of_string s =
    let ctype = Parse.core_type (Lexing.from_string s) in
    let env = Compmisc.initial_env () in
    try (
      match (Typetexp.transl_type_scheme env ctype).ctyp_desc with
      | Ttyp_constr (path, _, _) -> path
      | _ -> failwith "."
    ) with Env.Error e -> Env.report_error Format.std_formatter e; failwith "."

  let float_typ : Types.type_expr = typ_of_string "float"

  let float_array_typ : Types.type_expr = typ_of_string "float array"

  let float_array_array_typ : Types.type_expr = typ_of_string "float array array"

  let alpha_array_typ : Types.type_expr = typ_of_string "'a array"

  let floatarray_typ : Types.type_expr = typ_of_string "floatarray"

  let float_path : Path.t = ctyp_path_of_string "float"

  let array_path : Path.t = ctyp_path_of_string "'a array"

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

  let moregeneral_expand_env env t_ref t_to_test =
    try
      let env =
        try Env.env_of_only_summary Envaux.env_from_summary env
        with Envaux.Error _ -> env
      in Ctype.moregeneral env false t_ref t_to_test
    with Assert_failure _ -> false

  let moregeneral_no_env_expansion env t_ref t_to_test =
    try Ctype.moregeneral env false t_ref t_to_test
    with Assert_failure _ -> false

  let typ_matches_float texp = moregeneral texp.exp_env float_typ texp.exp_type

  let typ_matches_float_array texp = moregeneral texp.exp_env float_array_typ texp.exp_type

  let typ_matches_float_array_array texp = moregeneral texp.exp_env float_array_array_typ texp.exp_type

  let rec get_res_typ t = match t.Types.desc with
    | Tarrow (_, _, t, _) | Tlink t -> get_res_typ t
    | _ -> t

  let rec split_arrow_typ t = match t.Types.desc with
    | Tlink t -> split_arrow_typ t
    | Tarrow (l, t_arg, t_ret, _) -> (l, t_arg) :: (split_arrow_typ t_ret)
    | _ -> [Nolabel, t] (* true return type *)

  let rec get_arg_and_ret t = match t.Types.desc with
    | Tlink t -> get_arg_and_ret t
    | Tarrow (_, t_arg, t_ret, _) -> (t_arg, t_ret)
    | _ -> invalid_arg ""

  (* Get the argument type with label [label], or the first argument type without
     label if NoLabel, and returns the function type without the retrieved type *)
  let rec get_arg_typ label t =
    let open Types in match t.desc with
    | Tlink t -> get_arg_typ label t
    | Tarrow (Nolabel,t_arg,t_ret,_) -> Some t_arg, t_ret
    | Tarrow (l,t_arg,t_ret,x) ->
      if l = label then
        Some t_arg, t_ret
      else
        let res, modified_typ = get_arg_typ label t_ret in
        res, {t with desc = Tarrow (l,t_arg,modified_typ,x)}
    | _ -> None, t

  let rec remove_links t = match t.Types.desc with
    | Tlink t -> remove_links t
    | _ -> t

  (* Check if 'a array is used with float array somewhere in a given type :
     t2 is supposed to be an instance of t1 *)
  let rec poly_array_replaced_by_float_array env t1 t2 =
    let check =
      moregeneral_no_env_expansion env alpha_array_typ t1 &&
      moregeneral_no_env_expansion env t1 alpha_array_typ &&
      moregeneral_no_env_expansion env float_array_typ t2
    in
    check ||
    (
      let t1,t2 = remove_links t1, remove_links t2 in
      match t1.desc, t2.desc with
      | Tarrow (_,t_arg1,t_ret1,_), Tarrow (_,t_arg2,t_ret2,_) ->
        poly_array_replaced_by_float_array env t_arg1 t_arg2 ||
        poly_array_replaced_by_float_array env t_ret1 t_ret2
      | Tconstr (p1,ts1,_), Tconstr (p2,ts2,_) when p1 = p2 && List.length ts1 = List.length ts2 ->
        List.combine ts1 ts2 |>
        List.exists (fun (t,t') -> poly_array_replaced_by_float_array env t t')
      | Ttuple ts1, Ttuple ts2 when List.length ts1 = List.length ts2 ->
        List.combine ts1 ts2 |>
        List.exists (fun (t,t') -> poly_array_replaced_by_float_array env t t')
      | _ -> false
    )


  let rec rewrite ~env ~grep ~replace t =
    if moregeneral env grep t then replace
    else
      let rewrite = rewrite ~env ~grep ~replace in
      let desc = match t.desc with
        | Tlink t -> (rewrite t).Types.desc
        | Tarrow (l, arg, ret, x) -> Tarrow (l, rewrite arg, rewrite ret, x)
        | Ttuple ts -> Ttuple (List.map rewrite ts)
        | Tconstr (p,ts,x) -> Tconstr (p, List.map rewrite ts, x)
        | _ -> t.desc
      in { t with desc }
end

module RefactorFloatArray = struct
  open Typedtree
  open Tast_iterator
  open Patch
  open TypUtils
  open Location

  let patch_list_to_kind l = if l = [] then NoPatch else PatchList l

  (* Working with positions *)
  let get_pos texp = (texp.exp_loc.loc_start.pos_cnum, texp.exp_loc.loc_end.pos_cnum)

  let pos_in (s1,e1) (s2,e2) = s2 <= s1 && e1 <= e2

  exception ReturnOk

  let pat_contains_float_array p =
    let pat iter p = match p.pat_desc with
      | Tpat_array _ when moregeneral p.pat_env float_array_typ p.pat_type ->
        raise ReturnOk
      | _ -> default_iterator.pat iter p
    in
    let iter = { default_iterator with pat } in
    try
      iter.pat iter p;
      false
    with ReturnOk -> true

  let fetch_attr_loc str =
    List.fold_left
      (fun acc attr -> if attr.Parsetree.attr_name.txt = str then Some attr.attr_loc else acc)
      None

  let fetch_attr str =
    List.fold_left
      (fun acc attr -> if attr.Parsetree.attr_name.txt = str then Some attr else acc)
      None

  let get_payload_string =
    let open Parsetree in function
      | PStr [{pstr_desc=Pstr_eval ({pexp_desc=Pexp_ident {txt;_};_},_);_}] -> Longident.last txt
      | PStr [] -> ""
      | _ -> assert false

  let value_binding patches _ iter vb =
    match fetch_attr_loc "ignore" vb.vb_attributes with
    | Some loc ->
      let loc_cnum = loc.loc_start.pos_cnum,loc.loc_end.pos_cnum in
      patches := (loc_cnum, Rewrite "", false) :: !patches
    | None -> default_iterator.value_binding iter vb

  let rec filter_split p = function
    | [] -> [], []
    | x :: xx ->
      let pp, not_pp = filter_split p xx in
      if p x then x :: pp, not_pp else pp, x :: not_pp

  let put_par_on_child child_exp = match child_exp.exp_desc with
    | Texp_constant (Asttypes.Const_float f) when f.[0] = '-' -> true (* negative number *)
    | Texp_ident _
    | Texp_array _
    | Texp_constant _ -> false
    | _ -> true

  let put_par : bool ref = ref false

  let handle_par_expr sub texp =
    let extra = function
      | Texp_constraint cty -> sub.typ sub cty
      | Texp_coerce (cty1, cty2) ->
        Option.iter (sub.typ sub) cty1;
        sub.typ sub cty2
      | Texp_newtype _ -> ()
      | Texp_poly cto -> Option.iter (sub.typ sub) cto
    in
    List.iter (fun (e, _, _) -> extra e) texp.exp_extra;
    sub.env sub texp.exp_env;
    match texp.exp_desc with
    | Texp_apply (
        {
          exp_desc = Texp_ident (
              Path.Pdot (_, ("+."|"-."|"*."|"/."|"**"|"="|"<>"|"<"|">"|"<="|">="|"=="|"!=")), _, _
            ); exp_loc=op_loc; _
        }, [(_, Some arg1); (_, Some arg2)]) ->
      let op_loc = op_loc.loc_start.pos_cnum in
      let par = !put_par in
      put_par :=
        not (arg1.exp_loc.loc_start.pos_cnum < op_loc && op_loc < arg2.exp_loc.loc_start.pos_cnum);
      sub.expr sub arg1;
      sub.expr sub arg2;
      put_par := par
    | Texp_apply (exp, list) ->
      let par = !put_par in
      put_par := false;
      sub.expr sub exp;
      put_par := true;
      List.iter (fun (_, o) -> Option.iter (sub.expr sub) o) list;
      put_par := par
    | Texp_construct (_, _, args) ->
      let par = !put_par in
      if List.length args = 1 then put_par := true;
      List.iter (sub.expr sub) args;
      put_par := par
    | _ -> default_iterator.expr sub texp

  let default_iterator = { default_iterator with expr = handle_par_expr }

  let rec expr patches file_log iter texp =
    begin match fetch_attr_loc "ignore" texp.exp_attributes with
      | Some loc ->
        let loc_cnum = loc.loc_start.pos_cnum,loc.loc_end.pos_cnum in
        patches := (loc_cnum, Rewrite "", false) :: !patches
      | None ->
        begin match fetch_attr "rewrite" texp.exp_attributes with
          | Some attr ->
            let loc_cnum = attr.Parsetree.attr_loc.loc_start.pos_cnum, attr.attr_loc.loc_end.pos_cnum in
            patches := (loc_cnum, Rewrite "", false) :: !patches;
            (
              match texp.exp_desc with
              | Texp_apply (id,_) ->
                let s = get_payload_string attr.attr_payload in
                let pos = get_pos id in
                let pos = if s = "" then (fst pos, snd pos + 1) else pos in
                patches := (pos, Rewrite s, false) :: !patches
              | _ -> ()
            );
            expr_no_attr patches file_log iter texp !put_par
          | None -> expr_no_attr patches file_log iter texp !put_par
        end
    end

  and expr_no_attr patches file_log iter texp par : unit =
    match texp.exp_desc with
    | Texp_function {cases={c_lhs; _}::_;_} ->
      List.fold_left
        (fun acc (x,_,_) -> match x with Tpat_constraint t -> fetch_attr_loc "tmp_annot" t.ctyp_attributes | _ -> acc)
        None c_lhs.pat_extra
      |> Option.iter (fun attr_loc ->
          let lpar = c_lhs.pat_loc.loc_start.pos_cnum - 1 in
          let rpar = attr_loc.loc_end.pos_cnum in
          let id_end = c_lhs.pat_loc.loc_end.pos_cnum in
          patches := ((lpar, lpar+1), Rewrite "", false) :: ((id_end, rpar+1), Rewrite "", false) :: !patches
        );
      default_iterator.expr iter texp
    | Texp_array l when moregeneral_expand_env texp.exp_env float_array_typ texp.exp_type ->
      let elt_patches = List.map (fun elt_t ->
          let elt_patch = ref [] in
          let iter = find_float_iterator elt_patch file_log in
          iter.expr iter elt_t;
          (get_pos elt_t, patch_list_to_kind !elt_patch, put_par_on_child elt_t)
        ) l in
      patches := (get_pos texp, ArrayConstructor elt_patches, par) :: !patches
    | Texp_apply
        ({exp_desc=Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "get"), _, _);
          exp_loc={loc_ghost=true;_};
          _},
         [(_, Some arr_exp); (_, Some i_exp)]) ->
      if typ_matches_float_array arr_exp then (
        let arr_pos = get_pos arr_exp in
        let i_pos = get_pos i_exp in
        let local_patch = ref [] in
        default_iterator.expr (find_float_iterator local_patch file_log) texp;
        let arr_patch, i_patch, other  = List.fold_left
            (fun (arr_patch, i_patch, other) ((pos,_,_) as p) ->
               if pos_in pos arr_pos then p::arr_patch, i_patch, other
               else if pos_in pos i_pos then arr_patch, p::i_patch, other
               else arr_patch, i_patch, p::other (* could be a type patch *)
            ) ([],[],[]) !local_patch
        in
        let arr_patch = (arr_pos, patch_list_to_kind arr_patch, false) in
        let i_patch = (i_pos, patch_list_to_kind i_patch, false) in
        patches := (get_pos texp, Get (arr_patch, i_patch), false) :: (other @ !patches)
      )
    | Texp_apply
        ({exp_desc=Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "get"), _, _);
          exp_loc={loc_ghost=false;_};
          _} as f_exp, (_, Some arr_exp) :: _) ->
      if typ_matches_float_array arr_exp then
        patches := (get_pos f_exp, Rewrite "Float.Array.get", false) :: !patches;
      default_iterator.expr iter texp
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "unsafe_get"), _, _);
          _} as f_exp,
         [(_, Some arr_exp); _]) ->
      if typ_matches_float_array arr_exp then
        patches := (get_pos f_exp, Rewrite "Float.Array.unsafe_get", false) :: !patches;
      default_iterator.expr iter texp
    | Texp_apply
        ({exp_desc=Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "set"), _, _);
          exp_loc={loc_ghost=true;_};
          _},
         [(_, Some arr_exp); (_, Some i_exp); (_, Some v_exp)])
      when typ_matches_float_array arr_exp ->
      let arr_pos = get_pos arr_exp in
      let i_pos = get_pos i_exp in
      let v_pos = get_pos v_exp in
      let local_patch = ref [] in
      default_iterator.expr (find_float_iterator local_patch file_log) texp;
      let arr_patch, i_patch, v_patch, other = List.fold_left
          (fun (arr_patch, i_patch, v_patch, other) ((pos,_,_) as p) ->
             if pos_in pos arr_pos then p::arr_patch, i_patch, v_patch, other
             else if pos_in pos i_pos then arr_patch, p::i_patch, v_patch, other
             else if pos_in pos v_pos then arr_patch, i_patch, p::v_patch, other
             else arr_patch, i_patch, v_patch, p::other (* could be a type patch *)
          ) ([],[],[],[]) !local_patch
      in
      let arr_patch = (arr_pos, patch_list_to_kind arr_patch, false) in
      let i_patch = (i_pos, patch_list_to_kind i_patch, false) in
      let v_patch = (v_pos, patch_list_to_kind v_patch, false) in
      patches := (get_pos texp, Set (arr_patch, i_patch, v_patch), false) :: (other @ !patches)
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "unsafe_set"), _, _);
          _} as f_exp,
         [(_, Some arr_exp); _; _]) ->
      if typ_matches_float_array arr_exp then
        patches := (get_pos f_exp, Rewrite "Float.Array.unsafe_set", false) :: !patches;
      default_iterator.expr iter texp
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "map"), _, _);
          _} as f_exp,
         (_, Some f) :: _) ->
      begin try
          let f_arg_t, f_ret_t = get_arg_and_ret f.exp_type in
          let is_float_arg = moregeneral f.exp_env float_typ f_arg_t in
          let is_float_ret = moregeneral f.exp_env float_typ f_ret_t in
          let fun_name =
            if is_float_arg && is_float_ret then "Float.Array.map"
            else if is_float_arg then "Float.Array.map_to_array"
            else if is_float_ret then "Float.Array.map_from_array"
            else ""
          in
          patches :=
            if fun_name <> "" then (get_pos f_exp, Rewrite fun_name, false) :: !patches
            else !patches
        with Invalid_argument _ -> ()
      end;
      default_iterator.expr iter texp
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "mapi"), _, _);
          _} as f_exp,
         [ _; (_, Some arr_exp)]) ->
      let arr_typ_match = typ_matches_float_array arr_exp in
      let exp_typ_match = typ_matches_float_array texp in
      let fun_name =
        if arr_typ_match && exp_typ_match then "Float.Array.mapi"
        else if arr_typ_match then "Floatarray.mapi_to_array"
        else if exp_typ_match then "Floatarray.mapi_from_array"
        else ""
      in
      patches :=
        if fun_name = "" then !patches
        else (get_pos f_exp, Rewrite fun_name, false) :: !patches;
      default_iterator.expr iter texp
    (*
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "map2"), _, _);
          _} as f_exp,
         [ _; (_, Some arr1_exp); (_, Some arr2_exp)]) ->
           TODO
    *)
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "make_matrix"), _, _);
          _},
         [ _; _ ; (_, Some v_exp)])
      when typ_matches_float v_exp ->
      patches := (get_pos texp, Rewrite "Floatarray.make_matrix", false) :: !patches;
      default_iterator.expr iter texp
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "iteri"), _, _);
          _} as f_exp,
         (_, Some f) :: _) ->
      begin try
          let _, t_aux = get_arg_and_ret f.exp_type in
          let arg, _ = get_arg_and_ret t_aux in
          if moregeneral f.exp_env float_typ arg then
            patches := (get_pos f_exp, Rewrite "Float.Array.iteri", false) :: !patches
        with Assert_failure _ -> () end;
      default_iterator.expr iter texp
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "iter2"), _, _);
          _} as f_exp,
         (_, Some f) :: _) ->
      begin try
          let arg1, t_aux = get_arg_and_ret f.exp_type in
          let arg2, _ = get_arg_and_ret t_aux in
          let is_float_arg1 = moregeneral f.exp_env float_typ arg1 in
          let is_float_arg2 = moregeneral f.exp_env float_typ arg2 in
          if is_float_arg1 && is_float_arg2 then
            patches := (get_pos f_exp, Rewrite "Float.Array.iter2", false) :: !patches
          else if is_float_arg1 || is_float_arg2 then
            file_log := ("iter2", f_exp.exp_loc.loc_start.pos_lnum) :: !file_log
          else
            ()
        with Assert_failure _ -> () end;
      default_iterator.expr iter texp
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "make"), _, _);
          _} as f_exp,
         [ _; (_, Some v_exp)]) ->
      patches :=
        if typ_matches_float v_exp then
          (get_pos f_exp, Rewrite "Float.Array.make", false) :: !patches
        else !patches;
      default_iterator.expr iter texp
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "fill"), _, _);
          _} as f_exp,
         (_,Some arr_exp) :: _) ->
      patches :=
        if typ_matches_float_array arr_exp then
          (get_pos f_exp, Rewrite "Float.Array.fill", false) :: !patches
        else !patches;
      default_iterator.expr iter texp
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "fold_left"), _, _);
          _} as f_exp,
         [_; _; (_, Some arr_exp)]) ->
      patches :=
        if typ_matches_float_array arr_exp then
          (get_pos f_exp, Rewrite "Float.Array.fold_left", false) :: !patches
        else !patches;
      default_iterator.expr iter texp
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "fold_right"), _, _);
          _} as f_exp,
         [_; (_, Some arr_exp); _]) ->
      patches :=
        if typ_matches_float_array arr_exp then
          (get_pos f_exp, Rewrite "Float.Array.fold_right", false) :: !patches
        else !patches;
      default_iterator.expr iter texp
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "mem"), _, _);
          _} as f_exp,
         [_; (_, Some arr_exp)]) ->
      patches :=
        if typ_matches_float_array arr_exp then
          (get_pos f_exp, Rewrite "Float.Array.mem", false) :: !patches
        else !patches;
      default_iterator.expr iter texp
    | Texp_ident (Path.Pdot (
        Path.Pdot (_, "Array"),
        ("of_list"|"to_list"|"create_float"|"copy"|"concat"|"init"|"length"|"get"|"set"|"unsafe_get"|"unsafe_set" as fun_name)) , _, _)
      when texp.exp_loc.loc_ghost = false ->
      let arg_ts = split_arrow_typ texp.exp_type |> List.map snd in
      if List.exists (moregeneral texp.exp_env float_array_typ) arg_ts &&
         List.for_all (fun t -> not (moregeneral texp.exp_env float_array_array_typ t)) arg_ts
      then
        let fun_name = if fun_name = "create_float" then "create" else fun_name in
        patches := (get_pos texp, Rewrite ("Float.Array." ^ fun_name), false) :: !patches
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (
                Path.Pdot
                  (_, "Array"),
                ("length"|"create"|"make_float"|"init"|"append"
                |"concat"|"sub"|"copy"|"blit"|"to_list"|"of_list"|"iter"|"iteri"
                |"for_all"|"exists"|"sort"|"stable_sort"|"fast_sort"|"to_seq"
                |"to_seqi"|"of_seq" as fun_name)
              ), _, _);
          _} as f_exp, args)
      when List.exists (fun (_,x) -> Option.fold ~none:false ~some:typ_matches_float_array x) args
        || typ_matches_float_array texp ->
      patches := (get_pos f_exp, Rewrite ("Float.Array." ^ fun_name), false) :: !patches;
      default_iterator.expr iter texp
    (* Case where we inspect polymorphic uses of 'a array as float array : only fills the log *)
    | Texp_apply
        ({exp_desc=Texp_ident (p, _, {val_type; _});
          exp_env=env; _}, args) ->
      let ret_typ = List.fold_left (fun f_typ (label,arg_exp) ->
          match arg_exp with
          | None -> f_typ
          | Some arg_exp ->
            let generic_arg_typ, f_typ = get_arg_typ label f_typ in
            let instance_arg_typ = arg_exp.exp_type in
            Option.iter (fun generic_arg_typ ->
                if poly_array_replaced_by_float_array env generic_arg_typ instance_arg_typ then
                  let pos = arg_exp.exp_loc.loc_start.pos_lnum in
                  file_log := (print_path p, pos) :: !file_log
              ) generic_arg_typ;
            f_typ
        ) val_type args
      in
      let _ = List.fold_left (fun f_typ (label,arg_typ) ->
          let gen_arg_typ, f_typ = get_arg_typ label f_typ in
          let gen_arg_typ = Option.value ~default:f_typ gen_arg_typ in
          if poly_array_replaced_by_float_array env gen_arg_typ arg_typ then
            let pos = texp.exp_loc.loc_start.pos_lnum in
            file_log := (print_path p, pos) :: !file_log
          else ();
          f_typ
        ) ret_typ (split_arrow_typ texp.exp_type)
      in
      default_iterator.expr iter texp
    | _ -> default_iterator.expr iter texp

  and typ patches _file_log iter ctyp =
    match fetch_attr_loc "tmp_annot_ret" ctyp.ctyp_attributes with
    | Some attr_loc ->
      let pos_end = attr_loc.loc_end.pos_cnum + 2 in
      let pos_start = ctyp.ctyp_loc.loc_start.pos_cnum - 4 in
      patches := ((pos_start, pos_end), Rewrite "", false) :: !patches
    | None ->
      begin match fetch_attr_loc "ignore" ctyp.ctyp_attributes with
        | Some loc ->
          let loc_cnum = loc.loc_start.pos_cnum,loc.loc_end.pos_cnum in
          patches := (loc_cnum, Rewrite "", false) :: !patches
        | None ->
          begin match ctyp.ctyp_desc with
            | Ttyp_constr (array_p, _,
                           [ {ctyp_desc=Ttyp_constr (float_p, _, []); _} ])
              when array_p = array_path && float_p = float_path ->
              let pos = ctyp.ctyp_loc.loc_start.pos_cnum, ctyp.ctyp_loc.loc_end.pos_cnum in
              if pos <> (-1,-1) then
                patches := (pos, Rewrite "floatarray", false) :: !patches
            | _ -> default_iterator.typ iter ctyp
          end
      end

  and find_float_iterator patches log =
    { default_iterator with value_binding = value_binding patches log; expr = expr patches log; typ = typ patches log }

  let rec get_structure_of_mod_expr me = match me.mod_desc with
    | Tmod_structure s -> s
    | Tmod_functor (_,me) -> get_structure_of_mod_expr me
    | Tmod_constraint (me,_,_,_) -> get_structure_of_mod_expr me
    | _ -> assert false

  let rec gen_open_patch str =
    if List.length str.str_items = 1 && (List.hd str.str_items).str_loc.loc_start.pos_cnum = -1 then (
      match (List.hd str.str_items).str_desc with
      | Tstr_include {incl_mod; _} -> get_structure_of_mod_expr incl_mod |> gen_open_patch
      | _ -> ((-1,-1), Rewrite "", false)
    )
    else
      let last_open =
        List.fold_left (fun (last_open,ok) str_item ->
            match str_item.str_desc,ok with
            | Tstr_open _, true -> (Some str_item), true
            | _ -> last_open, false
          ) (None,true) str.str_items
        |> fst in
      match last_open with
      | Some {str_loc; _} ->
        let pos = str_loc.loc_end.pos_cnum in
        ((pos,pos), Rewrite "\nopen Floatarray", false)
      | None ->
        let str_head = str.str_items |> List.filter (fun s -> s.str_loc.loc_start.pos_cnum <> -1) |> List.hd in
        let pos = str_head.str_loc.loc_start.pos_cnum in
        ((pos,pos), Rewrite "open Floatarray\n\n", false)

  let main (cmt_file : string) : result =
    let cmt = Cmt_format.read_cmt cmt_file in
    let patches = ref [] in
    let file_log = ref [] in
    let iter = find_float_iterator patches file_log in

    begin match cmt.cmt_annots with
      | Implementation str -> begin
          iter.structure iter str;
          if List.length !patches > 0 then
            patches := (gen_open_patch str) :: !patches
        end
      | Interface sg -> iter.signature iter sg
      | _ -> Printf.printf "Not an implementation nor an interface : %s\n%!" cmt_file
    end;
    let patches = !patches in
    if patches = [] then Nop else begin
      match cmt.cmt_sourcefile with
      | Some f ->
        let filename = Filename.concat (Filename.dirname cmt_file) (Filename.basename f) in
        let txt = read filename in
        let patches = List.map (fun ((pos,_,_) as patch) -> (pos, patch_to_str txt patch)) patches in
        patch patches txt |> write filename;
        Log.add_file_entries filename !file_log;
        Ok
      | None -> Err "No source file linked"
    end
end

let main path =
  if Filename.check_suffix path "cmt" || Filename.check_suffix path "cmti" then
    try
      let report = match RefactorFloatArray.main path with
        | Ok -> Some "[Ok]"
        | Err s -> Some (Printf.sprintf "[Err] %s" s)
        | Nop -> None
      in
      match report with Some s -> Printf.printf "%s %s\n%!" path s | None -> ()
    with
    | Persistent_env.Error e -> Persistent_env.report_error Format.std_formatter e; Printf.printf "%!"
    | e -> Printf.printf "[Exception] %s : %s" path (Printexc.to_string e)
  else ()

let rec explore ~f ?(process=true) path ignored_paths forced_paths =
  let process =
    if List.mem path forced_paths then (
      Printf.printf "forcing %s\n%!" path;
      true
    ) else if List.mem path ignored_paths then (
      Printf.printf "ignoring %s\n%!" path;
      false
    )
    else process
  in
  if Sys.is_directory path then
    Array.iter (fun file -> explore ~f ~process (Filename.concat path file) ignored_paths forced_paths) (Sys.readdir path)
  else if process then f path else ()

let () =
  let init_path = ref "" in
  let arg_fun = ref (fun _ -> ()) in
  let ignored_paths = ref [] in
  let forced_paths = ref [] in
  let log_output = ref "" in
  let open Arg in
  parse
    [
      "-path", Set_string init_path, "initial <path>";
      "-log", Set_string log_output, "<file> output log to file";
      "-ignore", Unit (fun () -> arg_fun := (fun s -> ignored_paths := s :: !ignored_paths)), "<path> to ignore";
      "-force", Unit (fun () -> arg_fun := (fun s -> forced_paths := s :: !forced_paths)), "<path> to force"
    ]
    (fun s -> !arg_fun s) "usage : ...";
  explore ~f:main !init_path !ignored_paths !forced_paths;
  if !log_output <> "" then (Log.write !log_output; Printf.printf "Log written in file %s\n%!" !log_output) else ()

include IGNORE(Asttypes)
