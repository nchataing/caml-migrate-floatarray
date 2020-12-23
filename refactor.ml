
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

  let add_to (l : 'a list ref) (elt : 'a) = l := elt :: !l

  (* Working with positions *)
  let get_loc texp = (texp.exp_loc.loc_start.pos_cnum, texp.exp_loc.loc_end.pos_cnum)

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
      let loc = loc.loc_start.pos_cnum,loc.loc_end.pos_cnum in
      patches := (mk_remove_patch ~loc) :: !patches
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
        let loc = loc.loc_start.pos_cnum,loc.loc_end.pos_cnum in
        mk_remove_patch ~loc |> add_to patches
      | None ->
        begin match fetch_attr "rewrite" texp.exp_attributes with
          | Some attr ->
            let loc = attr.Parsetree.attr_loc.loc_start.pos_cnum, attr.attr_loc.loc_end.pos_cnum in
            patches := (mk_remove_patch ~loc) :: !patches;
            (
              match texp.exp_desc with
              | Texp_apply (id,_) ->
                let s = get_payload_string attr.attr_payload in
                let loc = get_loc id in
                let loc = if s = "" then (fst loc, snd loc + 1) else loc in
                mk_rewrite_patch ~loc s |> add_to patches
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
          mk_remove_patch ~loc:(lpar, lpar+1) |> add_to patches;
          mk_remove_patch ~loc:(id_end, rpar+1) |> add_to patches
        );
      default_iterator.expr iter texp
    | Texp_array l when moregeneral_expand_env texp.exp_env float_array_typ texp.exp_type ->
      let elt_patches = List.map (fun elt_t ->
          let elt_patch = ref [] in
          let iter = find_float_iterator elt_patch file_log in
          iter.expr iter elt_t;
          mk_seq_patch ~loc:(get_loc elt_t) ~par:(put_par_on_child elt_t) !elt_patch
        ) l in
      mk_array_constr_patch ~loc:(get_loc texp) ~par elt_patches |> add_to patches
    (* TODO : adapt code
       | Texp_apply
        ({exp_desc=Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "get"), _, _);
          exp_loc={loc_ghost=true;_};
          _},
         [(_, Some arr_exp); (_, Some i_exp)]) ->
    *)
    | Texp_apply
        ({exp_desc=Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "get"), _, _);
          exp_loc={loc_ghost=false;_};
          _} as f_exp, (_, Some arr_exp) :: _) ->
      if typ_matches_float_array arr_exp then
        mk_rewrite_patch ~loc:(get_loc f_exp) "Float.Array.get" |> add_to patches;
      default_iterator.expr iter texp
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "unsafe_get"), _, _);
          _} as f_exp,
         [(_, Some arr_exp); _]) ->
      if typ_matches_float_array arr_exp then
        mk_rewrite_patch ~loc:(get_loc f_exp) "Float.Array.unsafe_get" |> add_to patches;
      default_iterator.expr iter texp
    (* TODO : adapt code
       | Texp_apply
        ({exp_desc=Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "set"), _, _);
          exp_loc={loc_ghost=true;_};
          _},
         [(_, Some arr_exp); (_, Some i_exp); (_, Some v_exp)])
       when typ_matches_float_array arr_exp ->
    *)
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "unsafe_set"), _, _);
          _} as f_exp,
         [(_, Some arr_exp); _; _]) ->
      if typ_matches_float_array arr_exp then
        mk_rewrite_patch ~loc:(get_loc f_exp) "Float.Array.unsafe_set" |> add_to patches;
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
          if fun_name <> "" then
            mk_rewrite_patch ~loc:(get_loc f_exp) fun_name |> add_to patches
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
      if fun_name <> "" then
        mk_rewrite_patch ~loc:(get_loc f_exp) fun_name |> add_to patches;
      default_iterator.expr iter texp
    (* TODO : adapt_code
       | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "map2"), _, _);
          _} as f_exp,
         [ _; (_, Some arr1_exp); (_, Some arr2_exp)]) ->
    *)
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "make_matrix"), _, _);
          _},
         [ _; _ ; (_, Some v_exp)])
      when typ_matches_float v_exp ->
      mk_rewrite_patch ~loc:(get_loc texp) "Floatarray.make_matrix" |> add_to patches;
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
            mk_rewrite_patch ~loc:(get_loc f_exp) "Float.Array.iteri" |> add_to patches
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
            mk_rewrite_patch ~loc:(get_loc f_exp) "Float.Array.iter2" |> add_to patches
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
      if typ_matches_float v_exp then
        mk_rewrite_patch ~loc:(get_loc f_exp) "Float.Array.make" |> add_to patches;
      default_iterator.expr iter texp
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "fill"), _, _);
          _} as f_exp,
         (_,Some arr_exp) :: _) ->
      if typ_matches_float_array arr_exp then
        mk_rewrite_patch ~loc:(get_loc f_exp) "Float.Array.fill" |> add_to patches;
      default_iterator.expr iter texp
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "fold_left"), _, _);
          _} as f_exp,
         [_; _; (_, Some arr_exp)]) ->
      if typ_matches_float_array arr_exp then
        mk_rewrite_patch ~loc:(get_loc f_exp) "Float.Array.fold_left" |> add_to patches;
      default_iterator.expr iter texp
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "fold_right"), _, _);
          _} as f_exp,
         [_; (_, Some arr_exp); _]) ->
      if typ_matches_float_array arr_exp then
        mk_rewrite_patch ~loc:(get_loc f_exp) "Float.Array.fold_right" |> add_to patches;
      default_iterator.expr iter texp
    | Texp_apply
        ({exp_desc=
            Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "mem"), _, _);
          _} as f_exp,
         [_; (_, Some arr_exp)]) ->
      if typ_matches_float_array arr_exp then
        mk_rewrite_patch ~loc:(get_loc f_exp) "Float.Array.mem" |> add_to patches;
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
        mk_rewrite_patch ~loc:(get_loc texp) ("Float.Array." ^ fun_name) |> add_to patches
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
      mk_rewrite_patch ~loc:(get_loc f_exp) ("Float.Array." ^ fun_name) |> add_to patches;
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
      let loc = (attr_loc.loc_end.pos_cnum + 2, ctyp.ctyp_loc.loc_start.pos_cnum - 4) in
      mk_remove_patch ~loc |> add_to patches
    | None ->
      begin match fetch_attr_loc "ignore" ctyp.ctyp_attributes with
        | Some loc ->
          let loc = loc.loc_start.pos_cnum,loc.loc_end.pos_cnum in
          mk_remove_patch ~loc |> add_to patches
        | None ->
          begin match ctyp.ctyp_desc with
            | Ttyp_constr (array_p, _,
                           [ {ctyp_desc=Ttyp_constr (float_p, _, []); _} ])
              when array_p = array_path && float_p = float_path ->
              let loc = ctyp.ctyp_loc.loc_start.pos_cnum, ctyp.ctyp_loc.loc_end.pos_cnum in
              if loc <> (-1,-1) then
                mk_rewrite_patch ~loc "floatarray" |> add_to patches
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
      | _ -> assert false
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
        mk_rewrite_patch ~loc:(pos,pos) "\nopen Floatarray"
      | None ->
        let str_head = str.str_items |> List.filter (fun s -> s.str_loc.loc_start.pos_cnum <> -1) |> List.hd in
        let pos = str_head.str_loc.loc_start.pos_cnum in
        mk_rewrite_patch ~loc:(pos,pos) "open Floatarray\n\n"

  let main (cmt_file : string) : result =
    let cmt = Cmt_format.read_cmt cmt_file in
    let patches = ref [] in
    let file_log = ref [] in
    let iter = find_float_iterator patches file_log in

    begin match cmt.cmt_annots with
      | Implementation str -> begin
          iter.structure iter str;
          if List.length !patches > 0 then
            gen_open_patch str |> add_to patches
        end
      | Interface sg -> iter.signature iter sg
      | _ -> Printf.printf "Not an implementation nor an interface : %s\n%!" cmt_file
    end;
    let patches = !patches in
    if patches = [] then Nop else begin
      match cmt.cmt_sourcefile with
      | Some f ->
        let filename = Filename.concat (Filename.dirname cmt_file) (Filename.basename f) in
        read filename |> apply_patch patches |> write filename;
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
