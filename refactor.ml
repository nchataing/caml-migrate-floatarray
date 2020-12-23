
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

module RefactorFloatArray = struct
  open Typedtree
  open Tast_iterator
  open Patch
  open Typ_utils
  open Location

  let add_to (l : 'a list ref) (elt : 'a) = l := elt :: !l

  let get_loc texp = (texp.exp_loc.loc_start.pos_cnum, texp.exp_loc.loc_end.pos_cnum)

  let pos_in (s1,e1) (s2,e2) = s2 <= s1 && e1 <= e2

  exception ReturnOk

  let pat_contains_float_array p =
    let pat iter p = match p.pat_desc with
      | Tpat_array _ when moregeneral p.pat_env (parse_typ "float array") p.pat_type ->
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

  type action = Patch of string | Log

  let fun_tbl = Hashtbl.create 30
  let () = List.iter (fun (key,value) -> Hashtbl.add fun_tbl key value)
      [
        ("Array.length", [("float array -> _", Patch "Float.Array.length")]);
        ("Array.get", [("float array -> _", Patch "Float.Array.get")]); 
        ("Array.set", [("float array -> _", Patch "Float.Array.set")]);
        ("Array.make", [("_ -> float -> _", Patch "Float.Array.make")]);
        ("Array.create", [("_ -> float -> _", Patch "Float.Array.create")]);
        ("Array.create_float", [("_", Patch "Float.Array.create")]);
        ("Array.make_float", [("_", Patch "Float.Array.create")]);
        ("Array.init", [("_ -> (_ -> float) -> _", Patch "Float.Array.init")]);
        ("Array.make_matrix", [("_ -> _ -> float -> _", Patch "Floatarray.make_matrix")]);
        ("Array.create_matrix", [("_ -> _ -> float -> _", Patch "Floatarray.make_matrix")]);
        ("Array.append", [("float array -> _", Patch "Float.Array.append")]);
        ("Array.concat", [("float array list -> _", Patch "Float.Array.concat")]);
        ("Array.sub", [("float array -> _", Patch "Float.Array.sub")]);
        ("Array.copy", [("float array -> _", Patch "Float.Array.copy")]);
        ("Array.fill", [("float array -> _", Patch "Float.Array.fill")]);
        ("Array.blit", [("float array -> _", Patch "Float.Array.blit")]);
        ("Array.to_list", [("float array -> _", Patch "Float.Array.to_list")]);
        ("Array.of_list", [("float list -> _", Patch "Float.Array.of_list")]);
        ("Array.iter", [("(float -> _) -> _", Patch "Float.Array.iter")]);
        ("Array.iteri", [("(_ -> float -> _) -> _", Patch "Float.Array.iteri")]);
        ("Array.map", [("(float -> float) -> _", Patch "Float.Array.map");
                       ("(float -> _) -> _", Patch "Float.Array.map_to_array");
                       ("(_ -> float) -> _", Patch "Float.Array.map_from_array")]);
        ("Array.mapi", [("(int -> float -> float) -> _", Patch "Float.Array.mapi");
                        ("(int -> float -> _) -> _", Patch "Floatarray.mapi_to_array");
                        ("(int -> _ -> float) -> _", Patch "Floatarray.mapi_from_array")]);
        ("Array.fold_left", [("(_ -> float -> _) -> _", Patch "Float.Array.fold_left")]);
        ("Array.fold_right", [("(float -> _ -> _) -> _", Patch "Float.Array.fold_right")]);
        ("Array.iter2", [("(float -> float -> _) -> _", Patch "Float.Array.iter2");
                         ("(float -> _ -> _) -> _", Log);
                         ("(_ -> float -> _) -> _", Log)]);
        ("Array.map2", [("(float -> float -> float) -> _", Patch "Float.Array.map2");
                        ("(float -> _ -> _) -> _", Log);
                        ("(_ -> float -> _) -> _", Log);
                        ("(_ -> _ -> float) -> _", Log)]);
        ("Array.for_all", [("(float -> _) -> _", Patch "Float.Array.for_all")]);
        ("Array.exists", [("(float -> _) -> _", Patch "Float.Array.exists")]);
        ("Array.mem", [("float -> _", Patch "Float.Array.mem")]);
        ("Array.memq", [("float -> _", Patch "Float.Array.memieee")]); (* ??? *)
        ("Array.sort", [("(float -> float -> _) -> _", Patch "Float.Array.sort")]);
        ("Array.stable_sort", [("(float -> float -> _) -> _", Patch "Float.Array.stable_sort")]);
        ("Array.to_seq", [("float array -> _", Patch "Float.Array.to_seq")]);
        ("Array.to_seqi", [("float array -> _", Patch "Float.Array.to_seqi")]);
        ("Array.of_seq", [("_ -> float array", Patch "Float.Array.of_seq")]);
        ("Array.unsafe_get", [("float array -> _", Patch "Float.Array.unsafe_get")]);
        ("Array.unsafe_set", [("float array -> _", Patch "Float.Array.unsafe_set")]);
      ]

  exception Stop

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
    | Texp_array l when moregeneral_expand_env texp.exp_env (parse_typ "float array") texp.exp_type ->
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
    (* TODO : adapt code
       | Texp_apply
        ({exp_desc=Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "set"), _, _);
          exp_loc={loc_ghost=true;_};
          _},
         [(_, Some arr_exp); (_, Some i_exp); (_, Some v_exp)])
       when typ_matches_float_array arr_exp ->
    *)
    | Texp_ident (path, _, _) ->
      Hashtbl.find_opt fun_tbl (print_path path)
      |> Option.iter (fun l ->
          try List.iter (fun (t_str, action) ->
              if exp_type_match texp t_str then (
                (match action with 
                 | Patch s -> mk_rewrite_patch ~loc:(get_loc texp) s |> add_to patches
                 | Log -> file_log := (print_path path, texp.exp_loc.loc_start.pos_lnum) :: !file_log);
                raise Stop
              )
            ) l
          with Stop -> ()
        )
    (* Case where we inspect polymorphic uses of 'a array as float array : only fills the log *)
  (*
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
                                     *)
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
              when array_p = ctyp_path_of_string "'a array" && float_p = ctyp_path_of_string "float" ->
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
