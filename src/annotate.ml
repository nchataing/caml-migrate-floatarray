(******************************************************************)
(* Copyright (C) 2020-2021 Nicolas Chataing. All rights reserved. *)
(*                                                                *)
(* This software may be modified and distributed under the terms  *)
(* of the BSD license.  See the LICENSE file for details.         *)
(******************************************************************)

open Typedtree
open Types
open Asttypes
open Tast_iterator
open Location
open Patch
open Typ_utils

let rec type_contains env t_out t_in =
  moregeneral env t_in t_out
  ||
  match t_out.Types.desc with
  | Tlink t -> type_contains env t t_in
  | Tarrow (_, arg_t, ret_t, _) -> type_contains env arg_t t_in || type_contains env ret_t t_in
  | Tconstr (_, ts, _) | Ttuple ts -> List.exists (fun t -> type_contains env t t_in) ts
  | _ -> false

type arg_kind = Named of string | Unnamed of int

module SMap = Map.Make (String)

type fn_info = {
  pat : pattern;
  expr : expression;
  (* the typ is expr.exp_type with 'Tvar None' rewritten as 'Tvar Some _n' *)
  typ : type_expr;
  (* the head of path is the function name *)
  path : string list;
  arg_typs : (arg_kind * type_expr) list;
  mutable typ_var_map : type_expr SMap.t;
  (* when two or more functions have the same name in a module, only the last one will be exposed *)
  mutable last : bool;
  mutable sig_typ : type_expr option;
}

(* dummy Ident.t to use in order to initialize a ref *)
let dummy_id = Ident.create_local "@@@"

let last_such_that_path_matches ~tbl ~path =
  let res_id = ref dummy_id in
  Hashtbl.iter
    (fun id { path = path'; last; _ } -> if path = path' && last then res_id := id else ())
    tbl;
  if !res_id = dummy_id then None else Some !res_id

let name_typ_var t =
  let n = ref 0 in
  let rec aux t =
    let desc =
      match t.desc with
      | Tvar None ->
          incr n;
          Tvar (Some (Printf.sprintf "_%d" (!n - 1)))
      | Tarrow (l, t, t', x) -> Tarrow (l, aux t, aux t', x)
      | Tconstr (p, ts, x) -> Tconstr (p, List.map aux ts, x)
      | Ttuple ts -> Ttuple (List.map aux ts)
      | Tlink t -> Tlink (aux t)
      | _ -> t.desc
    in
    { t with desc }
  in
  aux t

let get_typ_vars t =
  let open Set.Make (String) in
  let rec aux t =
    match t.desc with
    | Tvar (Some s) -> singleton s
    | Tvar None -> empty (* TODO handle anonymous type variables *)
    | Tarrow (_, arg_t, ret_t, _) -> union (aux arg_t) (aux ret_t)
    | Tconstr (_, ts, _) | Ttuple ts -> ts |> List.map aux |> List.fold_left union empty
    | Tlink t -> aux t
    | _ -> empty
    (* TODO what do we want to handle later ? *)
  in
  aux t |> elements

let get_arg_typs t =
  let rec aux n t =
    match t.desc with
    | Tarrow (label, arg_t, ret_t, _) ->
        let label, n =
          match label with Labelled s | Optional s -> (Named s, n) | Nolabel -> (Unnamed n, n + 1)
        in
        (label, arg_t) :: aux n ret_t
    | Tlink t -> aux n t
    | _ -> []
  in
  aux 0 t

let value_binding ~tbl ~path sub ({ vb_pat; vb_expr; _ } as vb) =
  ( match vb_pat.pat_desc with
  | Tpat_var (id, { txt = name; _ }) ->
      let path = name :: !path in
      ( match last_such_that_path_matches ~tbl ~path with
      | None -> ()
      | Some id ->
          let info = Hashtbl.find tbl id in
          info.last <- false );
      let typ = name_typ_var vb_expr.exp_type in
      let typ_vars = get_typ_vars typ in
      if List.length typ_vars > 0 then
        let typ_var_map = SMap.empty in
        let arg_typs = get_arg_typs typ in
        let info =
          {
            pat = vb_pat;
            expr = vb_expr;
            typ;
            path;
            arg_typs;
            typ_var_map;
            last = true;
            sig_typ = None;
          }
        in
        Hashtbl.add tbl id info
  | _ -> () );
  default_iterator.value_binding sub vb

let module_binding ~path sub mb =
  let old_path = !path in
  (path := match mb.mb_id with Some id -> Ident.name id :: !path | None -> []);
  default_iterator.module_binding sub mb;
  path := old_path

let rec compare_to_generic gen_t t =
  match (gen_t.desc, t.desc) with
  | Tvar (Some s), _ -> [ (s, t) ]
  | Tlink gen_t, _ -> compare_to_generic gen_t t
  | _, Tlink t -> compare_to_generic gen_t t
  | Tarrow (_, gen_arg, gen_ret, _), Tarrow (_, arg, ret, _) ->
      compare_to_generic gen_arg arg @ compare_to_generic gen_ret ret
  | Ttuple gen_ts, Ttuple ts | Tconstr (_, gen_ts, _), Tconstr (_, ts, _) -> (
      try List.map2 compare_to_generic gen_ts ts |> List.flatten with Invalid_argument _ -> [] )
  | _ -> []

let rec intersect_typs t1 t2 =
  let desc =
    match (t1.desc, t2.desc) with
    | Tvar v1, Tvar v2 when v1 = v2 -> Tvar v1
    | Tlink t1, _ -> (intersect_typs t1 t2).desc
    | _, Tlink t2 -> (intersect_typs t1 t2).desc
    | Tarrow (l1, arg1, ret1, x), Tarrow (l2, arg2, ret2, _) when l1 = l2 ->
        let arg = intersect_typs arg1 arg2 in
        let ret = intersect_typs ret1 ret2 in
        Tarrow (l1, arg, ret, x)
    | Ttuple ts1, Ttuple ts2 -> (
        try
          let ts = List.map2 intersect_typs ts1 ts2 in
          Ttuple ts
        with Invalid_argument _ -> Tvar None )
    | Tconstr (p1, ts1, x), Tconstr (p2, ts2, _) when p1 = p2 ->
        let ts = List.map2 intersect_typs ts1 ts2 in
        Tconstr (p1, ts, x)
    (* if types don't match, then we replace by a type variable *)
    | _, _ -> Tvar None
  in
  { t1 with desc }

let expr ~tbl sub texp =
  match texp.exp_desc with
  | Texp_apply ({ exp_desc = Texp_ident (Path.Pident id, _, _); _ }, args) when Hashtbl.mem tbl id
    ->
      let info = Hashtbl.find tbl id in
      let n = ref 0 in
      List.iter
        (fun (label, arg_exp) ->
          try
            let label =
              match label with
              | Labelled s | Optional s -> Named s
              | Nolabel ->
                  incr n;
                  Unnamed (!n - 1)
            in
            Option.iter
              (fun arg_exp ->
                let generic_arg_typ = List.assoc label info.arg_typs in
                let arg_typ = arg_exp.exp_type in
                List.iter
                  (fun (var, typ) ->
                    let typ =
                      match SMap.find_opt var info.typ_var_map with
                      | Some typ' -> intersect_typs typ typ'
                      | None -> typ
                    in
                    info.typ_var_map <- SMap.add var typ info.typ_var_map)
                  (compare_to_generic generic_arg_typ arg_typ))
              arg_exp
          with Not_found -> ())
        args;
      default_iterator.expr sub texp
  | Texp_ident (Path.Pident id, _, _) when Hashtbl.mem tbl id ->
      let info = Hashtbl.find tbl id in
      compare_to_generic info.typ texp.exp_type
      |> List.iter (fun (var, typ) ->
             let typ =
               match SMap.find_opt var info.typ_var_map with
               | Some typ' -> intersect_typs typ typ'
               | None -> typ
             in
             info.typ_var_map <- SMap.add var typ info.typ_var_map)
  | _ -> default_iterator.expr sub texp

let process_signature ~tbl : signature_item list -> unit =
  let rec process path = function
    | Sig_value (id, { val_type; _ }, Exported) -> (
        let path = Ident.name id :: path in
        match last_such_that_path_matches ~tbl ~path with
        | None -> ()
        | Some id ->
            let info = Hashtbl.find tbl id in
            compare_to_generic info.typ val_type
            |> List.iter (fun (var, typ) ->
                   let typ =
                     match SMap.find_opt var info.typ_var_map with
                     | Some typ' -> intersect_typs typ typ'
                     | None -> typ
                   in
                   info.typ_var_map <- SMap.add var typ info.typ_var_map) )
    | Sig_module (id, _, decl, _, _) -> (
        match decl.md_type with
        | Mty_signature s ->
            let m_name = Ident.name id in
            List.iter (process (m_name :: path)) s
        | _ -> () )
    | _ -> ()
  in
  List.iter (process [])

let rec rewrite_type_vars ~var_map ~env t =
  let aux = rewrite_type_vars ~var_map ~env in
  let desc =
    match t.desc with
    | Tvar (Some s) ->
        SMap.find_opt s var_map |> Option.fold ~none:(Tvar None) ~some:(fun t -> t.desc)
    | Tlink t -> (aux t).desc
    | Tarrow (l, arg, ret, x) -> Tarrow (l, aux arg, aux ret, x)
    | Ttuple ts -> Ttuple (List.map aux ts)
    | Tconstr (p, ts, x) ->
        Tconstr (p, List.map aux ts, x) (* TODO: check that the constructor is in the env *)
    | _ -> t.desc
  in
  { t with desc }

(* let rewrite_type_match env t t_match = let rec aux t = if moregeneral env t_match t then (true,
   t_match) else let desc = match t.desc with | Tlink t -> (aux t |> snd).desc | Tarrow (l, arg,
   ret, x) -> let arg_match, new_arg = aux arg in let ret_match, new_ret = aux ret in if arg_match
   || ret_match then Tarrow (l, new_arg, new_ret, x) else Tvar None | Ttuple ts -> let matches,
   new_ts = List.map aux ts |> List.split in if List.fold_left ( || ) false matches then Ttuple
   new_ts else Tvar None | Tconstr (p, ts, x) -> let matches, new_ts = List.map aux ts |> List.split
   in if List.fold_left ( || ) false matches then Tconstr (p, new_ts, x) else Tvar None | _ -> Tvar
   None in (desc <> Tvar None, { t with desc }) in snd (aux t) *)

let get_exp_constraint_opt =
  List.fold_left (fun acc (x, _, _) -> match x with Texp_constraint t -> Some t | _ -> acc) None

let get_pat_constraint_opt =
  List.fold_left (fun acc (x, _, _) -> match x with Tpat_constraint t -> Some t | _ -> acc) None

let gen_annot_from_info ~typ_match info annots =
  let rec gen_annot_aux annots expr fun_typ =
    match expr.exp_desc with
    | Texp_function
        {
          arg_label = Optional _ as label;
          cases = [ { c_rhs = { exp_desc = Texp_let (_, _, expression); _ }; _ } ];
          _;
        } ->
        (* consume the argument *)
        let _, fun_typ = get_arg_and_ret label fun_typ in
        gen_annot_aux annots expression fun_typ
    | Texp_function { arg_label; cases = [ { c_lhs; c_rhs; _ } ]; _ } -> (
        let env = info.expr.exp_env in
        let var_map = info.typ_var_map in
        let arg_typ, ret_typ = get_arg_and_ret arg_label fun_typ in
        let typ = rewrite_type_vars ~var_map ~env arg_typ in
        let annots =
          if typ.desc <> Tvar None && type_contains env typ typ_match then
            let typ_str = print_typ typ in
            match get_pat_constraint_opt c_lhs.pat_extra with
            | Some t ->
                let loc = t.ctyp_loc in
                let loc = (loc.loc_start.pos_cnum, loc.loc_end.pos_cnum) in
                mk_annot_patch ~loc (fun _ -> typ_str) :: annots
            | None ->
                let loc = c_lhs.pat_loc in
                let loc = (loc.loc_start.pos_cnum, loc.loc_end.pos_cnum) in
                mk_annot_patch ~loc (fun txt ->
                    Printf.sprintf "(%s : (%s)[@tmp_annot])" txt typ_str)
                :: annots
          else annots
        in
        match c_rhs.exp_desc with
        | Texp_function _ -> gen_annot_aux annots c_rhs ret_typ
        | _ ->
            (* generate return type annotation here *)
            let ret_typ = rewrite_type_vars ~var_map ~env ret_typ in
            if ret_typ.desc <> Tvar None && type_contains env ret_typ typ_match then
              let typ_str = print_typ ret_typ in
              match
                (get_exp_constraint_opt c_rhs.exp_extra, get_pat_constraint_opt c_lhs.pat_extra)
              with
              | Some t, _ ->
                  let loc = t.ctyp_loc in
                  let loc = (loc.loc_start.pos_cnum, loc.loc_end.pos_cnum) in
                  mk_annot_patch ~loc (fun _ -> typ_str) :: annots
              | _, Some t ->
                  let pos = t.ctyp_loc.loc_end.pos_cnum + 1 in
                  let loc = (pos, pos) in
                  mk_annot_patch ~loc (fun _ -> Printf.sprintf " : ((%s)[@tmp_annot_ret])" typ_str)
                  :: annots
              | _ ->
                  let pos = c_lhs.pat_loc.loc_end.pos_cnum in
                  let loc = (pos, pos) in
                  mk_annot_patch ~loc (fun _ -> Printf.sprintf " : ((%s)[@tmp_annot_ret])" typ_str)
                  :: annots
            else annots )
    | _ -> annots
  in
  try gen_annot_aux annots info.expr info.typ with Not_found -> annots

let generate_annotations ~tbl ~typ_match =
  Hashtbl.fold (fun _ -> gen_annot_from_info ~typ_match) tbl []

let annotate ~(typ_match : Types.type_expr) ~(has_changed : bool ref) (src_file : string)
    (cmt_file : string) =
  if Filename.check_suffix cmt_file ".cmt" then (
    let cmt = Cmt_format.read_cmt cmt_file in
    let cmi_file = Filename.chop_suffix cmt_file ".cmt" ^ ".cmi" in
    let cmi = Cmi_format.read_cmi cmi_file in
    let tbl = Hashtbl.create 10 in
    let path = ref [] in
    let finder =
      {
        default_iterator with
        expr = expr ~tbl;
        module_binding = module_binding ~path;
        value_binding = value_binding ~tbl ~path;
      }
    in
    begin
      match cmt.cmt_annots with
      | Implementation str -> finder.structure finder str
      | Interface sg -> finder.signature finder sg
      | _ -> Printf.printf "Not an implementation nor an interface : %s\n%!" cmt_file
    end;
    process_signature ~tbl cmi.cmi_sign;
    let annots = generate_annotations ~tbl ~typ_match in
    if annots <> [] then (
      let old_digest = Digest.file src_file in
      Io.read src_file |> apply_patch annots |> Io.write src_file;
      let new_digest = Digest.file src_file in
      if not (Digest.equal new_digest old_digest) then has_changed := true ) )
