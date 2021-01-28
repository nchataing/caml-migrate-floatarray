(******************************************************************)
(* Copyright (C) 2020-2021 Nicolas Chataing. All rights reserved. *)
(*                                                                *)
(* This software may be modified and distributed under the terms  *)
(* of the BSD license.  See the LICENSE file for details.         *)
(******************************************************************)

(* TODO put log in a separate file *)
module Log = struct
  let log : Buffer.t = Buffer.create 0

  let add_file_entries file entries =
    if List.length entries <> 0 then begin
      Buffer.add_string log (Printf.sprintf "In file %s\n" file);
      entries
      |> List.sort (fun x y -> compare (snd x) (snd y))
      |> List.iter (fun (msg, loc) -> Buffer.add_string log (Printf.sprintf "[%d] %s\n" loc msg))
    end
end

open Typedtree
open Tast_iterator
open Patch
open Typ_utils

let add_to (l : 'a list ref) (elt : 'a) = l := elt :: !l

let get_loc texp = (texp.exp_loc.loc_start.pos_cnum, texp.exp_loc.loc_end.pos_cnum)

(* let loc_in (s1,e1) (s2,e2) = s2 <= s1 && e1 <= e2 *)

let fetch_attr_loc str =
  List.fold_left
    (fun acc attr -> if attr.Parsetree.attr_name.txt = str then Some attr.attr_loc else acc)
    None

let fetch_attr str =
  List.fold_left
    (fun acc attr -> if attr.Parsetree.attr_name.txt = str then Some attr else acc)
    None

let get_payload_string =
  let open Parsetree in
  function
  | PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_ident { txt; _ }; _ }, _); _ } ] ->
      Longident.last txt
  | PStr [] -> ""
  | _ -> assert false

let value_binding patches _ iter vb =
  match fetch_attr_loc "ignore" vb.vb_attributes with
  | Some loc ->
      let loc = (loc.loc_start.pos_cnum, loc.loc_end.pos_cnum) in
      patches := mk_remove_patch ~loc :: !patches
  | None -> default_iterator.value_binding iter vb

type action = Patch of string | Log

let fun_tbl = Hashtbl.create 30

let () =
  List.iter
    (fun (key, value) -> Hashtbl.add fun_tbl key value)
    [
      ("Array.length", [ ("float array -> _", Patch "Float.Array.length") ]);
      ("Array.get", [ ("float array -> _", Patch "Float.Array.get") ]);
      ("Array.set", [ ("float array -> _", Patch "Float.Array.set") ]);
      ("Array.make", [ ("_ -> float -> _", Patch "Float.Array.make") ]);
      ("Array.create", [ ("_ -> float -> _", Patch "Float.Array.create") ]);
      ("Array.create_float", [ ("_", Patch "Float.Array.create") ]);
      ("Array.make_float", [ ("_", Patch "Float.Array.create") ]);
      ("Array.init", [ ("_ -> (_ -> float) -> _", Patch "Float.Array.init") ]);
      ("Array.make_matrix", [ ("_ -> _ -> float -> _", Patch "Floatarray.make_matrix") ]);
      ("Array.create_matrix", [ ("_ -> _ -> float -> _", Patch "Floatarray.make_matrix") ]);
      ("Array.append", [ ("float array -> _", Patch "Float.Array.append") ]);
      ("Array.concat", [ ("float array list -> _", Patch "Float.Array.concat") ]);
      ("Array.sub", [ ("float array -> _", Patch "Float.Array.sub") ]);
      ("Array.copy", [ ("float array -> _", Patch "Float.Array.copy") ]);
      ("Array.fill", [ ("float array -> _", Patch "Float.Array.fill") ]);
      ("Array.blit", [ ("float array -> _", Patch "Float.Array.blit") ]);
      ("Array.to_list", [ ("float array -> _", Patch "Float.Array.to_list") ]);
      ("Array.of_list", [ ("float list -> _", Patch "Float.Array.of_list") ]);
      ("Array.iter", [ ("(float -> _) -> _", Patch "Float.Array.iter") ]);
      ("Array.iteri", [ ("(_ -> float -> _) -> _", Patch "Float.Array.iteri") ]);
      ( "Array.map",
        [
          ("(float -> float) -> _", Patch "Float.Array.map");
          ("(float -> _) -> _", Patch "Float.Array.map_to_array");
          ("(_ -> float) -> _", Patch "Float.Array.map_from_array");
        ] );
      ( "Array.mapi",
        [
          ("(int -> float -> float) -> _", Patch "Float.Array.mapi");
          ("(int -> float -> _) -> _", Patch "Floatarray.mapi_to_array");
          ("(int -> _ -> float) -> _", Patch "Floatarray.mapi_from_array");
        ] );
      ("Array.fold_left", [ ("(_ -> float -> _) -> _", Patch "Float.Array.fold_left") ]);
      ("Array.fold_right", [ ("(float -> _ -> _) -> _", Patch "Float.Array.fold_right") ]);
      ( "Array.iter2",
        [
          ("(float -> float -> _) -> _", Patch "Float.Array.iter2");
          ("(float -> _ -> _) -> _", Log);
          ("(_ -> float -> _) -> _", Log);
        ] );
      ( "Array.map2",
        [
          ("(float -> float -> float) -> _", Patch "Float.Array.map2");
          ("(float -> _ -> _) -> _", Log);
          ("(_ -> float -> _) -> _", Log);
          ("(_ -> _ -> float) -> _", Log);
        ] );
      ("Array.for_all", [ ("(float -> _) -> _", Patch "Float.Array.for_all") ]);
      ("Array.exists", [ ("(float -> _) -> _", Patch "Float.Array.exists") ]);
      ("Array.mem", [ ("float -> _", Patch "Float.Array.mem") ]);
      ("Array.memq", [ ("float -> _", Patch "Float.Array.memieee") ]);
      (* ??? *)
      ("Array.sort", [ ("(float -> float -> _) -> _", Patch "Float.Array.sort") ]);
      ("Array.stable_sort", [ ("(float -> float -> _) -> _", Patch "Float.Array.stable_sort") ]);
      ("Array.to_seq", [ ("float array -> _", Patch "Float.Array.to_seq") ]);
      ("Array.to_seqi", [ ("float array -> _", Patch "Float.Array.to_seqi") ]);
      ("Array.of_seq", [ ("_ -> float array", Patch "Float.Array.of_seq") ]);
      ("Array.unsafe_get", [ ("float array -> _", Patch "Float.Array.unsafe_get") ]);
      ("Array.unsafe_set", [ ("float array -> _", Patch "Float.Array.unsafe_set") ]);
    ]

exception Stop

let default_iterator = Par.iterator

let get_or_set_used = ref false

let rec expr patches file_log iter texp =
  match fetch_attr_loc "ignore" texp.exp_attributes with
  | Some loc ->
      let loc = (loc.loc_start.pos_cnum, loc.loc_end.pos_cnum) in
      mk_remove_patch ~loc |> add_to patches
  | None -> (
      match fetch_attr "rewrite" texp.exp_attributes with
      | Some attr ->
          let loc = (attr.Parsetree.attr_loc.loc_start.pos_cnum, attr.attr_loc.loc_end.pos_cnum) in
          patches := mk_remove_patch ~loc :: !patches;
          ( match texp.exp_desc with
          | Texp_apply (id, _) ->
              let s = get_payload_string attr.attr_payload in
              let loc = get_loc id in
              let loc = if s = "" then (fst loc, snd loc + 1) else loc in
              mk_rewrite_patch ~loc s |> add_to patches
          | _ -> () );
          expr_no_attr patches file_log iter texp
      | None -> expr_no_attr patches file_log iter texp )

and expr_no_attr patches file_log iter texp : unit =
  let par = Par.put () in
  let extra_done = ref true in
  begin
    match texp.exp_desc with
    | Texp_function { cases; _ } ->
        List.iter
          (fun { c_lhs; c_rhs; c_guard } ->
            List.iter
              (fun (x, _, _) ->
                match x with
                | Tpat_constraint t -> (
                    match fetch_attr_loc "tmp_annot" t.ctyp_attributes with
                    | Some attr_loc ->
                        let lpar = c_lhs.pat_loc.loc_start.pos_cnum - 1 in
                        let rpar = attr_loc.loc_end.pos_cnum in
                        let id_end = c_lhs.pat_loc.loc_end.pos_cnum in
                        mk_remove_patch ~loc:(lpar, lpar + 1) |> add_to patches;
                        mk_remove_patch ~loc:(id_end, rpar + 1) |> add_to patches
                    | None -> iter.typ iter t )
                | _ -> ())
              c_lhs.pat_extra;
            default_iterator.expr iter c_rhs;
            Option.iter (default_iterator.expr iter) c_guard)
          cases
    | Texp_array l when moregeneral_expand_env texp.exp_env (parse_typ "float array") texp.exp_type
      ->
        let elt_patches =
          List.map
            (fun elt_t ->
              let elt_patch = ref [] in
              let iter = find_float_iterator elt_patch file_log in
              iter.expr iter elt_t;
              mk_seq_patch ~loc:(get_loc elt_t) ~par:(Par.put_on_child elt_t) !elt_patch)
            l
        in
        mk_array_constr_patch ~loc:(get_loc texp) ~par elt_patches |> add_to patches
    | Texp_apply
        ( {
            exp_desc = Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "get"), _, _);
            exp_loc = { loc_ghost = true; _ };
            _;
          },
          [ (_, Some arr_exp); (_, Some i_exp) ] ) ->
        if moregeneral_expand_env texp.exp_env (parse_typ "float array") arr_exp.exp_type then (
          let local_patch = ref [] in
          let iter = find_float_iterator local_patch file_log in
          let arr_patch =
            iter.expr iter arr_exp;
            mk_seq_patch ~loc:(get_loc arr_exp) !local_patch
          in
          let i_patch =
            local_patch := [];
            iter.expr iter i_exp;
            mk_seq_patch ~loc:(get_loc i_exp) !local_patch
          in
          mk_get_patch ~loc:(get_loc texp) arr_patch i_patch |> add_to patches;
          extra_done := false;
          get_or_set_used := true )
    | Texp_apply
        ( {
            exp_desc = Texp_ident (Path.Pdot (Path.Pdot (_, "Array"), "set"), _, _);
            exp_loc = { loc_ghost = true; _ };
            _;
          },
          [ (_, Some arr_exp); (_, Some i_exp); (_, Some v_exp) ] ) ->
        if moregeneral_expand_env texp.exp_env (parse_typ "float array") arr_exp.exp_type then (
          let local_patch = ref [] in
          let iter = find_float_iterator local_patch file_log in
          let arr_patch =
            iter.expr iter arr_exp;
            mk_seq_patch ~loc:(get_loc arr_exp) !local_patch
          in
          let i_patch =
            local_patch := [];
            iter.expr iter i_exp;
            mk_seq_patch ~loc:(get_loc i_exp) !local_patch
          in
          let v_patch =
            local_patch := [];
            iter.expr iter i_exp;
            mk_seq_patch ~loc:(get_loc v_exp) !local_patch
          in
          mk_set_patch ~loc:(get_loc texp) arr_patch i_patch v_patch |> add_to patches;
          extra_done := false;
          get_or_set_used := true )
    | Texp_ident (path, _, _) ->
        Hashtbl.find_opt fun_tbl (print_path path)
        |> Option.iter (fun l ->
               try
                 List.iter
                   (fun (t_str, action) ->
                     if exp_type_match texp t_str then (
                       ( match action with
                       | Patch s -> mk_rewrite_patch ~loc:(get_loc texp) s |> add_to patches
                       | Log ->
                           file_log :=
                             (print_path path, texp.exp_loc.loc_start.pos_lnum) :: !file_log );
                       raise Stop ))
                   l
               with Stop -> ())
    (* Case where we inspect polymorphic uses of 'a array as float array : only fills the log *)
    (* | Texp_apply ({exp_desc=Texp_ident (p, _, {val_type; _}); exp_env=env; _}, args) -> let
       ret_typ = List.fold_left (fun f_typ (label,arg_exp) -> match arg_exp with | None -> f_typ |
       Some arg_exp -> let generic_arg_typ, f_typ = get_arg_typ label f_typ in let instance_arg_typ
       = arg_exp.exp_type in Option.iter (fun generic_arg_typ -> if
       poly_array_replaced_by_float_array env generic_arg_typ instance_arg_typ then let pos =
       arg_exp.exp_loc.loc_start.pos_lnum in file_log := (print_path p, pos) :: !file_log )
       generic_arg_typ; f_typ ) val_type args in let _ = List.fold_left (fun f_typ (label,arg_typ)
       -> let gen_arg_typ, f_typ = get_arg_typ label f_typ in let gen_arg_typ = Option.value
       ~default:f_typ gen_arg_typ in if poly_array_replaced_by_float_array env gen_arg_typ arg_typ
       then let pos = texp.exp_loc.loc_start.pos_lnum in file_log := (print_path p, pos) ::
       !file_log else (); f_typ ) ret_typ (split_arrow_typ texp.exp_type) in default_iterator.expr
       iter texp *)
    | _ -> default_iterator.expr iter texp
  end;
  let extra = function Texp_constraint cty -> iter.typ iter cty | _ -> () in
  if not !extra_done then List.iter (fun (e, _, _) -> extra e) texp.exp_extra

and typ patches iter ctyp =
  match fetch_attr_loc "tmp_annot_ret" ctyp.ctyp_attributes with
  | Some attr_loc ->
      let loc = (ctyp.ctyp_loc.loc_start.pos_cnum - 4, attr_loc.loc_end.pos_cnum + 2) in
      mk_remove_patch ~loc |> add_to patches
  | None -> (
      match fetch_attr_loc "ignore" ctyp.ctyp_attributes with
      | Some loc ->
          let loc = (loc.loc_start.pos_cnum, loc.loc_end.pos_cnum) in
          mk_remove_patch ~loc |> add_to patches
      | None -> (
          match ctyp.ctyp_desc with
          | Ttyp_constr (array_p, _, [ { ctyp_desc = Ttyp_constr (float_p, _, []); _ } ])
            when array_p = ctyp_path_of_string "'a array" && float_p = ctyp_path_of_string "float"
            ->
              let loc = (ctyp.ctyp_loc.loc_start.pos_cnum, ctyp.ctyp_loc.loc_end.pos_cnum) in
              if loc <> (-1, -1) then mk_rewrite_patch ~loc "floatarray" |> add_to patches
          | _ -> default_iterator.typ iter ctyp ) )

and find_float_iterator patches log =
  {
    default_iterator with
    value_binding = value_binding patches log;
    expr = expr patches log;
    typ = typ patches;
  }

let rec get_structure_of_mod_expr me =
  match me.mod_desc with
  | Tmod_structure s -> s
  | Tmod_functor (_, me) -> get_structure_of_mod_expr me
  | Tmod_constraint (me, _, _, _) -> get_structure_of_mod_expr me
  | _ -> assert false

let rec gen_open_patch str =
  if List.length str.str_items = 1 && (List.hd str.str_items).str_loc.loc_start.pos_cnum = -1 then
    match (List.hd str.str_items).str_desc with
    | Tstr_include { incl_mod; _ } -> get_structure_of_mod_expr incl_mod |> gen_open_patch
    | _ -> assert false
  else
    let last_open =
      List.fold_left
        (fun (last_open, ok) str_item ->
          match (str_item.str_desc, ok) with
          | Tstr_open _, true -> (Some str_item, true)
          | _ -> (last_open, false))
        (None, true) str.str_items
      |> fst
    in
    match last_open with
    | Some { str_loc; _ } ->
        let pos = str_loc.loc_end.pos_cnum in
        mk_rewrite_patch ~loc:(pos, pos) "\nopen Floatarray"
    | None ->
        let str_head =
          str.str_items |> List.filter (fun s -> s.str_loc.loc_start.pos_cnum <> -1) |> List.hd
        in
        let pos = str_head.str_loc.loc_start.pos_cnum in
        mk_rewrite_patch ~loc:(pos, pos) "open Floatarray\n\n"

let refactor (src_file : string) (cmt_file : string) : unit =
  let cmt = Cmt_format.read_cmt cmt_file in
  let patches = ref [] in
  let file_log = ref [] in
  let iter = find_float_iterator patches file_log in
  get_or_set_used := false;

  begin
    match cmt.cmt_annots with
    | Implementation str ->
        iter.structure iter str;
        if !get_or_set_used then gen_open_patch str |> add_to patches
    | Interface sg -> iter.signature iter sg
    | _ -> Printf.printf "Not an implementation nor an interface : %s\n%!" cmt_file
  end;
  let patches = !patches in
  if patches <> [] then (
    Io.read src_file |> apply_patch patches |> Io.write src_file;
    Log.add_file_entries src_file !file_log )
