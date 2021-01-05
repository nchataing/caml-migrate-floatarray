(******************************************************************)
(* Copyright (C) 2020-2021 Nicolas Chataing. All rights reserved. *)
(*                                                                *)
(* This software may be modified and distributed under the terms  *)
(* of the BSD license.  See the LICENSE file for details.         *)
(******************************************************************)

open Typedtree
open Tast_iterator

let put_on_child exp =
  match exp.exp_desc with
  | Texp_constant (Asttypes.Const_float f) when f.[0] = '-' -> true
  | Texp_ident _ | Texp_array _ | Texp_constant _ -> false
  | _ -> true

let put_par = ref false

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
  | Texp_apply
      ( {
          exp_desc =
            Texp_ident
              ( Path.Pdot
                  ( _,
                    ( "+." | "-." | "*." | "/." | "**" | "=" | "<>" | "<" | ">" | "<=" | ">=" | "=="
                    | "!=" ) ),
                _,
                _ );
          exp_loc = op_loc;
          _;
        },
        [ (_, Some arg1); (_, Some arg2) ] ) ->
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

let iterator = { default_iterator with expr = handle_par_expr }

let put () = !put_par
