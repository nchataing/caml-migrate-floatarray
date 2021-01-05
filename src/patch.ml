(******************************************************************)
(* Copyright (C) 2020-2021 Nicolas Chataing. All rights reserved. *)
(*                                                                *)
(* This software may be modified and distributed under the terms  *)
(* of the BSD license.  See the LICENSE file for details.         *)
(******************************************************************)

type loc = int * int

type kind =
  | Seq (* sequential patching, sub patches are applied in order *)
  | F of (string -> string list -> string)

(* a function defines how to rearrange sub patches *)

type t = { loc : loc; kind : kind; sub : t list; par : bool }

let get_loc { loc; _ } = loc

let par_start_end_match s =
  let n = String.length s in
  s.[0] = '('
  && s.[n - 1] = ')'
  &&
  let par_count = ref 0 in
  let res = ref true in
  for i = 1 to String.length s - 2 do
    if s.[i] = '(' then incr par_count else if s.[i] = ')' then decr par_count;
    if !par_count = -1 then res := false
  done;
  !res

let rec patch_to_str txt { loc = s, e; kind; sub; par } =
  let sub = List.sort (fun t1 t2 -> compare t1.loc t2.loc) sub in
  let sub_str = List.map (fun ({ loc; _ } as p) -> (loc, patch_to_str txt p)) sub in
  let str =
    match kind with
    | Seq ->
        let off, str =
          List.fold_left
            (fun (off, x) ((s, e), str) ->
              (e, Printf.sprintf "%s%s%s" x (String.sub txt off (s - off)) str))
            (s, "") sub_str
        in
        str ^ String.sub txt off (e - off)
    | F f -> List.map snd sub_str |> f txt
  in
  if par && not (par_start_end_match str) then "(" ^ str ^ ")" else str

let mk_rewrite_patch ~loc ?(par = false) str = { loc; kind = F (fun _ _ -> str); sub = []; par }

let mk_remove_patch ~loc = { loc; kind = F (fun _ _ -> ""); sub = []; par = false }

let mk_seq_patch ~loc ?(par = false) sub = { loc; kind = Seq; sub; par }

let mk_get_patch ~loc arr_p index_p =
  {
    loc;
    kind =
      F (fun _ l -> match l with [ a; i ] -> Printf.sprintf "%s.!(%s)" a i | _ -> assert false);
    sub = [ arr_p; index_p ];
    par = false;
  }

let mk_set_patch ~loc arr_p index_p val_p =
  {
    loc;
    kind =
      F
        (fun _ l ->
          match l with [ a; i; v ] -> Printf.sprintf "%s.!(%s) <- %s" a i v | _ -> assert false);
    sub = [ arr_p; index_p; val_p ];
    par = false;
  }

let mk_array_constr_patch ~loc ?(par = false) sub =
  {
    loc;
    kind =
      F
        (fun txt -> function
          | [] -> "Floatarray.empty ()"
          | [ s1 ] -> Printf.sprintf "Floatarray.make1 %s" s1
          | [ s1; s2 ] -> Printf.sprintf "Floatarray.make2 %s %s" s1 s2
          | [ s1; s2; s3 ] -> Printf.sprintf "Floatarray.make3 %s %s %s" s1 s2 s3
          | [ s1; s2; s3; s4 ] -> Printf.sprintf "Floatarray.make4 %s %s %s %s" s1 s2 s3 s4
          | _ ->
              let sub = List.map (fun p -> { p with par = false }) sub in
              patch_to_str txt { loc; kind = Seq; sub; par }
              |> Printf.sprintf "Floatarray.from_array %s");
    sub;
    par;
  }

let mk_annot_patch ~loc f = { loc; kind = F (fun txt _ -> f txt); sub = []; par = false }

let apply_patch patches txt =
  let loc = (0, String.length txt - 1) in
  mk_seq_patch ~loc patches |> patch_to_str txt
