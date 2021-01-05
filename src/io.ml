(******************************************************************)
(* Copyright (C) 2020-2021 Nicolas Chataing. All rights reserved. *)
(*                                                                *)
(* This software may be modified and distributed under the terms  *)
(* of the BSD license.  See the LICENSE file for details.         *)
(******************************************************************)

let read filename =
  let ic = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let write filename txt =
  let oc = open_out_bin filename in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc txt)

let with_chdir (dir : string) (f : unit -> 'a) : 'a =
  let old = Sys.getcwd () in
  Sys.chdir dir;
  Fun.protect ~finally:(fun () -> Sys.chdir old) f
