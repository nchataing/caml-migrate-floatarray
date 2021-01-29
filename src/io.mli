(******************************************************************)
(* Copyright (C) 2020-2021 Nicolas Chataing. All rights reserved. *)
(*                                                                *)
(* This software may be modified and distributed under the terms  *)
(* of the BSD license.  See the LICENSE file for details.         *)
(******************************************************************)

val read : string -> string
(** Securely reads a file *)

val write : string -> string -> unit
(** Securely writes to a file *)

val with_chdir : string -> (unit -> 'a) -> 'a
(** with_chdir dir f executes the function f after having changed directory to dir, and then comes
    back to the working directory *)
