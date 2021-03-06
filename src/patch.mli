(******************************************************************)
(* Copyright (C) 2020-2021 Nicolas Chataing. All rights reserved. *)
(*                                                                *)
(* This software may be modified and distributed under the terms  *)
(* of the BSD license.  See the LICENSE file for details.         *)
(******************************************************************)

type t

type loc = int * int

val get_loc : t -> loc

val mk_rewrite_patch : loc:loc -> ?par:bool -> string -> t

val mk_remove_patch : loc:loc -> t

val mk_seq_patch : loc:loc -> ?par:bool -> t list -> t

val mk_ghost_get_patch : loc:loc -> op_descr:string -> t -> t -> t

val mk_get_patch : loc:loc -> par:bool -> t -> t -> t

val mk_ghost_set_patch : loc:loc -> op_descr:string -> t -> t -> t -> t

val mk_set_patch : loc:loc -> par:bool -> t -> t -> t -> t

val mk_array_constr_patch : loc:loc -> ?par:bool -> t list -> t

val mk_annot_patch : loc:loc -> (string -> string) -> t

val apply_patch : t list -> string -> string
