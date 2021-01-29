(******************************************************************)
(* Copyright (C) 2020-2021 Nicolas Chataing. All rights reserved. *)
(*                                                                *)
(* This software may be modified and distributed under the terms  *)
(* of the BSD license.  See the LICENSE file for details.         *)
(******************************************************************)

val iterator : Tast_iterator.iterator
(** Iterator based on Tast_iterator.default_iterator that computes at the same time if parenthesis
    should be put around expressions *)

val put : unit -> bool
(** When using the above iterator put () tells if parenthesis should be put around the current
    expression *)

val put_on_child : Typedtree.expression -> bool
(** Tells if parenthesis should be put around the children of an expression *)
