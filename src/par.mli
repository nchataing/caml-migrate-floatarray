(******************************************************************)
(* Copyright (C) 2020-2021 Nicolas Chataing. All rights reserved. *)
(*                                                                *)
(* This software may be modified and distributed under the terms  *)
(* of the BSD license.  See the LICENSE file for details.         *)
(******************************************************************)

val iterator : Tast_iterator.iterator

val put : unit -> bool

val put_on_child : Typedtree.expression -> bool
