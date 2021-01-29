(******************************************************************)
(* Copyright (C) 2020-2021 Nicolas Chataing. All rights reserved. *)
(*                                                                *)
(* This software may be modified and distributed under the terms  *)
(* of the BSD license.  See the LICENSE file for details.         *)
(******************************************************************)

val iter_module_descrs : path:string -> f:(string -> string -> unit) -> ignored:string list -> unit
(** Iterate over the function `f` over the modules in `path`, ignoring the `ignored` files. The
    function `f` takes the path to a ml file and a path to its annotation (e.g. foo.ml and foo.cmt).
    This function relies on `dune describe` so the target repository should be a dune project *)
