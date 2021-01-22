type t

type loc = int * int

val get_loc : t -> loc

val mk_rewrite_patch : loc:loc -> ?par:bool -> string -> t

val mk_remove_patch : loc:loc -> t

val mk_seq_patch : loc:loc -> ?par:bool -> t list -> t

val mk_get_patch : loc:loc -> t -> t -> t

val mk_set_patch : loc:loc -> t -> t -> t -> t

val mk_array_constr_patch : loc:loc -> ?par:bool -> t list -> t

val apply_patch : t list -> string -> string
