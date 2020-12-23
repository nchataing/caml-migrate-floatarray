val moregeneral : Env.t -> Types.type_expr -> Types.type_expr -> bool
val moregeneral_expand_env : Env.t -> Types.type_expr -> Types.type_expr -> bool

val exp_type_match : Typedtree.expression -> string -> bool 

val split_arrow_typ : Types.type_expr -> (Asttypes.arg_label * Types.type_expr) list

val parse_typ : string -> Types.type_expr

val print_typ : Types.type_expr -> string
val print_path : Path.t -> string

val ctyp_path_of_string : string -> Path.t
