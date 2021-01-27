val read : string -> string

val write : string -> string -> unit

val with_chdir : string -> (unit -> 'a) -> 'a
