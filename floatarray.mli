val to_array : floatarray -> float array
val from_array : float array -> floatarray
val empty : unit -> floatarray

val make1 : float -> floatarray
val make2 : float -> float -> floatarray
val make3 : float -> float -> float -> floatarray
val make4 : float -> float -> float -> float -> floatarray

val make_matrix : int -> int -> float -> floatarray

val mapi_to_array : (int -> float -> 'a) -> floatarray -> 'a array
val mapi_from_array : (int -> 'a -> float) -> 'a array -> floatarray

val (.!()) : floatarray -> int -> float
val (.!()<-) : floatarray -> int -> float -> unit
