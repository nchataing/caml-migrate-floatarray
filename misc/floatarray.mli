val from_array : float array -> floatarray

val to_array : floatarray -> float array

val make1 : float -> floatarray

val make2 : float -> float -> floatarray

val make3 : float -> float -> float -> floatarray

val make4 : float -> float -> float -> float -> floatarray

val make_matrix : int -> int -> float -> floatarray array

val mapi_to_array : (int -> float -> 'a) -> floatarray -> 'a array

val mapi_from_array : (int -> 'a -> float) -> 'a array -> floatarray
