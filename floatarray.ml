let to_array = Float.Array.to_array Fun.id
let from_array = Float.Array.from_array Fun.id

let empty : floatarray = Obj.magic [||]

type t1 = { x1 : float}
type t2 = { x1 : float; x2 : float }
type t3 = { x1 : float; x2 : float; x3 : float }
type t4 = { x1 : float; x2 : float; x3 : float; x4 : float }

let make1 x1 : floatarray = Obj.magic { x1 }
let make2 x1 x2 : floatarray = Obj.magic { x1; x2 }
let make3 x1 x2 x3 : floatarray = Obj.magic { x1; x2; x3 }
let make4 x1 x2 x3 x4 : floatarray = Obj.magic { x1; x2; x3; x4 }

let mapi_to_array f a = Array.init (Float.Array.length a) (fun i -> f i (Float.Array.unsafe_get a i))

let mapi_from_array f a = Float.Array.init (Array.length a) (fun i -> f i (Array.unsafe_get a i))

let make_matrix n m v = Array.init (fun _ -> Float.Array.init (fun _ -> v))
