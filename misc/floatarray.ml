let (.!()) = Float.Array.get
let (.!()<-) = Float.Array.set

type a1 = { x : float }
type a2 = { x : float; y : float }
type a3 = { x : float; y : float; z : float }
type a4 = { x : float; y : float; z : float; t : float }

let make1 x : floatarray = Obj.magic { x }
let make2 x y : floatarray = Obj.magic { x; y }
let make3 x y z : floatarray = Obj.magic { x; y; z }
let make4 x y z t : floatarray = Obj.magic { x; y; z; t }

let make_matrix n m x =
  Array.init n (fun _ -> Float.Array.make m x)

let mapi_to_array f a =
  Array.init (Float.Array.length a) (fun i -> f i (Float.Array.unsafe_get a i))

let mapi_from_array f a =
  Float.Array.init (Array.length a) (fun i -> f i (Array.unsafe_get a i))
