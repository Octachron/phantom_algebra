#if OCAML_MAJOR>=4 && OCAML_MINOR>=6

type elt = float
type t = floatarray
let len = Array.Floatarray.length
let get = Array.Floatarray.unsafe_get
let set = Array.Floatarray.unsafe_set

let create = Array.Floatarray.create

#else

type elt = float
type t = elt array
let len = Array.length
let get = Array.unsafe_get
let set = Array.unsafe_set

let create = Array.create_float
#endif
