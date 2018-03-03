open Phantom_algebra.Core
open Phantom_algebra.Type_functions
let (|?) x name =
  let line ppf =
    Format.fprintf ppf
      "——————————————————————————————————————————————————————————————————————@,"
  in
  Format.printf
    "@[<v>%t    \x1b[94;4m%s\x1b[97;24m@,@;<0 8>@[%a@]@,@]" line name pp x; x

let v = vec2 0. 1. |? "v (0 1)"
let m = mat2 (vec2 1. 0.) (scalar 2. * v) |? "m (1 0 | 2 * v) "
let m2 = m * m  |? "m * m "
let ez = v * m  |? "v * m"

let ew = vec3 0. 0. 1.  |? "(0 0 1)"
let s = scalar 7.  |? "s=7"
let ew = v + vec2 1. 1.  |? "v + 1=(1 2)"
let v' = s + v  |? "s + v = (7,8)"
let v'' = s * v  |? "s * v = (0 7)"

let v''' = v' * ( scalar 1. + v'') |? "v' * ( 1 + v'') = (7, 64)"

let xyrot theta = mat3
    (vec3 (cos theta) (sin theta) 0.)
    (vec3 (~-.(sin theta)) (cos theta) 0.)
    (vec3 0. 0. 1.)


let pi = 4. *. atan 1.
let r = xyrot (pi /.6.) |? "Rxy_(π/6)"



let e1 = vec3 1. 0. 0. |? "e1=(1 0 0)"
let e2 = xyrot (pi /. 2.) * e1 |? "R_(π/2) e1 = (0 1 0)"

let r = exp ( scalar (pi /. 6.) * e1 ^ e2 ) |? "exp (π/6 (dx ^ dy)) = Rxy_(π/6)"


let e3 = cross e1 e2 |? "*(e1 ^ e2) = e3"
let f2 = e1 ^ e2 |? "dx ^ dy"

let m_one = cross (vec2 0. 1.) (vec2 1. 0.) |? "(0 1) ^ (1 0) = -1"

let v4 = scalar 0. |+| vec2 1. 2. |+| scalar 3. |? "v4=(0 | 1 2 | 3)"
let v4' = vec2 1. 2. |+| vec2 3. 4. |? "(1 2 | 3 4)"

let stretch = vec4' (vec2 3. 2. |+| scalar 1.) |? "vec4'( 3 2 | 1) = (3 2 1 1)"
let eye = mat2 (vec2 1. 0.) (vec2 0. 1.) |? "eye"

let d1 = mat2 (vec2 2. 0.) (vec2 0. 1.) |? "d1= diag (2 1)"
let d2 = mat2 (vec2 1. 0.) (vec2 0. 2.) |? "d2= diag(1 2)"
let d3 = d1 / d2 |? "d1/d2"

let exp_eye = exp eye |? "exp id = diag(e,e)"

let sym = mat2 (vec2 0. 1.) (vec2 1. 0.)

let t = mat2 (vec2 1. 2.) (vec2 3. 4.) * sym |? "test"
let sym = eye / sym |? "1/sym"

let mid = ( xyrot (pi/. 24.) ** 24 ) |? "R_(π/24) ** 24 = -xy "
let mid = ( xyrot (pi/. 7.) ** (-7) ) |? "R_(π/7) ** -7 = -xy"

;;

module Random = struct
;; Random.self_init ()
let u () = Random.float 1.
  let (<*>) f x () = f (x ())
  let (<$>) f x () = f()(x())
  let scalar = scalar <*> u
  let vec2 = vec2 <*> u <$> u
  let vec3 = vec3 <*> u <$> u <$> u
  let vec4 = vec4 <*> u <$> u <$> u <$> u
  let mat2 = mat2 <*> vec2 <$> vec2
  let mat3 = mat3 <*> vec3 <$> vec3 <$> vec3
  let mat4 = mat4 <*> vec4 <$> vec4 <$> vec4 <$> vec4
end

let rand =
  let m1 = Random.mat3 () |? "rm1" in
  let m2 = Random.mat3 () |? "rm2" in
  let m3 = m1 * m2 |? "rm1 * rm2" in
  (m3 / m2 - m1) |? "rm1 * rm2 / rm2 - m1 "
;;
#if OCAML_MAJOR>=4 && OCAML_MINOR>=6
let f = v.%[x'] |? "v.(0) = 0"
let ryy = r.%[yy'] |? "r.(yy)"
let rx = r.%[x'] |? "r.(x)"

let sel = w'&z'&y'&x'
let sw = v4 .%[sel] |? "v4.(3210) = (3 2 1 0)"

let msw = eye.%[y'&x'] |? "Id[1,0] = xy-sym"

let diag = eye.%[xx'&xy'&yy'] |? "Id(xx,xy,yy) = (1 0 1)"
#endif
(*
let error = cross (vec4 0. 0. 0. 1.) (vec4 0. 1. 0. 0.)
let error' = cross (scalar 1.) (scalar 2.)
*)

module X :sig val t: _ scalar end = struct
let fn v w =
  (cross v w) + scalar 1.
let t = fn (vec2 0. 1.) (vec2 1. 0.)
end
