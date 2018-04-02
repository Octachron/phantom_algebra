let const_e = exp 1.

open Phantom_algebra.Core
open Phantom_algebra.Type_functions
open Tools


let v = vec2 0. 1. |? "v (0 1)"
let m = "m = (1 0 | 2 * v)" :=
  mat2 (vec2 1. 0.) (scalar 2. * v) =? mat2 (vec2 1. 0.) (vec2 0. 2.)

let m2 = "m * m" := m * m  =? mat2 (vec2 1. 0.) (vec2 0. 4.)
let ez = "v * m" := v * m  =? vec2 0. 2.

let ew = vec3 0. 0. 1.  |? "(0 0 1)"
let s = scalar 7.  |? "s=7"

let ew = "(0 1) + 1" := v + vec2 1. 1.  =? vec2 1. 2.
let v' = "s + v" := s + v  =? vec2 7.  8.
let v'' = "s * v" := s * v  =? vec2 0. 7.

let v''' = "v' * ( 1 + v'')" := v' * ( scalar 1. + v'') =? vec2 7. 64.

let xyrot theta = mat3
    (vec3 (  cos theta) (sin theta) 0.)
    (vec3 (-.sin theta) (cos theta) 0.)
    (vec3        0.         0.      1.)


let pi = 4. *. atan 1.
let r = "Rxy_(π/6)" :=
    xyrot (pi /.6.) =?
    let x = sqrt 3. /. 2. and y = 0.5 in
    mat3 (vec3 x y 0.) (vec3 (-.y) x 0.) (vec3 0. 0. 1.)
let r' =
"rotation x y (π/6)" :=
  rotation (vec3 1. 0. 0.) (vec3 0. 1. 0.) (pi /. 6.) =? r

let e1 = vec3 1. 0. 0. |? "e1=(1 0 0)"
let e2 = "R_(π/2) e1 = e2" := xyrot (pi /. 2.) * e1 =? vec3 0. 1. 0.

let r =
  "exp (π/6 (dx ^ dy)) = Rxy_(π/6)" :=
    exp ( scalar (pi /. 6.) * e1 ^ e2 ) =? r


let e3 = "*(e1 ^ e2)=e3" := cross e1 e2 =? vec3 0. 0. 1.
let f2 = "dx ^ dy" :=
    e1 ^ e2 =? mat3
      (vec3    0. 1. 0.)
      (vec3 ~-.1. 0. 0.)
      (vec3    0. 0. 0.)

let m_one =
  "(0 1) ^ (1 0) = -1" := cross (vec2 0. 1.) (vec2 1. 0.) =? scalar (-1.)

let v4 = "concat: scalar |+| vec2 |+| scalar " :=
    scalar 0. |+| vec2 1. 2. |+| scalar 3.
    =? vec4 0. 1. 2. 3.

let v4' = "concat: vec2 |+| vec2" :=
    vec2 1. 2. |+| vec2 3. 4. =? vec4 1. 2. 3. 4.

let stretch =
  "stretch vec4'  (3. 2. |+| 1.)  ⇒ (3. 2. 1. 1.)" :=
  vec4' (vec2 3. 2. |+| scalar 1.) =? vec4 3. 2. 1. 1.
let eye2 = "eye" := eye d2 =? mat2 (vec2 1. 0.) (vec2 0. 1.)

let di1 =
  "di1= diag (2 1)" := mat2 (vec2 2. 0.) (vec2 0. 1.) =? diag (vec2 2. 1.)
let di2 = "di2= diag(1 2)" :=
  mat2 (vec2 1. 0.) (vec2 0. 2.) =? diag (vec2 1. 2.)
let di3 = "di1/di2" := di1 / di2 =? diag (vec2 2. 0.5)

let exp_eye =
  "exp id = diag(e,e)" := exp eye2 =? diag (vec2' @@ scalar const_e)

let sym = mat2 (vec2 0. 1.) (vec2 1. 0.)

let t = "( 1 2; 3 4) (0 1; 1 0)" :=
    mat2 (vec2 1. 2.) (vec2 3. 4.) * sym
    =? mat2 (vec2 2. 1.) (vec2 4. 3.)
let sym = "sym is an involution" := eye2 / sym =? sym

let mxy = mat3 (vec3 (-1.) 0. 0.) (vec3 0. (-1.) 0.) (vec3 0. 0. 1.)
let mid =
   "R_(π/24) ** 24 = -xy " := ( xyrot (pi/. 24.) ** 24 ) =? mxy
let mid =
  "R_(π/7) ** -7 = -xy" := ( xyrot (pi/. 7.) ** (-7) ) =? mxy

;;

let linear =
  let open Alea in
  let s =scalar () in
  let v = vec4 () in
  let w = vec4 () in
  let u = vec4 () in
  "s * (( u + v ) + w) = s * u + (s * v + s * w)" :=
    s * (( u + v ) + w) =? s * u + (s * v + s * w)

let mat_distribution =
  let open Alea in
  let s = scalar () in
  let m = mat4 () in
  let n = mat4 () in
  let u = vec4 () in
  let v = vec4 () in
  "s * (m + n) * (u + v) = s m u + s m v + s n u + s n v" :=
    s*(m+n)*(u+v) =? s * m * u + s * m * v + s * n * u + s * n * v

let vec_mat =
  let m,n = Alea.(mat4 (),mat4 ()) in
  let s, t = Alea.(scalar (), scalar ()) in
  let v, w = Alea.(vec4 (), vec4 ()) in
  "v M" := (v + s * w) * (m + t * n) =?
           v * m + t * v * n
           + s * w * m + s * t * w * n

let rand =
  let m1 = Alea.mat3 () in
  let m2 = Alea.mat3 () in
  let m3 = m1 * m2  in
  "rm1 * rm2 / rm2 - m1 " := m3 / m2  =? m1
(*
let error = cross (vec4 0. 0. 0. 1.) (vec4 0. 1. 0. 0.)
let error' = cross (scalar 1.) (scalar 2.)
*)


let vec_div0 =
  let m = diag (vec4 1. 2. 3. 4.) in
  let v = vec4 5. 7. 11. 13. in
  " v M / M, 1)" := (v * m) / m =? v

let vec_div2 =
  let m = mat2 (vec2 1. 0.) (vec2 1. 1.) in
  let v = vec2 31. 37. in
  " v M / M, 3)" := (v * m) / m =? v

let vec_div =
  let m = Alea.mat4 () in
  let v = Alea.vec4 () in
  let vm = v * m in
  "v M / M = v, 4)" := vm / m =? v

module X :sig val t: _ scalar end = struct
let fn v w =
  (cross v w) + scalar 1.
let t =
  "(0 1) ^ (1 0) + 1 = 0" := fn (vec2 0. 1.) (vec2 1. 0.) =? scalar 0.
end

module M: sig end = struct
  let one r =
    let s = scalar 1. in
    dim_match r
      (fun _ -> s )
      (fun _ -> vec2' s )
      (fun _ -> vec3' s )
      (fun _ -> vec4' s )

  let t = "dim matching (1 1 1)" := one d3 =? vec3 1. 1. 1.
end


let trace =
  let a, b ,c = Alea.( mat4(), mat4(), mat4 ()) in
  "trace (ABC) = trace(BCA) " :=
    scalar (trace (a * b * c)) =? scalar ( trace (b * c * a) )


let anticommutator =
  let a, b ,c = Alea.( mat4(), mat4(), mat4 ()) in
  let ( ^ ) = anticommutator in
  "[A,[B,C]] + [B,[C,A]] + [C, [A,B]] = 0 " :=
    (a^(b^c)) + (b^(c^a)) =? ((a^b)^c)
