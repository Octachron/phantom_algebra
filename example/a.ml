open Phantom_algebra.Core

let v = vec2 0. 1.
let m = mat2 (vec2 1. 0.) (scalar 2. * v)
let m2 = m * m
let z = v * m
let w = vec3 0. 0. 1.
let s = scalar 7.
let w = v + vec2 1. 1.
let v' = s + v
let v'' = s * v

let v''' = v' * ( scalar 1. + v'')

let xyrot theta = mat3
    (vec3 (cos theta) (sin theta) 0.)
    (vec3 (~-.(sin theta)) (cos theta) 0.)
    (vec3 0. 0. 1.)


let pi = 4. *. atan 1.
let r = xyrot (pi /.6.)

let e1 = vec3 1. 0. 0.
let e2 = xyrot (pi /. 2.) * e1

let f = v.%[x]
let mxy = r.%[yy]

let e3 = cross e1 e2
let f2 = e1 ^ e2

let m_one = cross (vec2 0. 1.) (vec2 1. 0.)

let v4 = scalar 0. |+| vec2 1. 2. |+| scalar 3.
let v4' = vec2 1. 2. |+| vec2 3. 4.

(*
let error = cross (vec4 0. 0. 0. 1.) (vec4 0. 1. 0. 0.)
let error' = cross (scalar 1.) (scalar 2.)
*)

module X :sig val t: _ scalar end = struct
let fn v w =
  (cross v w) + scalar 1.
let t = fn (vec2 0. 1.) (vec2 1. 0.)
end

let () =
Format.eprintf
  "@[<v>v=%a@,m=%a@,m2=%a@,z=%a@,w=%a@,s=%a@,v'=%a@,v''=%a@,v'''=%a@,\
   r=%a@,-xy?=%a@,e1=%a@,e2=%a@,f=%a@,mxy=%a@,e3=%a@,f2=%a@,-1=%a@,v4=%a@]@."
  pp v
  pp m
  pp m2
  pp z
  pp w
  pp s
  pp v'
  pp v''
  pp v'''
  pp r
  pp (r * r * r * r * r * r)
  pp e1
  pp e2
  pp f
  pp mxy
  pp e3
  pp f2
  pp m_one
  pp v4
