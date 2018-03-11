let const_e = exp 1.

open Phantom_algebra.Core
open Phantom_algebra.Type_functions

let negligible x = x < 1e-12


let failure = ref false
let test x expected name =(*
  let line ppf =
    Format.fprintf ppf
      "——————————————————————————————————————————————————————————————————————@,"
  in*)
  let x, out = clone_2 x in
  begin match expected with
  | None ->
    Format.printf
      "@[<hv>  \x1b[94m%s:\x1b[97m@ @;<0 8>@[%a@]@,@]@." name pp x
  | Some y -> if negligible @@ norm(x - y) then
      Format.printf "@[  \x1b[94m%s\x1b[97m: \x1b[32m[✔]\x1b[97m@]@." name
    else begin
      failure := true;
      Format.printf
        "@[<hv 8>  \x1b[94m%s\x1b[97m:\x1b[31m[✘]\x1b[97m@ @ \
         got:@,%a@,    ≠    @,expected:@,%a@,@]@."
        name pp x pp y end
  end
  ; out

let ( =? ) x expected name = test x (Some expected) name
let (:=) x f = f x
let ( |? ) x name = test x None name

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
let eye = "eye" := eye d2 =? mat2 (vec2 1. 0.) (vec2 0. 1.)

let di1 =
  "di1= diag (2 1)" := mat2 (vec2 2. 0.) (vec2 0. 1.) =? diag (vec2 2. 1.)
let di2 = "di2= diag(1 2)" :=
  mat2 (vec2 1. 0.) (vec2 0. 2.) =? diag (vec2 1. 2.)
let di3 = "di1/di2" := di1 / di2 =? diag (vec2 2. 0.5)

let exp_eye =
  "exp id = diag(e,e)" := exp eye =? diag (vec2' @@ scalar const_e)

let sym = mat2 (vec2 0. 1.) (vec2 1. 0.)

let t = "( 1 2; 3 4) (0 1; 1 0)" :=
    mat2 (vec2 1. 2.) (vec2 3. 4.) * sym
    =? mat2 (vec2 2. 1.) (vec2 4. 3.)
let sym = "sym is an involution" := eye / sym =? sym

let mxy = mat3 (vec3 (-1.) 0. 0.) (vec3 0. (-1.) 0.) (vec3 0. 0. 1.)
let mid =
   "R_(π/24) ** 24 = -xy " := ( xyrot (pi/. 24.) ** 24 ) =? mxy
let mid =
  "R_(π/7) ** -7 = -xy" := ( xyrot (pi/. 7.) ** (-7) ) =? mxy

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

let linear =
  let open Random in
  let s =scalar () in
  let v = vec4 () in
  let w = vec4 () in
  let u = vec4 () in
  "s * (( u + v ) + w) = s * u + (s * v + s * w)" :=
    s * (( u + v ) + w) =? s * u + (s * v + s * w)

let mat_distribution =
  let open Random in
  let s = scalar () in
  let m = mat4 () in
  let n = mat4 () in
  let u = vec4 () in
  let v = vec4 () in
  "s * (m + n) * (u + v) = s m u + s m v + s n u + s n v" :=
    s*(m+n)*(u+v) =? s * m * u + s * m * v + s * n * u + s * n * v

let rand =
  let m1 = Random.mat3 () in
  let m2 = Random.mat3 () in
  let m3 = m1 * m2  in
  "rm1 * rm2 / rm2 - m1 " := m3 / m2  =? m1
(*
let error = cross (vec4 0. 0. 0. 1.) (vec4 0. 1. 0. 0.)
let error' = cross (scalar 1.) (scalar 2.)
*)

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

;;
#if OCAML_MAJOR>=4 && OCAML_MINOR>=6
let f = "v_x" :=  v.%[x'] =? scalar 0.
let ryy = "r_yy" := r.%[yy'] =? scalar (sqrt 3. /. 2.)
let rx = "r_x" := r.%[x'] =? vec3 (sqrt 3. /. 2.) 0.5 0.

let sel = w'&z'&y'&x'
let sw = "v4_3210" := v4 .%[sel] =? vec4 3. 2. 1. 0.

let msw = "Id[1,0] = xy-sym" := eye.%[y'&x'] =? sym

let diag = "Id(xx,xy,yy)" := eye.%[xx'&xy'&yy'] =? vec3 1. 0. 1.
#endif

;; if !failure then
  Format.printf "\x1b[91m⛅⛅⛅⛅⛅⛅⛅⛅⛅FAILURE⛅⛅⛅⛅⛅⛅⛅⛅⛅\x1b[97m@."
else Format.printf
    "\x1b[33m☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼\x1b[92mSuccess\x1b[33m☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼\x1b[97m@."
