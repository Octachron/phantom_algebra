(** Strongly typed linear algebra on small tensors à la GLSL *)

(** { 1 Helper type definitions}
    Skim through this section when reading the documentation
    for first (second, third and …) time
*)
type k = float

type 'a one = [`one of 'a]
type 'a z = [`zero of 'a]
type 'a two = [`two of 'a]
type 'a three = [`three of 'a]
type 'a four = [`four of 'a]

type ('a,'b,'c) any =
  [< `zero of 'b & 'a | `one of 'b & 'a | `two of 'b & 'a] as 'c

type ('a, 'b,'c, 'parameters) product =
  [<`zero of 'b & (* scalar broadcasting *)
             [< `zero of 'c & 'p1 z
             | `one of 'c & 'p1 one
             | `two of 'c & 'p1 two]
  | `one of 'b &
            [< `zero of 'c & 'p1 one
            | `one of 'c & 'p1 one
            | `two of 'c & 'p1 one]
  | `two of 'b &
            [< `zero of 'c & 'p1 two
            | `one of 'c & 'p1 one
            | `two of 'c & 'p1 two]
  ] as 'a
  constraint 'parameters = 'p1 * 'p2 * 'p3
(** (x,y,z,_ ) product computes the rank of x * y and
    put the result inside z *)

type ('a, 'b,'c, 'parameters) div =
  [<`zero of 'b & (* scalar broadcasting *)
             [< `zero of 'c & 'p1 z]
  | `one of 'b &
            [< `zero of 'c & 'p1 one
            | `one of 'c & 'p1 one]
  | `two of 'b &
            [< `zero of 'c & 'p1 two]
  ] as 'a
  constraint 'parameters = 'p1 * 'p2 * 'p3
(** (x,y,z,_ ) div computes the rank of x * y and
    put the result inside z *)

type ('a, 'b,'c, 'parameters) rank_diff =
  [<
    | `one of 'b & [< `one of 'c & 'p1 z]
    | `two of 'b &
              [< `one of 'c & 'p1 one
              | `two of 'c & 'p1 z ]
  ] as 'a
  constraint 'parameters = 'p1
(** (x,y,z,_ ) diff computes the rank of x - y and
    put the result inside z *)


type ('a, 'b,'c, 'parameters) sum =
  [<`zero of 'b & (* scalar broadcasting *)
             [< `zero of 'c & 'p1 z
             | `one of 'c & 'p1 one
             | `two of 'c & 'p1 two]
  | `one of 'b &
            [< `zero of 'c & 'p1 one
            | `one of 'c & 'p1 one ]
  | `two of 'b &
            [< `zero of 'c & 'p1 two
            | `two of 'c & 'p1 two]
  ] as 'a
  constraint 'parameters = 'p1 * 'p2 * 'p3
(** (x,y,z,_ ) sum computes the rank of x + y and
    put the result inside z *)


type ( 'dim, 'res, 'parameters ) cross =
  [< `two of 'res & ('p2 * 'p1 z) | `three of 'res & ('p2 three * 'p1 one) ]
  as 'dim
  constraint 'parameters = 'p1 * 'p2

(** { 1 Index data type } *)

type (+'dim,+'rank) index

val x: ([< _ one| _ two| _ three| _ four], [ | _ one]) index
val y: ([< _ two| _ three| _ four], [ | _ one]) index
val z: ([< _ three| _ four], [ | _ one]) index
val t: ([< _ four], [ | _ one]) index

val xx: ([< _ one|_ two|_ three|_ four], [ | _ two]) index
val yx: ([< _ two|_ three|`four], [ | _ two]) index
val zx: ([< _ three|`four], [ | _ two]) index
val tx: ([< _ four], [ | _ two]) index

val xy: ([< _ two|_ three|_ four], [ | _ two]) index
val yy: ([< _ two|_ three|_ four], [ | _ two]) index
val zy: ([< _ three|_ four], [ | _ two]) index
val ty: ([< _ four], [ | _ two]) index
val xz: ([<_ three|_ four], [ | _ two]) index
val yz: ([< _ three|_ four], [ | _ two]) index
val zz: ([< _ three|_ four], [ | _ two]) index
val tt: ([< _ four], [ | _ two]) index

val xt: ([ | _ four], [ | _ two]) index
val yt: ([ | _ four], [ | _ two]) index
val zt: ([ | _ four], [ | _ two]) index
val tt: ([ | _ four], [ | _ two]) index

(** Tensor type *)
type (+'dim,+'rank) t

val pp: Format.formatter -> ('dim,'rank) t -> unit
type +'x scalar = ('a, 'b z) t constraint 'x = 'a * 'b

type +'x vec2 = ('a two,'b one) t constraint 'x = 'a * 'b
type +'x vec3 = ('a three,'b one) t constraint 'x = 'a * 'b
type +'x vec4 = ('a four,'b one) t constraint 'x = 'a * 'b

type +'x mat2 = ('a two,'b two) t constraint 'x = 'a * 'b
type +'x mat3 = ('a three,'b two) t constraint 'x = 'a * 'b
type +'x mat4 = ('a four,'b two) t constraint 'x = 'a * 'b

val scalar: k -> _ scalar

val vec2: k -> k -> _ vec2
val vec3: k -> k -> k -> _ vec3
val vec4: k -> k -> k -> k -> _ vec4

val mat2: _ vec2 -> _ vec2 -> _ mat2
val mat3: _ vec3 -> _ vec3 -> _ vec3 -> _ mat3
val mat4: _ vec4 -> _ vec4 -> _ vec4 -> _ vec4 -> _ mat4


val map: (k -> k ) -> ('dim,'rank) t -> ('dim,'rank) t
val map2: (k -> k -> k ) -> ('dim,'rank) t -> ('dim,'rank) t -> ('dim,'rank) t


(** [x + y] is the standard vector sum, except for scalar argument
    which are broadcasted to a constant tensor *)
val (+): ('a,('rank1,'rank2,'rank3,_) sum ) t
  -> ('a,'rank2) t -> ('a,'rank3) t

(** [x - y] is the standard vector difference, except for scalar argument
    which are broadcasted to a constant tensor *)
val (-): ('a,('rank1,'rank2,'rank3,_) sum ) t
  -> ('a,'rank2) t -> ('a,'rank3) t

(** [ x * y] is:
    - the external product if x or y is a scalar
    - the matrix product if x or y is a matrix
    - the element-wise (hadamard) product otherwise
*)
val ( * ) : ('dim, ('rank1, 'rank2, 'rank3, _ ) product) t
  -> ('dim, 'rank2) t ->
  ('dim,'rank3) t

(** [ x / y] is:
    - the external product if x or y is a scalar or a matrix
    - the element-wise division if both x and y are a vector
*)
val ( / ) : ('dim, ('rank1, 'rank2, 'rank3, _ ) div) t
  -> ('dim, 'rank2) t ->
  ('dim,'rank3) t

(** [ (x|*|y)] is the canonical scalar product
*)
val ( |*| ) : ('dim, 'rank) t -> ('dim,'rank) t -> k

(** [norm x] is the canonical norm of x *)
val norm:  ('dim, 'rank) t -> k

(** See {!cross} for the 2d and 3d cross-product for vectors
   [ v ^ w ] is the matrix representation of the 2-form, i.e.
    an infinitesimal rotation in the plane generated by [v] and [w]
*)
val ( ^ ): ('dim, _ one) t -> ('dim, _ one ) t -> ('dim, _ two ) t

(** [cross v w] maps either two 3d vectors to
    a 3d pseudo-vector, or two 2d vectors to a scalar *)
val cross:  ( ('dim, 'dim2 * 'rank2, _ ) cross , _ one) t ->
  ('dim, _ one) t -> ('dim2, 'rank2) t

val floor: _ scalar -> int

(** [slice t n] or [ t.%[n] ] computes a slice of rank
    [tensor_rank - index_rank], in other words for a vector [v]
    and a matrix [m], [v.%[x]] and [m.%[xx]] are a scalar, whereas
    [m.%[x]] is the first row vector of the matrix [m] *)
val slice: ('dim,('rank1,'rank2,'rank3,_) rank_diff ) t ->
  ('dim,'rank2) index -> ('dim,'rank3) t
val (.%[]) : ('dim,('rank1,'rank2,'rank3,_) rank_diff ) t ->
  ('dim,'rank2) index -> ('dim,'rank3) t

(** [t.%(x)] returns the value of the tensor at index [x] *)
val get: ('dim,'rank) t -> ('dim,'rank) index -> k
val (.%()): ('dim,'rank) t -> ('dim,'rank) index -> k
