open Type_functions
type k = float
exception Unexpected_ranks of int list

(** { 1 Helper type definitions}
    Skim through this section when reading the documentation
    for first (second, third and …) time
*)

(** { 1 Index data type } *)
module type Index = sig

  type (+'dim,+'len,+'rank,+'group) index


  (** Index concatenation *)
  val (&): ( 'dim, ('len1,'len2,'len3,_) simple_sum, 'rank, 'group) index
    -> ('dim,'len2,'rank,'group) index
    -> ('dim,'len3,'rank,'group) index


  (** {1 XYZW group} *)
  val x': ([< _ one| _ two| _ three| _ four], _ one, _ one , [`xyzw]) index
  val y': ([< _ two| _ three| _ four], _ one, _ one , [`xyzw]) index
  val z': ([< _ three| _ four], _ one, _ one , [`xyzw]) index
  val w': ([< _ four], _ one, _ one , [`xyzw]) index

  val xx': ([< _ one|_ two|_ three|_ four], _ one, _ two, [`xyzw]) index
  val yx': ([< _ two|_ three|`four], _ one, _ two, [`xyzw]) index
  val zx': ([< _ three|`four], _ one, _ two, [`xyzw]) index
  val wx': ([< _ four], _ one, _ two, [`xyzw]) index
  val xy': ([< _ two|_ three|_ four], _ one, _ two, [`xyzw]) index
  val yy': ([< _ two|_ three|_ four], _ one, _ two, [`xyzw]) index
  val zy': ([< _ three|_ four], _ one, _ two, [`xyzw]) index
  val wy': ([< _ four], _ one, _ two, [`xyzw]) index
  val xz': ([<_ three|_ four], _ one, _ two, [`xyzw]) index
  val yz': ([< _ three|_ four], _ one, _ two, [`xyzw]) index
  val zz': ([< _ three|_ four], _ one, _ two, [`xyzw]) index
  val wz': ([< _ four], _ one, _ two, [`xyzw]) index
  val xw': ([ | _ four], _ one, _ two, [`xyzw]) index
  val yw': ([ | _ four], _ one, _ two, [`xyzw]) index
  val zw': ([ | _ four], _ one, _ two, [`xyzw]) index
  val ww': ([ | _ four], _ one, _ two, [`xyzw]) index


  (** {1 RGBA} *)
  val r': ([< _ one| _ two| _ three| _ four], _ one, _ one , [`rgba]) index
  val g': ([< _ two| _ three| _ four], _ one, _ one , [`rgba]) index
  val b': ([< _ three| _ four], _ one, _ one , [`rgba]) index
  val a': ([< _ four], _ one, _ one , [`rgba]) index

  val rr': ([< _ one|_ two|_ three|_ four], _ one, _ two, [`rgba]) index
  val gr': ([< _ two|_ three|`four], _ one, _ two, [`rgba]) index
  val br': ([< _ three|`four], _ one, _ two, [`rgba]) index
  val ar': ([< _ four], _ one, _ two, [`rgba]) index
  val rg': ([< _ two|_ three|_ four], _ one, _ two, [`rgba]) index
  val gg': ([< _ two|_ three|_ four], _ one, _ two, [`rgba]) index
  val bg': ([< _ three|_ four], _ one, _ two, [`rgba]) index
  val ag': ([< _ four], _ one, _ two, [`rgba]) index
  val rb': ([<_ three|_ four], _ one, _ two, [`rgba]) index
  val gb': ([< _ three|_ four], _ one, _ two, [`rgba]) index
  val bb': ([< _ three|_ four], _ one, _ two, [`rgba]) index
  val ab': ([< _ four], _ one, _ two, [`rgba]) index
  val ra': ([ | _ four], _ one, _ two, [`rgba]) index
  val ga': ([ | _ four], _ one, _ two, [`rgba]) index
  val ba': ([ | _ four], _ one, _ two, [`rgba]) index
  val aa': ([ | _ four], _ one, _ two, [`rgba]) index

    (** {1 STPQ group} *)
  val s': ([< _ one| _ two| _ three| _ four], _ one, _ one , [`stpq]) index
  val t': ([< _ two| _ three| _ four], _ one, _ one , [`stpq]) index
  val p': ([< _ three| _ four], _ one, _ one , [`stpq]) index
  val q': ([< _ four], _ one, _ one , [`stpq]) index

  val ss': ([< _ one|_ two|_ three|_ four], _ one, _ two, [`stpq]) index
  val ts': ([< _ two|_ three|`four], _ one, _ two, [`stpq]) index
  val ps': ([< _ three|`four], _ one, _ two, [`stpq]) index
  val qs': ([< _ four], _ one, _ two, [`stpq]) index
  val st': ([< _ two|_ three|_ four], _ one, _ two, [`stpq]) index
  val tt': ([< _ two|_ three|_ four], _ one, _ two, [`stpq]) index
  val pt': ([< _ three|_ four], _ one, _ two, [`stpq]) index
  val qt': ([< _ four], _ one, _ two, [`stpq]) index
  val sp': ([<_ three|_ four], _ one, _ two, [`stpq]) index
  val tp': ([< _ three|_ four], _ one, _ two, [`stpq]) index
  val pp': ([< _ three|_ four], _ one, _ two, [`stpq]) index
  val qp': ([< _ four], _ one, _ two, [`stpq]) index
  val sq': ([ | _ four], _ one, _ two, [`stpq]) index
  val tq': ([ | _ four], _ one, _ two, [`stpq]) index
  val pq': ([ | _ four], _ one, _ two, [`stpq]) index
  val qq': ([ | _ four], _ one, _ two, [`stpq]) index
end

module type Core = sig
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

  (** [vec$n' v] extends a vector of dimension [d <= n]
      to a vector of dimension v by repeting the last value
      of the vector *)
  val vec2': (_ two, [< _ one | _ z] ) t -> _ vec2
  val vec3': ([< _ two| _ three] , [< _ one | _ z] ) t -> _ vec4
  val vec4': ([< _ two| _ three | _ four ] , [< _ one | _ z] ) t -> _ vec3

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
      - the external product if x or y is a scalar
      - the matrix division if x and y are a matrix
      - the element-wise division if both x and y are a vector
  *)
  val ( / ) : ('dim, ('rank1, 'rank2, 'rank3, _ ) div) t
    -> ('dim, 'rank2) t ->
    ('dim,'rank3) t

  (** [ t ** k] is [ t * … * t ] k-time *)
  val ( ** ) : ('dim,'rank) t -> int -> ('dim,'rank) t

  (** [ (x|*|y)] is the canonical scalar product
  *)
  val ( |*| ) : ('dim, 'rank) t -> ('dim,'rank) t -> k

  (** [norm x] is the canonical norm of x *)
  val norm:  ('dim, 'rank) t -> k

  (** [norm_1 x] is ∑ |x_i| *)
  val norm_1:  ('dim, 'rank) t -> k

  (** [norm_q q x] is (∑ |x_i|^q) ^ 1/q *)
  val norm_q: float -> ('dim, 'rank) t -> k

  (** See {!cross} for the 2d and 3d cross-product for vectors
      [ v ^ w ] is the infinitesimal rotation matrix in the plane
      generated by [v] and [w] with an amplitude [|v||w| sin θ ].
      In other words the matrix representation of the 2-from [dv ^ dw] in
      the corresponding graded algebra.
  *)
  val ( ^ ): ('dim, _ one) t -> ('dim, _ one ) t -> ('dim, _ two ) t

  (** [cross v w] maps either two 3d vectors to
      a 3d pseudo-vector, or two 2d vectors to a scalar *)
  val cross:  ( ('dim, 'dim2 * 'rank2, _ ) cross , _ one) t ->
    ('dim, _ one) t -> ('dim2, 'rank2) t

  (** Vector concatenation : [ v |+| w ] *)
  val ( |+| ): ('dim1, ('rank1,'rank2,'dim1,'dim2,'dim3,_) nat_sum) t ->
    ('dim2, 'rank2) t -> ('dim3, _ one) t

  val floor: _ scalar -> int
end

module type Indexing = sig

  type (+'dim,+'len,'rank,'group) index
  type (+'dim,+'rank) t

  (** [slice t n] or [ t.%[n] ] computes a slice of rank
      [tensor_rank - index_rank], in other words for a vector [v]
      and a matrix [m], [v.%[x]] and [m.%[xx]] are a scalar, whereas
      [m.%[x]] is the first row vector of the matrix [m] *)
  val slice: ('dim1,('rank1,'rank2,'rank3, 'dim1,'dim3,'len,_) superindexing) t
    -> ('dim1, 'len, 'rank2, 'group) index -> ('dim3,'rank3) t

#if OCAML_MAJOR>=4 && OCAML_MINOR>=6
  val (.%[]): ('dim1,('rank1,'rank2,'rank3, 'dim1,'dim3,'len,_) superindexing) t
    -> ('dim1, 'len, 'rank2, 'group) index -> ('dim3,'rank3) t
#endif
  (** [t.%(x)] returns the value of the tensor at index [x] *)
  val get: ('dim,'rank) t -> ('dim,_ one, 'rank, 'group) index -> k

#if OCAML_MAJOR>=4 && OCAML_MINOR>=6
  val (.%()): ('dim,'rank) t -> ('dim, _ one,'rank,'group) index -> k
#endif
end

module type S = sig
  include Core
  include Index
  include Indexing with
    type ('a,'b,'c,'d) index := ('a,'b,'c,'d) index
    and type ('a,'b) t := ('a,'b) t

end
