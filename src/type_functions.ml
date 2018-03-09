(** Type-level functions

    Skim through this section when reading the documentation
    for the first (second, third and …) time
*)

type 'a one = [`one of 'a]
type 'a z = [`zero of 'a]
type 'a two = [`two of 'a]
type 'a three = [`three of 'a]
type 'a four = [`four of 'a]

type ('a,'b,'c) any =
  [< `zero of 'b & 'a | `one of 'b & 'a | `two of 'b & 'a] as 'c

type ('a, 'b,'c,'dim1,'dim2,'dim3, 'parameters) product =
  [<`zero of 'b & (* scalar broadcasting *)
             [< `zero of 'c * 'dim3 & 'p1 z * 'p2 one
             | `one of 'c * 'dim3 & 'p1 one * 'dim2
             | `two of 'c * 'dim3 & 'p1 two * 'dim2 ]
  | `one of 'b &
            [< `zero of 'c * 'dim3 & 'p1 one * 'dim2
            | `one of 'c * 'dim2 * 'dim3 & 'p1 one * 'dim1 * 'dim1
            | `two of 'c * 'dim2 * 'dim3 & 'p1 one * 'dim1 * 'dim1 ]
  | `two of 'b &
            [< `zero of 'c * 'dim3 & 'p1 two * 'dim2
            | `one of 'c * 'dim2 * 'dim3 & 'p1 one * 'dim1 * 'dim1
            | `two of 'c * 'dim2 * 'dim3 & 'p1 two * 'dim1 * 'dim1 ]
  ] as 'a
  constraint 'parameters = 'p1 * 'p2 * 'p3
(** (x,y,z,_ ) product computes the rank of x * y and
    put the result inside z *)

type ('a, 'b,'c,'dim1,'dim2,'dim3, 'parameters) div =
  [<`zero of 'b & (* scalar broadcasting *)
             [< `zero of 'c * 'dim3  & 'p1 z * 'p2 one]
  | `one of 'b &
            [< `zero of 'c * 'dim3 & 'p1 one * 'dim1
            | `one of 'c * 'dim2 * 'dim3 & 'p1 one * 'dim1 * 'dim1]
  | `two of 'b &
            [< `zero of 'c * 'dim3 & 'p1 two * 'dim1
            | `two of 'c * 'dim2 * 'dim3 & 'p2 two * 'dim1 * 'dim1
            ]
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


type ('a, 'b,'c,'dim1,'dim2,'dim3, 'parameters) sum =
  [<`zero of 'b & (* scalar broadcasting *)
             [< `zero of 'c * 'dim3 & 'p1 z * 'p2 one
             | `one of 'c * 'dim3 & 'p1 one * 'dim2
             | `two of 'c * 'dim3 & 'p1 two * 'dim2]
  | `one of 'b &
            [< `zero of 'c * 'dim3 & 'p1 one * 'dim1
            | `one of 'c * 'dim1 * 'dim3 & 'p1 one * 'dim2 * 'dim2 ]
  | `two of 'b &
            [< `zero of 'c * 'dim3 & 'p1 two * 'dim1
            | `two of 'c * 'dim1 * 'dim3 & 'p1 two * 'dim1 * 'dim3 ]
  ] as 'a
  constraint 'parameters = 'p1 * 'p2 * 'p3
(** (x,y,z,_ ) sum computes the rank of x + y and
    put the result inside z *)


type ( 'dim, 'res, 'parameters ) cross =
  [< `two of 'res & ('p2 * 'p1 z) | `three of 'res & ('p2 three * 'p1 one) ]
  as 'dim
  constraint 'parameters = 'p1 * 'p2


type ('dim1,'dim2,'dim3,'p) simple_sum =
    [< `one of 'dim2 &
               [< `one of 'dim3 & 'p two
               | `two of 'dim3 & 'p three
               | `three of 'dim3 & 'p four
               ]
    | `two of 'dim2 &
              [< `one of 'dim3 & 'p three
              | `two of 'dim3 & 'p four
              ]
    | `three of 'dim2 & [< `one of 'dim3 & 'p four]
    ] as 'dim1

(* dim1 + dim2 = dim3 *)
type ('dim1,'dim2,'dim3,'p) nat_sum =
  [< `one of 'dim2 &
             [< `one of 'dim3 & 'p two
             | `two of 'dim3 & 'p three
             | `three of 'dim3 & 'p four ]
  | `two of 'dim2 &
             [< `one of 'dim3 & 'p three
             | `two of 'dim3 & 'p four ]
  | `three of 'dim2 &
              [< `one of 'dim3 & 'p four ]
  ]
  as 'dim1

(* (dim + rank) (dim + len + rank ) + (dim,rank) *)
type ('tensor_rank,'index_rank,'res_rank,'dim,'res_dim, 'len, 'parameters)
    superindexing =
  [< `two of
        'index_rank &
        [< `two of (* m.{xx&xy} *)
             'len &
             [< `one of 'res_rank & 'p z
             | `two of 'res_dim * 'res_rank & 's two * 'p one
             | `three of 'res_dim * 'res_rank & 's three * 'p one
             | `four of 'res_dim * 'res_rank & 's four * 'p one
             ]
        | `one of (* m.{x} or m.{x&…&x_k} with k = dim m ⇒ dim = dim m *)
             'dim * 'len &
             'res_dim *
             [< `one of 'res_rank & 'p one (*m.{x} ⇒ rank=1, dim=dim m)*)
               (* vv  m.{x&…xy} ⇒ rank=2, dim = dim m vv*)
             | `two of 'dim * 'res_rank  & 'p two * 's two
             | `three of 'dim * 'res_rank  & 'p three * 's two
             | `four of 'dim * 'res_rank  & 'p four * 's two
             ]
        ]
    | `one of 'len &
              [< `one of 'res_rank & 'p z (* v.{x}: scalar *)
              | `two of 'res_rank * 'res_dim & 'p one * 's two
              | `three of 'res_rank * 'res_dim & 'p one * 's three
              | `four of 'res_rank * 'res_dim & 'p one * 's four
              ]
        ] as 'tensor_rank
      constraint 'parameters = 'p * 's
