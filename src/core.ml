open Type_functions

let unexpected ranks = raise (Interface.Unexpected_ranks ranks)

type (+'dim, +'rank) index = int

let x = 0
let y = 1
let z = 2
let t = 3
let xx = 0b10000
let xy = 0b10001
let xz = 0b10010
let xt = 0b00011
let yx = 0b10100
let yy = 0b10101
let yz = 0b10110
let yt = 0b10111
let zx = 0b11000
let zy = 0b11001
let zz = 0b11010
let zt = 0b11011
let tx = 0b11100
let ty = 0b11101
let tz = 0b11110
let tt = 0b11111

let index_rank x = x lsr 4

type (+'dim,+'rank) t = {rank:int; data: float array }

let mat_dim a = match Array.length a.data with
  | 4 -> 2
  | 9 -> 3
  | 16 -> 4
  | n -> int_of_float (sqrt (float_of_int n))

let pp ppf a = match a.rank with
  | 0 -> Format.pp_print_float ppf a.data.(0)
  | 1 ->
    Format.fprintf ppf "@[(%f" a.data.(0);
    for i=1 to (Array.length a.data -1) do
      Format.fprintf ppf "@ %f" a.data.(i)
    done;
    Format.fprintf ppf ")@]"
  | 2 ->
    let dim = mat_dim a in
    let line i =
      Format.fprintf ppf "@[| %f" a.data.(dim * i);
    for j=1 to (dim-1) do
      Format.fprintf ppf "@ %f" a.data.(dim * i + j)
    done;
    Format.fprintf ppf " |@]" in
    Format.fprintf ppf "@[<v>";
      line 0;
      for i = 1 to dim - 1 do Format.pp_print_cut ppf (); line i done;
    Format.fprintf ppf "@]"
  | n -> unexpected [n]




type +'x scalar = ('a, 'b z) t constraint 'x = 'a * 'b

type +'x vec2 = ('a two,'b one) t constraint 'x = 'a * 'b
type +'x vec3 = ('a three,'b one) t constraint 'x = 'a * 'b
type +'x vec4 = ('a four,'b one) t constraint 'x = 'a * 'b

type +'x mat2 = ('a two,'b two) t constraint 'x = 'a * 'b
type +'x mat3 = ('a three,'b two) t constraint 'x = 'a * 'b
type +'x mat4 = ('a four,'b two) t constraint 'x = 'a * 'b


let ( |+| ) {data=a;_} {data=b;_} =
  let l = Array.length a + Array.length b in
  let sep = Array.length a in
  let data = Array.make l 0. in
  for i = 0 to sep - 1 do
    data.(i) <- a.(i)
  done;
  for i = sep to l - 1 do
    data.(i) <- b.(i-sep)
  done;
  { rank = 1 ; data }

let scalar x = { rank = 0; data = [|x|] }
let vec2 x y = {rank=1; data = [|x;y|]}
let vec3 x y z = { rank=1; data = [|x;y;z|] }
let vec4 x y z t = { rank=1; data = [|x;y;z;t|] }

let mat2 {data=a;_} {data=b;_} =
  { rank = 2; data = [| a.(0); a.(1); b.(0); b.(1) |] }
let mat3 {data=a;_} {data=b;_} {data=c; _ } =
  { rank = 2; data = [| a.(0); a.(1); a.(2);
                        b.(0); b.(1); b.(2);
                        c.(0); c.(1); c.(2) |]
  }

let mat4 {data=a;_} {data=b;_} {data=c; _ } {data=d;_} =
  { rank = 2; data = [| a.(0); a.(1); a.(2); a.(3);
                        b.(0); b.(1); b.(2); b.(3);
                        c.(0); c.(1); c.(2); c.(3);
                        d.(0); d.(1); d.(2); d.(3);
                     |]
  }


let slice (t: (_,_) t) (n:(_ index)) = match t.rank with
  | 0 -> scalar t.data.(0)
  | 1 -> scalar t.data.(n land 0x3)
  | 2 ->
    let dim = mat_dim t in
    if index_rank n = 1 then
      scalar @@ t.data.( dim * ( 0x3 land n) + ((n lsr 2) land 0x3) )
    else
      { rank=1; data= Array.init dim
                    (fun i -> t.data.( i + dim * (n land 0x3) ))
      }
  | n -> unexpected [n]

let (.%[]) x = slice x

let get (t: (_,_) t) (n:(_ index)) = match t.rank with
  | 0 -> t.data.(0)
  | 1 -> t.data.(n land 0x3 )
  | 2 -> t.data.( (n lsr 2) land 0x3 + mat_dim t * (n land 0x3) )
  | n -> unexpected [n]

let (.%()) x = get x


let map f x = { x with data = Array.map f x.data }
let map2 f x y = { x with data = Array.map2 f x.data y.data }
let smap f x y = map (f x.data.(0)) y


let cross {data=a;_} {data=b;_} =
  if Array.length a = 2 then
    scalar (a.(0)*.b.(1) -. a.(1) *. b.(0))
  else
    vec3
      (a.(1) *. b.(2) -. a.(2) *. b.(1) )
      (a.(2) *. b.(0) -. a.(0) *. b.(2) )
      (a.(0) *. b.(1) -. a.(1) *. b.(0) )

let ( ^ ) {data=a;_} {data=b;_} =
  let dim = Array.length a in
  let data = Array.make (dim * dim) 0. in
  for i = 0 to dim -1 do
    for j = 0 to dim - 1 do
      let r = a.(i) *. b.(j) -. a.(j) *. b.(i) in
      data.( i * dim + j ) <- r +. data.( i + dim + j);
      data.( j * dim + i ) <- data.( j + dim + i) -. r
    done
  done;
  { rank=2; data }


let ( * ) a b = match a.rank, b.rank with
  | 0, _ -> smap ( *. ) a b
  | _, 0 -> smap ( *. ) b a
  | 1, 1 -> map2 ( *. ) a b
  | 1, 2 | 2, 1 ->
    let dim = min (Array.length a.data) (Array.length b.data) in
    let a , b, s1, s2= if a.rank = 1 then a.data, b.data, dim, 1
      else b.data, a.data, 1, dim in
    let sum i = let s = ref 0. and ij = ref (i * s1) in
      for j = 0 to dim -1 do
        s:= !s +. a.(j) *. b.(!ij);
        ij := s2 + !ij done;
      !s
    in
    { rank=1; data = Array.init dim sum }
  | 2, 2 ->
    let dim = mat_dim a in
    let a = a.data and b = b.data in
    let sum i j = let s = ref 0. in
      for k = 0 to dim - 1 do
        s:= a.(i * dim + k) *. b.(k * dim + j) +. !s done;
      !s
    in
    let data = Array.init (Array.length a)
        (fun n -> sum (n / dim) (n mod dim)) in
    { rank = 2; data }
  | x, y  -> unexpected [x;y]


let ( / ) a b =
  if a.rank = 1 && b.rank = 1 then
  map2 (/.) a b
 else
  smap (fun x y -> y /. x ) b a

let ( |*| ) a b =
  let s = ref 0. in
  for i = 0 to (Array.length a.data - 1) do
    s:= !s +. a.data.(i) *. b.data.(i)
  done;
  !s

let norm x = sqrt (x|*|x)

let (+) a b =
  if a.rank = 0 then
    smap (+.) a b
  else if b.rank = 0 then
    smap (+.) b a
  else { a with data = Array.map2 (+.) a.data b.data }

let (-) a b =
  if a.rank = 0 then
    smap (-.) a b
  else if b.rank = 0 then
    { a with
      data = Array.init (Array.length b.data)
          (fun n -> b.data.(n) -. a.data.(0))}
  else { a with data = Array.map2 (+.) a.data b.data }


let floor a = int_of_float ( a.data.(0) )
