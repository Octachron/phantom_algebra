open Type_functions

let unexpected ranks = raise (Interface.Unexpected_ranks ranks)

type (+'input_dim,+'output_dim,+'rank,+'group) index = int


let ilen x = 0x3F land (x lsr 18)
let shift x = 0xFF land (x lsr 16)
let irank x = x lsr 24
let t3 x = x, x, x
let x', r', s' = t3 0x1040000
let y', g', t' = t3 0x1040001
let z', b', p' = t3 0x1040002
let w', a', q' = t3 0x1040003


let xx', rr', ss' = t3 0x2040000
let xy', rg', st' = t3 0x2040001
let xz', rb', sp' = t3 0x2040002
let xw', ra', sq' = t3 0x2040003
let yx', gr', ts' = t3 0x2040004
let yy', gg', tt' = t3 0x2040005
let yz', gb', tp' = t3 0x2040006
let yw', ga', tq' = t3 0x2040007
let zx', br', ps' = t3 0x2040008
let zy', bg', pt' = t3 0x2040009
let zz', bb', pp' = t3 0x204000A
let zw', ba', pq' = t3 0x204000B
let wx', ar', qs' = t3 0x204000C
let wy', ag', qt' = t3 0x204000D
let wz', ab', qp' = t3 0x204000E
let ww', aa', qq' = t3 0x204000F


let dim_mask = 0xFF0000

let (&) x y =
  let nx = shift x in
  x + ((y land 0xFFFF) lsl nx) + y land dim_mask

type (+'dim,+'rank) t = {rank:int; data: float array }

let mat_dim a = match Array.length a.data with
  | 4 -> 2
  | 9 -> 3
  | 16 -> 4
  | n -> int_of_float (sqrt (float_of_int n))

let pp ppf a = match a.rank with
  | 0 -> Format.pp_print_float ppf a.data.(0)
  | 1 ->
    Format.fprintf ppf "@[(%g" a.data.(0);
    for i=1 to (Array.length a.data -1) do
      Format.fprintf ppf "@ %g" a.data.(i)
    done;
    Format.fprintf ppf ")@]"
  | 2 ->
    let dim = mat_dim a in
    let line i =
      Format.fprintf ppf "@[|% g" a.data.(dim * i);
    for j=1 to (dim-1) do
      Format.fprintf ppf "@,% g" a.data.(dim * i + j)
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

let vec2' a =
  if a.rank = 0 then
    { rank = 1; data = Array.make 2 a.data.(0)}
  else
    { rank=1; data= Array.copy a.data }

let vec_stretch k a =
  if a.rank = 0 then
    { rank = 1; data = Array.make 2 a.data.(0)}
  else
    let len = Array.length a.data in
    let data = Array.make k a.data.(len-1) in
    Array.blit a.data 0 data 0 len;
    { rank=1; data }

let vec3' x = vec_stretch 3 x
let vec4' x = vec_stretch 4 x



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



let swizzle v index =
  let size = ilen index in
  Array.init size
    (fun i ->
       let pos = i lsl 2 in
       let index' = index lsr pos in
       let masked = 0xF land index' in
       v.(masked)
    )

let slice (t: (_,_) t) (n:(_ index)) =
  match t.rank with
  | 0 -> scalar t.data.(0)
  | 1 ->
    if ilen n = 1 then
      scalar t.data.(n land 0xF)
    else { rank=1; data = swizzle t.data n }
  | 2 ->
    let dim = mat_dim t in
    let len = ilen n in
    if irank n = 2 then
      if len = 1 then
        scalar @@ t.data.( dim * ( 0x3 land n) + ((n lsr 2) land 0x3) )
      else
        { rank=1; data = Array.init len (fun i ->
              let s = n lsr (i lsl 3) in
              t.data.( dim * (0x3 land s) + (((n lsr 2) land 0x3) )))
        }
    else if len = 1 then
      { rank=1; data= Array.init dim
                    (fun i -> t.data.( i + dim * (n land 0x3) ))
      }
    else
      let data = Array.make (dim * dim) 0. in
      for i = 0 to dim - 1 do
        let s = 0x3 land ( n lsr (i lsl 3) ) in
        for j = 0 to dim - 1 do
          data.(i * dim + j) <- t.data.( s * dim + j)
        done;
      done;
      { rank=2; data}
  | n -> unexpected [n]


let get (t: (_,_) t) (n:(_ index)) = match t.rank with
  | 0 -> t.data.(0)
  | 1 -> t.data.(n land 0x3 )
  | 2 -> t.data.( (n lsr 2) land 0x3 + mat_dim t * (n land 0x3) )
  | n -> unexpected [n]

#if OCAML_MAJOR>=4 && OCAML_MINOR>=6
let (.%()) x = get x
let (.%[]) x = slice x
#endif

let amap2=
#if OCAML_MAJOR>=4 && OCAML_MINOR>=3
  Array.map2
#else
  fun f x y -> Array.init
      (min (Array.length x) (Array.length y))
      (fun i -> f x.(i) y.(i))
#endif
let map f x = { x with data = Array.map f x.data }
let map2 f x y = { x with data = amap2 f x.data y.data }
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
  else map2 (+.) a b

let (-) a b =
  if a.rank = 0 then
    smap (-.) a b
  else if b.rank = 0 then
    { a with
      data = Array.init (Array.length b.data)
          (fun n -> b.data.(n) -. a.data.(0))}
  else map2 (-.) a b


let floor a = int_of_float ( a.data.(0) )
