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

type (+'dim,+'rank) t = floatarray

module A = struct
  let len = Array.Floatarray.length
  let get = Array.Floatarray.unsafe_get
  let set = Array.Floatarray.unsafe_set

  let (#.) = get
  let iteri f x =
    for i = 0 to len x - 1 do
      f i x#.i
    done

  let copy x =
    let len = len x in
    let a = Array.Floatarray.create len in
      iteri (set a) x;
      a
  let create = Array.Floatarray.create
  let make n x =
    let a = create n in
    iteri (fun i _ -> set a i x) a;
    a
  let from_array x =
    let len = Array.length x in
    let a = create len in
    Array.iteri (set a) x;
    a

  let init n f = let a = Array.Floatarray.create n in
    for i = 0 to n - 1 do set a i (f i) done;
    a

  let map f x = init (len x) (fun i -> f x#.i)
end


let dim a = match A.len a with
  | 0 -> 0
  | 1 -> 1
  | 2 -> 2
  | 3 -> 3
  | 4 -> 4
  | 5 -> 2
  | 9 -> 3
  | 16 -> 4
  | _ -> assert false

let rank a = match A.len a with
  | 0 -> 0
  | 1 -> 0
  | 2 -> 1
  | 3 -> 1
  | 4 -> 1
  | 5 -> 2
  | 9 -> 2
  | 16 -> 2
  | _ -> assert false

let (#.) = A.(#.)

let mat_dim a = match A.len a with
  | 5 -> 2
  | 9 -> 3
  | 16 -> 4
  | n -> int_of_float (sqrt (float_of_int n))

let pp ppf a = match rank a with
  | 0 -> Format.pp_print_float ppf a#.0
  | 1 ->
    Format.fprintf ppf "@[(%g" a#.0;
    for i=1 to (A.len a -1) do
      Format.fprintf ppf "@ %g" a#.i
    done;
    Format.fprintf ppf ")@]"
  | 2 ->
    let dim = mat_dim a in
    let line i =
      Format.fprintf ppf "@[|% g" a#.(dim * i);
    for j=1 to (dim-1) do
      Format.fprintf ppf "@,% g" a#.(dim * i + j)
    done;
    Format.fprintf ppf " |@]" in
    Format.fprintf ppf "@[<v>";
      line 0;
      for i = 1 to dim - 1 do Format.pp_print_cut ppf (); line i done;
    Format.fprintf ppf "@]"
  | n -> unexpected [n]

(* ( A / B) B = A *)
let mat_div x y =
  let dim = mat_dim x in
  let left = A.copy y and right = A.copy x in
  let perm = Array.init dim (fun n -> n) in
(*  let (#.) a (i,j) = a.( i * dim + perm.(j) ) in
  let set (//) a (i,j)  y =
    let pos = i * dim + perm.(j) in
    a.(pos)<- a.(pos) // y in
    let (#+) = set (+.) and (#/) = set ( /. )in*)
  let switch i j =
    let tmp = perm.(i) in
    perm.(i) <- perm.(j); perm.(j) <- tmp; in
  let pivot start =
    let pos = start * dim in
    let mx = ref (abs_float left#.(pos + perm.(start) ) )
    and i = ref start in
    for j = start + 1 to dim -1 do
      let m = abs_float left#.(pos + perm.(j)) in
      if m > !mx then
        (mx := m; i:=j)
    done;
    if start <> !i then switch start !i in
  let col_transf coeff k l =
    let k' = perm.(k) and l' = perm.(l) in
    for i = k to dim-1 do
      let i = dim * i in
      let il = i + l' and ik = i + k' in
      A.set left il @@ left#.(il) +.  coeff *. left#.(ik);
      A.set right il @@ right#.(il) +.  coeff *. right#.(ik);
    done;
    for i = 0 to k-1 do
      let i = dim * i in
      let il = i + l' and ik = i + k' in
      A.set right il @@ right#.(il) +.  coeff *. right#.(ik);
    done;
  in
  (* zero upper *)
  for i = 0 to dim - 1 do
    let i' = dim * i in
    pivot i;
    let x = left#.(i' + perm.(i)) in
    if x <> 0. then
      for  j = i + 1 to dim - 1 do
        let coeff = -. left#.(i' + perm.(j)) /. x in
        if coeff <> 0. then
          col_transf coeff i j
      done
  done;
  (* zero lower *)
  for i = dim-1 downto 0 do
    let i' = dim * i in
    let x = left#.(i' + perm.(i)) in
    if x <> 0. then
      for j = 0 to i-1 do
      let coeff = -. left#.(i' + perm.(j)) /. x in
      col_transf coeff i j
    done
  done;
  for i = 0 to dim - 1 do
    for j = 0 to dim - 1 do
      let j' = perm.(j) in
      let ij = i * dim + j' in
      let x = left#.(j * dim + j') in
      if x <> 0. then
        A.set right ij @@ right#.(ij) /. x
      done;
  done;
  let data = left in
  for j = 0 to dim -1 do
    let j' = perm.(j) in
    for i = 0 to dim - 1 do
      let i = dim * i in
      A.set data (i + j) @@ right#.(i + j');
    done
  done;
 data

type +'x scalar = ('a, 'b z) t constraint 'x = 'a * 'b

type +'x vec2 = ('a two,'b one) t constraint 'x = 'a * 'b
type +'x vec3 = ('a three,'b one) t constraint 'x = 'a * 'b
type +'x vec4 = ('a four,'b one) t constraint 'x = 'a * 'b

type +'x mat2 = ('a two,'b two) t constraint 'x = 'a * 'b
type +'x mat3 = ('a three,'b two) t constraint 'x = 'a * 'b
type +'x mat4 = ('a four,'b two) t constraint 'x = 'a * 'b


let ( |+| ) a b =
  let l = A.len a + A.len b in
  let sep = A.len a in
  let data = A.create l in
  for i = 0 to sep - 1 do
    A.set data i a#.i
  done;
  for i = sep to l - 1 do
    A.set data i @@ b#.(i-sep)
  done;
  data


let scalar x = A.from_array [|x|]
let vec2 x y = A.from_array [|x;y|]
let vec3 x y z = A.from_array [|x;y;z|]
let vec4 x y z t = A.from_array [|x;y;z;t|]

let vec2' a =
  if rank a = 0 then
    A.make 2 a#.0
  else
    A.copy a

let vec_stretch k a =
  if rank a = 0 then
    A.make 2 a#.(0)
  else
    let data = A.create k in
    A.iteri (A.set data) a;
    let l = A.len a in
    let repeated = a#.(l-1) in
    for i = l to k -1 do
      A.set data i repeated
    done;
    data

let vec3' x = vec_stretch 3 x
let vec4' x = vec_stretch 4 x



let mat2 a b = A.from_array
  [| a#.(0); a#.(1); b#.(0); b#.(1); 0. (* padding *) |]
let mat3 a b c = A.from_array
  [| a#.(0); a#.(1); a#.(2);
     b#.(0); b#.(1); b#.(2);
     c#.(0); c#.(1); c#.(2) |]

let mat4 a b c d = A.from_array
  [| a#.(0); a#.(1); a#.(2); a#.(3);
     b#.(0); b#.(1); b#.(2); b#.(3);
     c#.(0); c#.(1); c#.(2); c#.(3);
     d#.(0); d#.(1); d#.(2); d#.(3);
  |]


let mat_len = function
  | 2 -> 5
  | 3 -> 9
  | 4 -> 16
  | _ -> assert false

let swizzle v index =
  let size = ilen index in
  A.init size
    (fun i ->
       let pos = i lsl 2 in
       let index' = index lsr pos in
       let masked = 0xF land index' in
       v#.(masked)
    )

let slice (t: (_,_) t) (n:(_ index)) =
  match rank t with
  | 0 -> scalar t#.(0)
  | 1 ->
    if ilen n = 1 then
      scalar t#.(n land 0xF)
    else swizzle t n
  | 2 ->
    let dim = mat_dim t in
    let len = ilen n in
    if irank n = 2 then
      if len = 1 then
        scalar @@ t#.( dim * ( 0x3 land n) + ((n lsr 2) land 0x3) )
      else begin
        A.init len (fun i ->
            let s = ((n lsr (i lsl 2)) land 0xF) in
            let i = 0x3 land s in
            let j = s lsr 2 in
             t#.( dim * i + j) )
      end
    else if len = 1 then
      A.init dim
        (fun i -> t#.( i + dim * (n land 0x3) ))
    else
      let data = A.create (mat_len dim) in
      let pos = ref n in
      for i = 0 to dim - 1 do
        let s = dim * (0x3 land !pos) in
        let i = i * dim in
        for j = 0 to dim - 1 do
          A.set data (i + j) @@ t#.( s + j)
        done;
        pos := !pos lsr 4
      done;
      data
  | n -> unexpected [n]


let get (t: (_,_) t) (n:(_ index)) = match rank t with
  | 0 -> t#.(0)
  | 1 -> t#.(n land 0x3 )
  | 2 -> t#.( (n lsr 2) land 0x3 + mat_dim t * (n land 0x3) )
  | n -> unexpected [n]
;;

#if OCAML_MAJOR>=4 && OCAML_MINOR>=6
let (.%()) x = get x
let (.%[]) x = slice x
#endif

let amap2 f x y =
  A.init
    (min (A.len x) (A.len y))
    (fun i -> f x#.(i) y#.(i))

let map f x = A.map f x
let map2 f x y = amap2 f x y
let smap f x y = map (f x#.(0)) y


let cross a b =
  if A.len a = 2 then
    scalar (a#.(0)*.b#.(1) -. a#.(1) *. b#.(0))
  else
    vec3
      (a#.(1) *. b#.(2) -. a#.(2) *. b#.(1) )
      (a#.(2) *. b#.(0) -. a#.(0) *. b#.(2) )
      (a#.(0) *. b#.(1) -. a#.(1) *. b#.(0) )

let ( ^ ) a b =
  let dim = A.len a in
  let data = A.create (mat_len dim) in
  for i = 0 to dim -1 do
    for j = 0 to dim - 1 do
      let r = a#.(i) *. b#.(j) -. a#.(j) *. b#.(i) in
      A.set data ( i * dim + j ) @@ r +. data#.( i + dim + j);
      A.set data ( j * dim + i ) @@ data#.( j + dim + i) -. r
    done
  done;
  data

let ( * ) a b = match rank a, rank b with
  | 0, _ -> smap ( *. ) a b
  | _, 0 -> smap ( *. ) b a
  | 1, 1 -> map2 ( *. ) a b
  | 1, 2 | 2, 1 ->
    let dim = min (A.len a) (A.len b) in
    let a , b, s1, s2= if rank a = 1 then a, b, dim, 1
      else b, a, 1, dim in
    let sum i = let s = ref 0. and ij = ref (i * s1) in
      for j = 0 to dim -1 do
        s:= !s +. a#.(j) *. b#.(!ij);
        ij := s2 + !ij done;
      !s
    in A.init dim sum
  | 2, 2 ->
    let dim = mat_dim a in
    let sum i j = let s = ref 0. in
      for k = 0 to dim - 1 do
        s:= a#.(i * dim + k) *. b#.(k * dim + j) +. !s done;
      !s
    in
    let data = A.init (A.len a)
        (fun n -> sum (n / dim) (n mod dim)) in
    data
  | x, y  -> unexpected [x;y]


let ( / ) a b =
  match rank a, rank b with
  | 1, 1 -> map2 (/.) a b
  | 2, 2 ->
    mat_div a b
  | _ -> smap (fun x y -> y /. x ) b a

let ( |*| ) a b =
  let s = ref 0. in
  for i = 0 to (A.len a - 1) do
    s:= !s +. a#.(i) *. b#.(i)
  done;
  !s

let norm x = sqrt (x|*|x)

let (+) a b =
  if rank a = 0 then
    smap (+.) a b
  else if rank b = 0 then
    smap (+.) b a
  else map2 (+.) a b

let (-) a b =
  if rank a = 0 then
    smap (-.) a b
  else if rank b = 0 then
    A.init (A.len b)
      (fun n -> b#.(n) -. a#.(0))
  else map2 (-.) a b


let floor a = int_of_float ( a#.(0) )
