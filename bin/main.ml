let () = print_endline "Hello, World!"
let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

let rec sort lst =
  match lst with [] -> [] | head :: tail -> insert head (sort tail)

and insert elt lst =
  match lst with
  | [] -> [ elt ]
  | head :: tail -> if elt <= head then elt :: lst else head :: insert elt tail

let deriv f dx x = (f (x +. dx) -. f x) /. dx
let sin' = deriv sin 1e-6
let rec map f l = match l with [] -> [] | hd :: tl -> f hd :: map f tl

type number = Int of int | Float of float | Error
type sign = Positive | Negative

let sign_int n = if n >= 0 then Positive else Negative

let add_num n1 n2 =
  match (n1, n2) with
  | Int i1, Int i2 ->
      if sign_int i1 = sign_int i2 && sign_int (i1 + i2) <> sign_int i1 then
        Float (float i1 +. float i2)
      else Int (i1 + i2)
  | Int i1, Float f2 -> Float (float i1 +. f2)
  | Float f1, Int i2 -> Float (f1 +. float i2)
  | Float f1, Float f2 -> Float (f1 +. f2)
  | Error, _ -> Error
  | _, Error -> Error

type 'a option = Some of 'a | None

let safe_square_root x = if x > 0. then Some (sqrt x) else None

type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

let rec member x btree =
  match btree with
  | Empty -> false
  | Node (y, left, right) ->
      if x = y then true else if x < y then member x left else member x right

let rec insert x btree =
  match btree with
  | Empty -> Node (x, Empty, Empty)
  | Node (y, left, right) ->
      if x <= y then Node (y, insert x left, right)
      else Node (y, left, insert x right)

type first_record = { x : int; y : int; z : int }
type middle_record = { x : int; z : int }
type last_record = { x : int }
type first_variant = A | B | C
type last_variant = A

let look_at_x_then_z (r : first_record) =
  let x = r.x in
  x + r.z

let permute (x : first_variant) =
  match x with A -> (B : first_variant) | B -> A | C -> C

type wrapped = First of first_record

let f (First r) = (r, r.x)

let add_vect v1 v2 =
  let len = min (Array.length v1) (Array.length v2) in
  let res = Array.make len 0.0 in
  for i = 0 to len - 1 do
    res.(i) <- v1.(i) +. v2.(i)
  done;
  res

type mutable_point = { mutable x : float; mutable y : float }

let translate p dx dy =
  p.x <- p.x +. dx;
  p.y <- p.y +. dy

let mypoint = { x = 0.0; y = 0.0 }

let insertion_sort a =
  for i = 1 to Array.length a - 1 do
    let val_i = a.(i) in
    let j = ref i in
    while !j > 0 && val_i < a.(!j - 1) do
      a.(!j) <- a.(!j - 1);
      j := !j - 1
    done;
    a.(!j) <- val_i
  done
