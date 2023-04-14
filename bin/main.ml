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
