include Ppx_deriving_runtime

let (>>=) x f =
  match x with Ok x -> f x | (Error _) as x -> x

let (>|=) x f =
  x >>= fun x -> Ok (f x)

let rec map_bind f acc xs =
  match xs with
  | x :: xs -> f x >>= fun x -> map_bind f (x :: acc) xs
  | [] -> Ok (List.rev acc)

type 'a error_or = ('a, string) result

(** [safe_map f l] returns the same value as [List.map f l], but
    computes it tail-recursively so that large list lengths don't
    cause a stack overflow *)
let safe_map f l = List.rev (List.rev_map f l)
