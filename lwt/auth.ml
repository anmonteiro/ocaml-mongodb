(* TODO: each auth function should return a result of (unit, string) *)

type auth =
  [ `NoAuth
  | `Plain of string * string
  | `MongoCR of string * string
  | `SCRAM_SHA_1 of string * string
  | `SCRAM_SHA_256 of string * string
  ]

let parse_payload payload_str =
  let parts = String.split_on_char ',' payload_str in
  List.fold_left
    (fun acc part ->
      match String.split_on_char '=' part with
      | k :: v :: xs ->
        (k, v ^ String.make (List.length xs) '=') :: acc
      | _ ->
        failwith (Printf.sprintf "split should have at least 2 parts: %s" part))
    []
    parts

let hi data salt iterations mode dk_len =
  Pbkdf.pbkdf2
    ~prf:mode
    ~salt
    ~password:data
    ~count:iterations
    ~dk_len

let hmac key text =
  Digestif.SHA1.hmac_string ~key text |> Digestif.SHA1.to_raw_string

let h mode text =
  let (module H) = Digestif.module_of_hash' mode in
  H.digest_string text |> H.to_raw_string

let xor a b =
  Mirage_crypto.Uncommon.xor a b |> Base64.encode_exn
