open Types
module LE = Angstrom.LE

let decode_int64 = LE.any_int64

let decode_int32 = LE.any_int32

let next_x00 str cur = String.index_from str cur

let decode_ename =
  let open Angstrom in
  Angstrom.take_till (function '\x00' -> true | _ -> false) >>= fun ename ->
  Angstrom.char nul >>| fun _ -> ename

let decode_cstring = decode_ename

let decode_len = Angstrom.lift Int32.to_int decode_int32

let decode_double = Angstrom.lift Int64.float_of_bits decode_int64

let decode_string =
  let open Angstrom in
  decode_len >>= fun len ->
  (* From {http://bsonspec.org/spec.html}:
   *   String - The int32 is the number bytes in the (byte*\) + 1 (for the
   *   trailing '\x00'). The (byte*\) is zero or more UTF-8 encoded characters.
   *)
  take (len - 1) >>= fun str ->
  Angstrom.char nul >>| fun _ -> str


let doc_to_list doc =
  (* we need to transform a doc with key as incrementing from '0' to a list *)
  List.map snd doc

let decode_binary =
  let open Angstrom in
  decode_len >>= fun len ->
  any_char >>= fun subtype ->
  take len >>| fun str ->
  match subtype with
  | '\x00' ->
    `Binary (Generic str)
  | '\x01' ->
    `Binary (Function str)
  | '\x04' ->
    `Binary (UUID str)
  | '\x05' ->
    `Binary (MD5 str)
  | '\x80' ->
    `Binary (UserDefined str)
  | '\x06' ->
    failwith "NYI: encrypted BSON"
  | _ ->
    raise Malformed_bson

let decode_objectId =
  Angstrom.lift
    (fun oid -> `ObjectId (ObjectID.of_string oid))
    (Angstrom.take 12)

let decode_boolean =
  Angstrom.lift
    (fun c ->
      `Boolean
        (match c with
        | '\x00' ->
          false
        | _ ->
          assert (c = '\x01');
          true))
    Angstrom.any_char

let decode_utc = Angstrom.lift (fun i -> `UTC i) decode_int64

let decode_regex =
  Angstrom.lift2 (fun s o -> `Regex (s, o)) decode_cstring decode_cstring

let decode_jscode = Angstrom.lift (fun s -> `JSCode s) decode_string

let decode_doc =
  let open Angstrom in
  let decode_element decode_doc =
    any_char >>= fun c ->
    decode_ename >>= fun ename ->
    let element =
      match c with
      | '\x01' ->
        lift (fun d -> `Double d) decode_double
      | '\x02' ->
        lift (fun s -> `String s) decode_string
      | '\x03' ->
        lift (fun doc -> `Document doc) decode_doc
      | '\x04' ->
        lift (fun doc -> `Array (doc_to_list doc)) decode_doc
      | '\x05' ->
        decode_binary
      | '\x07' ->
        decode_objectId
      | '\x08' ->
        decode_boolean
      | '\x09' ->
        decode_utc
      | '\x0A' ->
        return `Null
      | '\x0B' ->
        decode_regex
      | '\x0D' ->
        decode_jscode
      | '\x0F' ->
        (* decode jscode_w_s *)
        lift3
          (fun _len s doc -> `JSCodeWS (s, doc))
          decode_len
          decode_string
          decode_doc
      | '\x10' ->
        lift (fun i -> `Int32 i) decode_int32
      | '\x11' ->
        lift (fun i -> `Timestamp i) decode_int64
      | '\x12' ->
        lift (fun i -> `Int64 i) decode_int64
      | '\xFF' ->
        return `MinKey
      | '\x7F' ->
        return `MaxKey
      | _ ->
        raise Malformed_bson
    in
    element >>| fun element -> ename, element
  in
  let decode_doc =
    fix (fun decode_doc ->
        decode_len >>= fun _len ->
        (* TODO(anmonteiro): assert that len is equal to what fix consumed *)
        let decode_element = decode_element decode_doc in
        fix (fun m ->
            let _rec = lift2 add_element decode_element m in
            peek_char_fail >>= function
            | '\x00' ->
              char nul *> return []
            | _ ->
              _rec))
  in
  decode_doc

let decode str =
  Result.get_ok (Angstrom.parse_string ~consume:All decode_doc str)
