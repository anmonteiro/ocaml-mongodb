open Types

let has_element = List.mem_assoc

(* The remove operations. *)
let remove_element = List.remove_assoc

(* for constructing a document 1. we make a empty document 2. we create element
   as we want 3. we add the element to the document, with a element name *)
let add_element ename element doc =
  (* Emulating StringMap add operation *)
  let doc =
    if has_element ename doc then
      remove_element ename doc
    else
      doc
  in
  (ename, element) :: doc

let encode_int64 = Faraday.LE.write_uint64

let encode_float f v = encode_int64 f (Int64.bits_of_float v)

let encode_int32 = Faraday.LE.write_uint32

let encode_ename f c ename =
  Faraday.write_char f c;
  Faraday.write_string f ename;
  Faraday.write_char f nul

let encode_string f s =
  let len = String.length s in
  if len > 0 && s.[len - 1] = nul then
    raise Wrong_string
  else (
    encode_int32 f (Int32.of_int (len + 1));
    Faraday.write_string f s;
    Faraday.write_char f nul)

let encode_objectId f s =
  let s = ObjectID.to_string s in
  if String.length s <> 12 then
    raise Invalid_objectId
  else
    Faraday.write_string f s

let encode_binary f c b =
  encode_int32 f (Int32.of_int (String.length b));
  Faraday.write_char f c;
  Faraday.write_string f b

let encode_char f c = Faraday.write_char f c

let encode_cstring f cs =
  Faraday.write_string f cs;
  encode_char f '\x00'

let list_to_doc l =
  (* we need to transform the list to a doc with key as incrementing from '0' *)
  let rec to_doc i acc = function
    | [] ->
      acc
    | hd :: tl ->
      to_doc (i + 1) (add_element (string_of_int i) hd acc) tl
  in
  to_doc 0 [] l

let encode f doc =
  let rec encode_element f ename element =
    match element with
    | `Double v ->
      encode_ename f '\x01' ename;
      encode_float f v
    | `String v ->
      encode_ename f '\x02' ename;
      encode_string f v
    | `Document v ->
      encode_ename f '\x03' ename;
      encode_doc f v
    | `Array v ->
      encode_ename f '\x04' ename;
      encode_doc f (list_to_doc v)
    | `Binary v ->
      encode_ename f '\x05' ename;
      (match v with
      | Generic v ->
        encode_binary f '\x00' v
      | Function v ->
        encode_binary f '\x01' v
      | UUID v ->
        encode_binary f '\x04' v
      | MD5 v ->
        encode_binary f '\x05' v
      | UserDefined v ->
        encode_binary f '\x80' v)
    | `ObjectId v ->
      encode_ename f '\x07' ename;
      encode_objectId f v
    | `Boolean v ->
      encode_ename f '\x08' ename;
      Faraday.write_char f (if v then '\x01' else '\x00')
    | `UTC v ->
      encode_ename f '\x09' ename;
      encode_int64 f v
    | `Null ->
      encode_ename f '\x0A' ename
    | `Regex (v1, v2) ->
      encode_ename f '\x0B' ename;
      encode_cstring f v1;
      encode_cstring f v2
    | `JSCode v ->
      encode_ename f '\x0D' ename;
      encode_string f v
    | `JSCodeWS (v, d) ->
      encode_ename f '\x0F' ename;
      let tmp_str_f = Faraday.create 16
      and tmp_doc_f = Faraday.create 16 in
      encode_string tmp_str_f v;
      encode_doc tmp_doc_f d;
      encode_int32
        f
        (Int32.of_int
           (4
           + Faraday.pending_bytes tmp_str_f
           + Faraday.pending_bytes tmp_doc_f));
      Faraday.schedule_bigstring f (Faraday.serialize_to_bigstring tmp_str_f);
      Faraday.schedule_bigstring f (Faraday.serialize_to_bigstring tmp_doc_f)
    | `Int32 v ->
      encode_ename f '\x10' ename;
      encode_int32 f v
    | `Timestamp v ->
      encode_ename f '\x11' ename;
      encode_int64 f v
    | `Int64 v ->
      encode_ename f '\x12' ename;
      encode_int64 f v
    | `MinKey ->
      encode_ename f '\xFF' ename
    | `MaxKey ->
      encode_ename f '\x7F' ename
  and encode_doc f doc =
    let process_element f (ename, element) =
      encode_element f ename element;
      f
    in
    let e_f =
      List.fold_left process_element (Faraday.create 64) (List.rev doc)
    in
    encode_int32 f (Int32.of_int (5 + Faraday.pending_bytes e_f));
    Faraday.schedule_bigstring f (Faraday.serialize_to_bigstring e_f);
    Faraday.write_char f '\x00'
  in
  encode_doc f doc
