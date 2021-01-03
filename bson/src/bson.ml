module Parse = Parse
module Serialize = Serialize

include Types

let empty = [];;

let is_empty = function
  | [] -> true
  | _ -> false;;

let has_element = List.mem_assoc

(*
  The remove  operations.
*)
let remove_element = List.remove_assoc

(*
  for constructing a document
  1. we make a empty document
  2. we create element as we want
  3. we add the element to the document, with a element name
*)
let add_element ename element doc =
  (* Emulating StringMap add operation *)
  let doc =
    if has_element ename doc then remove_element ename doc
    else doc
  in
  (ename,element)::doc;;

(*
  for using a document
  1. we get an element from document, if existing
  2. we get the value of the element
*)
let get_element = List.assoc



let create_double v = `Double v;;
let create_string v = `String v;;
let create_doc_element v = `Document v;;
let create_list l = `Array l;;
let create_doc_element_list l = create_list (List.map create_doc_element l);;
(* let create_generic_binary v = Binary (Generic v);;
let create_function_binary v = Binary (Function v);;
let create_uuid_binary v = Binary (UUID v);;
let create_md5_binary v = Binary (MD5 v);; *)
let create_user_binary v = `Binary (UserDefined v);;
(* let is_valid_objectId objectId = if String.length objectId = 12 || String.length objectId = 24 then true else false;; *)

let hex_to_string str =
  let string_fold ~f ~z str =
    let st = ref z in
    ( String.iter (fun c -> st := f !st c) str  ; !st )
  in
  let hexdigit p = function
    | 'a' .. 'f' as x -> int_of_char x - 87
    | 'A' .. 'F' as x -> int_of_char x - 55
    | '0' .. '9' as x -> int_of_char x - 48
    | x ->
      Format.ksprintf invalid_arg "of_hex: invalid character at pos %d: %C" p x
  in
  match
    string_fold
      ~f:(fun (s, i, p, acc) ->
          let p' = succ p in
          function
          | char ->
            match acc, hexdigit p char with
            | (None  , x) -> (s, i, p', Some (x lsl 4))
            | (Some y, x) -> Bytes.set_uint8 s i (x lor y) ; (s, succ i, p', None))
      ~z:(Bytes.create (String.length str lsr 1), 0, 0, None)
      str
  with
  | _ , _, _, Some _ ->
    Format.ksprintf invalid_arg "of_hex: odd numbers of characters"
  | bytes, _, _, _ -> Bytes.to_string bytes

let create_objectId v =
  if String.length v = 12 then `ObjectId (ObjectID.of_string v)
  else if String.length v = 24 then
   try (`ObjectId (ObjectID.of_string (hex_to_string v))) with (Failure "int_of_string" [@ocaml.warning "-52"]) -> raise Invalid_objectId
  else raise Invalid_objectId;;
let create_boolean v = `Boolean v;;
let create_utc v = `UTC v;;
let create_null () = `Null;;
let create_regex s1 s2 = `Regex (s1, s2);;
let create_jscode v = `JSCode v;;
let create_jscode_w_s s doc = `JSCodeWS (s, doc);;
let create_int32 v = `Int32 v;;
let create_int64 v = `Int64 v;;
(* let create_timestamp v = Timestamp v;; *)
let create_minkey () = `MinKey;;
let create_maxkey () = `MaxKey;;

let get_double = function | `Double v -> v | _ -> raise Wrong_bson_type;;
let get_string = function | `String v -> v | _ -> raise Wrong_bson_type;;
let get_doc_element = function | `Document v -> v | _ -> raise Wrong_bson_type;;
let get_list = function | `Array v -> v | _ -> raise Wrong_bson_type;;
let get_generic_binary = function | `Binary (Generic v) -> v | _ -> raise Wrong_bson_type;;
let get_function_binary = function | `Binary (Function v) -> v | _ -> raise Wrong_bson_type;;
let get_uuid_binary = function | `Binary (UUID v) -> v | _ -> raise Wrong_bson_type;;
let get_md5_binary = function | `Binary (MD5 v) -> v | _ -> raise Wrong_bson_type;;
let get_user_binary = function | `Binary (UserDefined v) -> v | _ -> raise Wrong_bson_type;;
let get_objectId = function | `ObjectId v -> ObjectID.to_string v | _ -> raise Wrong_bson_type;;
let get_boolean = function | `Boolean v -> v | _ -> raise Wrong_bson_type;;
let get_utc = function | `UTC v -> v | _ -> raise Wrong_bson_type;;
let get_regex = function | `Regex v -> v | _ -> raise Wrong_bson_type;;
let get_jscode = function | `JSCode v -> v | _ -> raise Wrong_bson_type;;
let get_jscode_w_s = function | `JSCodeWS v -> v | _ -> raise Wrong_bson_type;;
let get_int32 = function | `Int32 v -> v | _ -> raise Wrong_bson_type;;
let get_int64 = function | `Int64 v -> v | _ -> raise Wrong_bson_type;;
let get_timestamp = function | `Timestamp v -> v | _ -> raise Wrong_bson_type;;

let all_elements d = d

let hexdump_pp fmt t =
  for i = 0 to String.length t - 1 do
    let c = Char.code t.[i] in
    Format.fprintf fmt "%.2x"  c
  done

let hexdump = Format.asprintf "%a" hexdump_pp


  (*
    Not that this bson to json conversion is far from completion.
    It is used to help the test verification and can handle only simple objects.
  *)
let to_simple_json doc =
  let rec el_to_sl el =
    List.rev (List.fold_left (fun acc e -> (e_to_s e)::acc) [] el)
  and e_to_s = function
    | `Double v -> string_of_float v
    | `String v -> "\"" ^ v ^ "\""
    | `Document v -> d_to_s v
    | `Array v -> let sl = el_to_sl v in "[" ^ (String.concat ", " sl) ^ "]"
    | `Binary v ->
      begin match v with
	| Generic v | Function v | UUID v | MD5 v | UserDefined v -> "\"" ^ v ^ "\""
      end
    | `ObjectId v ->
        let v = ObjectID.to_string v in
        "\"" ^ (hexdump v) ^ "\""
    | `Boolean v -> if v then "true" else "false"
    | `UTC v -> Int64.to_string v
    | `Null-> "\"null\""
    | `Regex (v1,v2) -> "(\"" ^ v1 ^ ", \"" ^ v2 ^ "\")"
    | `JSCode v -> "\"" ^ v ^ "\""
    | `JSCodeWS (v, d) -> "(\"" ^ v ^ ", \"" ^ (d_to_s d) ^ "\")"
    | `Int32 v -> Int32.to_string v
    | `Timestamp v -> Int64.to_string v
    | `Int64 v -> Int64.to_string v
    | `MinKey -> "\"minkey\""
    | `MaxKey -> "\"maxkey\""
  and d_to_s d =
    let buf = Buffer.create 16 in
    Buffer.add_string buf "{";
    (* let bindings = all_elements d in *)
    let process acc (ename, element) =
      ("\"" ^ ename ^ "\" : " ^ (e_to_s element)) :: acc;
    in
    Buffer.add_string buf (String.concat ", " (List.rev (List.fold_left process [] d)));
    Buffer.add_string buf "}";
    Buffer.contents buf
  in
  d_to_s doc;;
