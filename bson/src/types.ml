module ObjectID : sig
  type t

  val of_string: string -> t
  val to_string: t -> string
end = struct
  type t = string
  let of_string t = t
  let to_string t = t
end

exception Invalid_objectId

exception Wrong_bson_type

exception Wrong_string

exception Malformed_bson

type binary =
  | Generic of string
  | Function of string
  | UUID of string
  | MD5 of string
  | UserDefined of string

type t = (string * element) list

and element =
  [ `Double of float
  | `String of string
  | `Document of t
  | `Array of element list
  | `Binary of binary
  | `ObjectId of ObjectID.t
  | `Boolean of bool
  | `UTC of int64
  | `Null
  | `MinKey
  | `MaxKey
  | `Regex of string * string
  | `JSCode of string
  | `JSCodeWS of string * t
  | `Int32 of int32
  | `Int64 of int64
  | `Timestamp of int64
  ]

let nul = '\x00'

let has_element = List.mem_assoc

(* The remove operations. *)
let remove_element = List.remove_assoc

(* for constructing a document 1. we make a empty document 2. we create element
   as we want 3. we add the element to the document, with a element name *)
(* TODO(anmonteiro): wtf is this, check the spec if we need to check dupes *)
let add_element (ename, element) doc =
  (* Emulating StringMap add operation *)
  let doc =
    if has_element ename doc then
      remove_element ename doc
    else
      doc
  in
  (ename, element) :: doc

