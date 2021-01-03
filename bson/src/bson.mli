(** This module includes a Bson document data structure, together with its
    encoding (to bytes) and decoding (from bytes).

    The logic of {b usage} is like this

    - Create an empty Bson document
    - Create the elements you want
    - Add elements to the document with names
    - Or remove elements from the document via the names
    - Get elements from the document via the names
    - After obtaining an element, get the raw value from the element

    The functions inside this module seem to be many, however, most of them are
    just for creating elements. These functions are to
    {e hide the implementation details of the type elements}. Also, in this way,
    the Bson document can be used more safely.

    Please refer to the {{:http://bsonspec.org/#/specification} Official Bson
    specification} for more information.

    {e Version 0.89.1} *)

(** Raised when an objectId's length is not 12. see
    http://bsonspec.org/#/specification *)
exception Invalid_objectId

(** Raised when an unkown bson type is met while encoding the bson doc *)
exception Wrong_bson_type

(** Raised only when trying to decode the bytes to string. *)
exception Wrong_string

(** Raised when bad things happen while decoding the bytes to bson doc *)
exception Malformed_bson

type binary =
  | Generic of string
  | Function of string
  | UUID of string
  | MD5 of string
  | UserDefined of string

module ObjectID : sig
  type t
end

(** The type for the Bson document. This is the main data structure *)
type t = (string * element) list

(** The type for the fields for the Bson document *)
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

(** {6 Basic operations on Bson document} *)

val empty : t
(** The empty Bson document *)

val is_empty : t -> bool
(** Check whether this Bson document empty or not *)

val add_element : string -> element -> t -> t
(** Add an element to a Bson document *)

val get_element : string -> t -> element
(** Get an element from a Bson document via its name *)

val has_element : string -> t -> bool
(** Check whether this Bson document has a specific element *)

val remove_element : string -> t -> t
(** Remove an element from a Bson document *)

module Parse : sig
  val decode_int64 : int64 Angstrom.t

  val decode_int32 : int32 Angstrom.t

  val decode_doc : t Angstrom.t
  (** Decode bytes (assuming type string as a carrier) to a Bson document *)

  val decode : string -> t
end

module Serialize : sig
  val encode_int64 : Faraday.t -> int64 -> unit

  val encode_int32 : Faraday.t -> int32 -> unit

  val encode_char : Faraday.t -> char -> unit

  val encode_cstring : Faraday.t -> string -> unit

  val encode : Faraday.t -> t -> unit
  (** Encode a Bson document to bytes (using type string as a carrier) *)
end

(** {6 Creating elements} *)

val create_double : float -> element

val create_string : string -> element

val create_doc_element : t -> element

val create_list : element list -> element

val create_doc_element_list : t list -> element

val create_user_binary : string -> element

val create_objectId : string -> element

val create_boolean : bool -> element

val create_utc : int64 -> element

val create_null : unit -> element

val create_regex : string -> string -> element

val create_jscode : string -> element

val create_jscode_w_s : string -> t -> element

val create_int32 : int32 -> element

val create_int64 : int64 -> element

val create_minkey : unit -> element

val create_maxkey : unit -> element

(** {6 Getting raw values from elements} *)

val get_double : element -> float

val get_string : element -> string

val get_doc_element : element -> t

val get_list : element -> element list

val get_generic_binary : element -> string

val get_function_binary : element -> string

val get_uuid_binary : element -> string

val get_md5_binary : element -> string

val get_user_binary : element -> string

val get_objectId : element -> string

val get_boolean : element -> bool

val get_utc : element -> int64

val get_regex : element -> string * string

val get_jscode : element -> string

val get_jscode_w_s : element -> string * t

val get_int32 : element -> int32

val get_int64 : element -> int64

val get_timestamp : element -> int64

val all_elements : t -> (string * element) list

(** {6 Experimental. Convert a Bson document to Json.} *)

val to_simple_json : t -> string

(*val create_generic_binary : string -> element;; val create_function_binary :
  string -> element;; val create_uuid_binary : string -> element;; val
  create_md5_binary : string -> element;; val create_timestamp : int64 ->
  element;;*)
