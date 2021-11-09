open Mongo

module type Client = sig
  type socket

  type runtime

  type t

  val create_connection
    :  config:Config.t
    -> ?auth:
         [ `NoAuth
         | `Plain of string * string
         | `MongoCR of string * string
         | `SCRAM_SHA_1 of string * string
         | `SCRAM_SHA_256 of string * string
         ]
    -> socket
    -> (t, 'a list) Lwt_result.t

  val with_ : ?collection:string -> ?database:string -> t -> t

  val with_collection : t -> string -> t

  val message : t -> doc:Bson.t -> (Bson.t, 'a list) Lwt_result.t

  val insert
    :  ?ordered:bool
    -> ?bypass_document_validation:bool
    -> t
    -> Bson.t list
    -> (int, Connection.write_error list) Lwt_result.t
  (** {6 Insert} *)

  (** {6 Update} *)

  val update
    :  ?ordered:bool
    -> ?bypass_document_validation:bool
    -> t
    -> upsert:bool
    -> all:bool
    -> selector:Bson.t
    -> Bson.t
    -> (Connection.update_output, Connection.write_error list) Lwt_result.t

  (** update the {b first document} matched in MongoDB. e.g., update_one m (s,
      u);; m is the Mongo. s is the selector document, used to match the
      documents that need to be updated. u is the update document and any
      matched documents will be updated to u. May raise Mongo_failed exception.*)

  val update_one
    :  t
    -> ?upsert:bool
    -> selector:Bson.t
    -> Bson.t
    -> (Connection.update_output, Connection.write_error list) Lwt_result.t

  val update_many
    :  t
    -> ?upsert:bool
    -> selector:Bson.t
    -> Bson.t
    -> (Connection.update_output, Connection.write_error list) Lwt_result.t
  (** update {b all documents} matched in MongoDB. e.g., update m (s, u);; m is
      the Mongo. s is the selector document, used to match the documents that
      need to be updated. u is the update document and any matched documents
      will be updated to u. May raise Mongo_failed exception. *)

  (** {6 Delete} *)

  val delete
    :  ?ordered:bool
    -> t
    -> all:bool
    -> Bson.t
    -> (int, Connection.write_error list) Lwt_result.t

  val delete_one
    :  t
    -> Bson.t
    -> (int, Connection.write_error list) Lwt_result.t
  (** delete the {b first document} matched in MongoDB. e.g., delete_one m s;; m
      is the Mongo. s is the selector document, used to match the documents that
      need to be deleted. May raise Mongo_failed exception.*)

  val delete_many
    :  t
    -> Bson.t
    -> (int, Connection.write_error list) Lwt_result.t
  (** delete the {b all documents} matched in MongoDB. e.g., delete_one m s;; m
      is the Mongo. s is the selector document, used to match the documents that
      need to be deleted. May raise Mongo_failed exception.*)

  (** {6 Query / Find} *)

  val find
    :  ?skip:int
    -> ?limit:int
    -> ?filter:Bson.t
    -> ?projection:Bson.t
    -> ?sort:Bson.t
    -> t
    -> (Bson.t, 'a list) Lwt_result.t
  (** find {b all / the default number} of documents in the db and collection.
      May raise Mongo_failed exception.*)

  val find_one
    :  ?skip:int
    -> ?filter:Bson.t
    -> ?projection:Bson.t
    -> t
    -> (Bson.t, 'a list) Lwt_result.t
  (** find {b the first} document in the db and collection. May raise
      Mongo_failed exception.*)

  val find_and_modify
    :  ?bypass_document_validation:bool
    -> ?query:Bson.t
    -> ?sort:Bson.t
    -> ?remove:bool
    -> ?update:Bson.t
    -> ?new_:bool
    -> ?projection:Bson.t
    -> ?upsert:bool
    -> t
    -> (Bson.t, 'a list) Lwt_result.t

  val count
    :  ?skip:int
    -> ?limit:int
    -> ?query:Bson.t
    -> t
    -> (int, 'a list) Lwt_result.t
  (** counts the number of documents in a collection *)

  (** {6 Query / Find more via cursor} *)

  val get_more
    :  t
    -> ?limit:int
    -> ?max_time_ms:int
    -> cursor:int64
    -> (Bson.t, 'a list) Lwt_result.t
  (** get {b all / the default number} of documents via a cursor_id. e.g.
      get_more_of_num m cursor_id num. May raise Mongo_failed exception.*)

  (** {6 Kill cursor} *)

  val kill_cursors : t -> int64 list -> (Bson.t, 'a list) Lwt_result.t
  (** kill a list of cursors, to save MongoDB resources. e.g., kill_cursors m
      cursor_list. May raise Mongo_failed exception.*)

  val get_indexes : t -> (Bson.t, 'a list) Lwt_result.t
  (** return a list of all indexes *)

  val ensure_index
    :  t
    -> key:Bson.t
    -> options:Connection.index_option list
    -> (int, Connection.write_error list) Lwt_result.t

  (** ensure an index *)
  val ensure_simple_index
    :  ?options:Connection.index_option list
    -> t
    -> string
    -> (int, Connection.write_error list) Lwt_result.t
  (** ensure an index (helper) *)

  val ensure_multi_simple_index
    :  ?options:Connection.index_option list
    -> t
    -> string list
    -> (int, Connection.write_error list) Lwt_result.t
  (** ensure multi-fields index (helper) *)

  val drop_index : t -> string -> (Bson.t, 'a list) Lwt_result.t
  (** drop a index *)

  val drop_all_index : t -> (Bson.t, 'a list) Lwt_result.t
  (** drop all index of a collection *)

  (** {6 Instance Administration Commands} *)

  val drop_collection : t -> (Bson.t, 'a list) Lwt_result.t
  (** removes an entire collection from a database *)

  val drop_database : t -> (Bson.t, 'a list) Lwt_result.t
  (** drops a database, deleting the associated data files *)

  module Admin : sig
    val of_connection : t -> t

    val command : t -> string -> (Bson.t, 'a list) Lwt_result.t

    val listDatabases : t -> (Bson.t, 'a list) Lwt_result.t

    val buildInfo : t -> (Bson.t, 'a list) Lwt_result.t

    val collStats : t -> (Bson.t, 'a list) Lwt_result.t

    val connPoolStats : t -> (Bson.t, 'a list) Lwt_result.t

    val cursorInfo : t -> (Bson.t, 'a list) Lwt_result.t

    val getCmdLineOpts : t -> (Bson.t, 'a list) Lwt_result.t

    val hostInfo : t -> (Bson.t, 'a list) Lwt_result.t

    val listCommands : t -> (Bson.t, 'a list) Lwt_result.t

    val serverStatus : t -> (Bson.t, 'a list) Lwt_result.t
  end

  val shutdown : t -> unit Lwt.t

  val is_closed : t -> bool
end
