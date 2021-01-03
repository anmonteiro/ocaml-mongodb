(** {b This is a major client-faced module, for the high level usage.}

    This module includes a series of APIs that client can use directly to
    communicate with MongoDB. The most important functions are for insert,
    udpate, delete, query, get_more. They are the essential interactions that a
    client can have with MongoDB.

    Please note that the current version of APIs here are all essential only.
    For example, Clients cannot set detailed flags for queries, etc. All
    operations here are with default flags (which is 0).

    A Mongo is bound to a db and a collection. All operations will be done upon
    the bound db and collection only.

    Please refer to
    {{:http://docs.mongodb.org/meta-driver/latest/legacy/mongodb-wire-protocol/}
    MongoDB Wire Protocol} for more information *)

(** the exception will be raised if anything is wrong, with a string message *)
module Config : sig
  type t =
    { db : string
    ; collection : string
    ; host : string
    ; port : int
    ; max_connections : int
    }

  val create
    :  ?host:string
    -> ?port:int
    -> ?max_connections:int
    -> db:string
    -> collection:string
    -> unit
    -> t
end

module Response : sig
  type t =
    { header : Header.t
    ; response_flags : int32
    ; cursor_id : int64
    ; starting_from : int32
    ; num_returned : int32
    ; document_list : Bson.t list
    }

  val to_string : t -> string

  val pp_hum : Format.formatter -> t -> unit
end

module Connection : sig
  type t

  type error =
    [ `Protocol_error of string
    | `Eof
    | (* of Error_code.t * string *)
      `Exn of exn
    ]

  type ('a, 'b) response_handler = ('a, 'b) result -> unit

  type error_handler = error -> unit

  val create : config:Config.t -> t

  val with_ : ?collection:string -> ?database:string -> t -> t

  val with_collection : t -> string -> t
  (** change instance collection *)

  val message
    :  t
    -> doc:Bson.t
    -> response_handler:(Bson.t, 'a list) response_handler
    -> unit

  type write_error =
    { index : int
    ; code : int
    ; message : string
    }

  val insert
    :  ?ordered:bool
    -> ?bypass_document_validation:bool
    -> t
    -> response_handler:((int, write_error list) result -> unit)
    -> Bson.t list
    -> unit

  (** {6 Insert} *)

  (** {6 Update} *)
  type upserted =
    { index : int
    ; _id : string
    }

  type update_output =
    { found : int
    ; updated : int
    ; upserted : upserted list
    }

  val update
    :  t
    -> ?ordered:bool
    -> ?bypass_document_validation:bool
    -> upsert:bool
    -> all:bool
    -> selector:Bson.t
    -> response_handler:(update_output, write_error list) response_handler
    -> Bson.t
    -> unit

  (** update the {b first document} matched in MongoDB. e.g., update_one m (s,
      u);; m is the Mongo. s is the selector document, used to match the
      documents that need to be updated. u is the update document and any
      matched documents will be updated to u. May raise Mongo_failed exception.*)

  val update_one
    :  t
    -> ?upsert:bool
    -> selector:Bson.t
    -> Bson.t
    -> response_handler:((update_output, write_error list) result -> unit)
    -> unit

  val update_many
    :  t
    -> ?upsert:bool
    -> selector:Bson.t
    -> Bson.t
    -> response_handler:((update_output, write_error list) result -> unit)
    -> unit
  (** update {b all documents} matched in MongoDB. e.g., update m (s, u);; m is
      the Mongo. s is the selector document, used to match the documents that
      need to be updated. u is the update document and any matched documents
      will be updated to u. May raise Mongo_failed exception. *)

  (** {6 Delete} *)

  val delete
    :  ?ordered:bool
    -> t
    -> all:bool
    -> selector:Bson.t
    -> response_handler:(int, 'a list) response_handler
    -> unit

  val delete_one
    :  t
    -> Bson.t
    -> response_handler:(int, 'a list) response_handler
    -> unit
  (** delete the {b first document} matched in MongoDB. e.g., delete_one m s;; m
      is the Mongo. s is the selector document, used to match the documents that
      need to be deleted. May raise Mongo_failed exception.*)

  val delete_many
    :  t
    -> Bson.t
    -> response_handler:(int, 'a list) response_handler
    -> unit
  (** delete the {b all documents} matched in MongoDB. e.g., delete_one m s;; m
      is the Mongo. s is the selector document, used to match the documents that
      need to be deleted. May raise Mongo_failed exception.*)

  (** {6 Query / Find} *)

  val find
    :  ?skip:int
    -> ?limit:int
    -> ?filter:Bson.t
    -> ?sort:Bson.t
    -> ?selector:Bson.t
    -> t
    -> response_handler:(Bson.t, 'a list) response_handler
    -> unit
  (** find {b all / the default number} of documents in the db and collection.
      May raise Mongo_failed exception.*)

  val find_one
    :  ?skip:int
    -> ?filter:Bson.t
    -> ?selector:Bson.t
    -> t
    -> response_handler:(Bson.t, 'a list) response_handler
    -> unit
  (** find {b the first} document in the db and collection. May raise
      Mongo_failed exception.*)

  val find_and_modify
    :  ?bypass_document_validation:bool
    -> ?query:Bson.t
    -> ?sort:Bson.t
    -> ?remove:bool
    -> ?update:Bson.t
    -> ?new_:bool
    -> ?selector:Bson.t
    -> ?upsert:bool
    -> t
    -> response_handler:(Bson.t, 'a list) response_handler
    -> unit

  val count
    :  ?skip:int
    -> ?limit:int
    -> ?query:Bson.t
    -> t
    -> response_handler:(int, 'a list) response_handler
    -> unit
  (** counts the number of documents in a collection *)

  (** {6 Query / Find more via cursor} *)

  val get_more
    :  t
    -> ?limit:int
    -> ?max_time_ms:int
    -> cursor:int64
    -> response_handler:(Bson.t, 'a list) response_handler
    -> unit
  (** get {b all / the default number} of documents via a cursor_id. e.g.
      get_more_of_num m cursor_id num. May raise Mongo_failed exception.*)

  (** {6 Kill cursor} *)

  val kill_cursors
    :  t
    -> int64 list
    -> response_handler:(Bson.t, 'a list) response_handler
    -> unit
  (** kill a list of cursors, to save MongoDB resources. e.g., kill_cursors m
      cursor_list. May raise Mongo_failed exception.*)

  (** {6 Index} *)

  (** option for index. See
      {b
         http://docs.mongodb.org/manual/reference/method/db.collection.ensureIndex/#db.collection.ensureIndex}
      for more info *)
  type index_option =
    | Background of bool
    | Unique of bool
    | Name of string
    | DropDups of bool
    | Sparse of bool
    | ExpireAfterSeconds of int
    | V of int
    | Weight of Bson.t
    | Default_language of string
    | Language_override of string

  val get_indexes
    :  t
    -> response_handler:(Bson.t, 'a list) response_handler
    -> unit
  (** return a list of all indexes *)

  val ensure_index
    :  t
    -> key:Bson.t
    -> options:index_option list
    -> response_handler:((int, write_error list) result -> unit)
    -> unit

  (** ensure an index *)
  val ensure_simple_index
    :  ?options:index_option list
    -> t
    -> string
    -> response_handler:((int, write_error list) result -> unit)
    -> unit
  (** ensure an index (helper) *)

  val ensure_multi_simple_index
    :  ?options:index_option list
    -> t
    -> string list
    -> response_handler:((int, write_error list) result -> unit)
    -> unit
  (** ensure multi-fields index (helper) *)

  val drop_index
    :  t
    -> string
    -> response_handler:(Bson.t, 'a list) response_handler
    -> unit
  (** drop a index *)

  val drop_all_index
    :  t
    -> response_handler:(Bson.t, 'a list) response_handler
    -> unit
  (** drop all index of a collection *)

  (** {6 Instance Administration Commands} *)

  val drop_collection
    :  t
    -> response_handler:(Bson.t, 'a list) response_handler
    -> unit
  (** removes an entire collection from a database *)

  val drop_database
    :  t
    -> response_handler:(Bson.t, 'a list) response_handler
    -> unit
  (** drops a database, deleting the associated data files *)

  module Admin : sig
    val of_connection : t -> t

    val command
      :  t
      -> string
      -> response_handler:(Bson.t, 'a list) response_handler
      -> unit

    val listDatabases
      :  t
      -> response_handler:(Bson.t, 'a list) response_handler
      -> unit

    val buildInfo
      :  t
      -> response_handler:(Bson.t, 'a list) response_handler
      -> unit

    val collStats
      :  t
      -> response_handler:(Bson.t, 'a list) response_handler
      -> unit

    val connPoolStats
      :  t
      -> response_handler:(Bson.t, 'a list) response_handler
      -> unit

    val cursorInfo
      :  t
      -> response_handler:(Bson.t, 'a list) response_handler
      -> unit

    val getCmdLineOpts
      :  t
      -> response_handler:(Bson.t, 'a list) response_handler
      -> unit

    val hostInfo
      :  t
      -> response_handler:(Bson.t, 'a list) response_handler
      -> unit

    val listCommands
      :  t
      -> response_handler:(Bson.t, 'a list) response_handler
      -> unit

    val serverStatus
      :  t
      -> response_handler:(Bson.t, 'a list) response_handler
      -> unit
  end

  val shutdown : t -> unit
  (** [shutdown connection] initiates the graceful shutdown of [connection], and
      sends an HTTP/2 GOAWAY frame with NO_ERROR on the output channel (See
      {{:https://tools.ietf.org/html/rfc7540#section-6.8} RFC7540§6.8} for more
      details). *)

  val next_read_operation : t -> [> `Read | `Close ]
  (** [next_read_operation t] returns a value describing the next operation that
      the caller should conduct on behalf of the connection. *)

  val read : t -> Bigstringaf.t -> off:int -> len:int -> int
  (** [read t bigstring ~off ~len] reads bytes of input from the provided range
      of [bigstring] and returns the number of bytes consumed by the connection.
      {!read} should be called after {!next_read_operation} returns a [`Read]
      value and additional input is available for the connection to consume. *)

  val read_eof : t -> Bigstringaf.t -> off:int -> len:int -> int
  (** [read t bigstring ~off ~len] reads bytes of input from the provided range
      of [bigstring] and returns the number of bytes consumed by the connection.
      {!read} should be called after {!next_read_operation} returns a [`Read]
      and an EOF has been received from the communication channel. The
      connection will attempt to consume any buffered input and then shutdown
      the HTTP parser for the connection. *)

  val next_write_operation
    :  t
    -> [ `Write of Bigstringaf.t Faraday.iovec list | `Yield | `Close of int ]
  (** [next_write_operation t] returns a value describing the next operation
      that the caller should conduct on behalf of the connection. *)

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  (** [report_write_result t result] reports the result of the latest write
      attempt to the connection. {!report_write_result} should be called after a
      call to {!next_write_operation} that returns a [`Write buffer] value.

      - [`Ok n] indicates that the caller successfully wrote [n] bytes of output
        from the buffer that the caller was provided by {!next_write_operation}.
      - [`Closed] indicates that the output destination will no longer accept
        bytes from the write processor. *)

  val yield_writer : t -> (unit -> unit) -> unit
  (** [yield_writer t continue] registers with the connection to call [continue]
      when writing should resume. {!yield_writer} should be called after
      {!next_write_operation} returns a [`Yield] value. *)

  val yield_reader : t -> (unit -> unit) -> unit
  (** [yield_reader t continue] immediately calls [continue]. This function *
      shouldn't generally be called and it's only here to simplify adhering * to
      the Gluten [RUNTIME] module type. *)

  val report_exn : t -> exn -> unit
  (** [report_exn t exn] reports that an error [exn] has been caught and that it
      has been attributed to [t]. Calling this function will switch [t] into an
      error state. Depending on the state [t] is transitioning from, it may call
      its (connection-level) error handler before terminating the connection. *)

  val is_closed : t -> bool
  (** [is_closed t] is [true] if both the read and write processors have been
      shutdown. When this is the case {!next_read_operation} will return
      [`Close _] and {!next_write_operation} will do the same will return a
      [`Write _] until all buffered output has been flushed, at which point it
      will return [`Close]. *)
end
