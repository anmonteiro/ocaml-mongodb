module Reader = Parse.Reader
module Writer = Serialize.Writer

type error =
  [ `Protocol_error of string
  | `Eof
  | (* of Error_code.t * string *)
    `Exn of exn
  ]

type ('a, 'b) response_handler = ('a, 'b) result -> unit

type error_handler = error -> unit

type t =
  { config : Config.t
  ; reader : Reader.reply
  ; writer : Writer.t
  ; requests : (int32, (Response.t -> unit) * error_handler) Hashtbl.t
  ; mutable request_id : int32
  }

let with_ ?collection ?database t =
  let collection =
    match collection with
    | Some collection ->
      collection
    | None ->
      t.config.collection
  in
  let db = match database with Some db -> db | None -> t.config.db in
  { t with config = { t.config with collection; db } }

let with_collection t collection = with_ t ~collection

let is_closed t = Reader.is_closed t.reader && Writer.is_closed t.writer

let is_active t = not (Hashtbl.length t.requests = 0)

let yield_reader t k = Reader.on_wakeup t.reader k

let wakeup_reader t = Reader.wakeup t.reader

let yield_writer t k = Writer.on_wakeup t.writer k

let wakeup_writer t = Writer.wakeup t.writer

let create ~config =
  let requests = Hashtbl.create 24 in
  let handler t (response : Response.t) =
    let t = Lazy.force t in
    let id = response.header.response_to in
    let real_handler, _error_handler = Hashtbl.find t.requests id in
    Hashtbl.remove t.requests id;
    real_handler response
  in
  let rec t =
    lazy
      { config
      ; reader = Reader.reply (handler t)
      ; writer = Writer.create ()
      ; requests
      ; request_id = 0l
      }
  in
  Lazy.force t

let drain t =
  Hashtbl.iter
    (fun _ (_resp_handler, error_handler) -> error_handler `Eof)
    t.requests;
  Hashtbl.clear t.requests

let next_request_id t =
  let r = Int32.succ t.request_id in
  t.request_id <- r;
  r

let request ?(error_handler = ignore) t ~write ~response_handler =
  let request_id = next_request_id t in
  Hashtbl.add t.requests request_id (response_handler, error_handler);
  write ~request_id;
  wakeup_writer t

let maybe_add opt ~f ~name bson =
  match opt with Some opt -> Bson.add_element name (f opt) bson | None -> bson

let maybe_add_bson = maybe_add ~f:Bson.create_doc_element

let maybe_add_int = maybe_add ~f:(fun x -> Bson.create_int32 (Int32.of_int x))

let maybe_add_bool = maybe_add ~f:Bson.create_boolean

let op_msg
    ?ordered
    ?bypass_document_validation
    ~op:(op_name, op_val)
    (payload_name, payload)
  =
  let bson =
    Bson.empty
    |> Bson.add_element op_name (Bson.create_string op_val)
    |> Bson.add_element payload_name (Bson.create_doc_element_list payload)
    |> maybe_add_bool ~name:"ordered" ordered
    |> maybe_add_bool
         ~name:"bypassDocumentValidation"
         bypass_document_validation
  in
  bson

type write_error =
  { index : int
  ; code : int
  ; message : string
  }

let decode_write_error bson =
  let bson = Bson.get_doc_element bson in
  { index = Bson.get_int32 (Bson.get_element "index" bson) |> Int32.to_int
  ; code = Bson.get_int32 (Bson.get_element "code" bson) |> Int32.to_int
  ; message = Bson.get_string (Bson.get_element "errmsg" bson)
  }

let make_response_handler f { Response.num_returned = _; document_list; _ } =
  let d = List.hd document_list in
  match Bson.get_element "ok" d |> Bson.get_double with
  | 1. ->
    f (Ok d)
  | _ ->
    f (Error [])

let write_ops_response_handler f d =
  f
    (Result.bind d (fun d ->
         match Bson.get_list (Bson.get_element "writeErrors" d) with
         | xs ->
           Error (List.map decode_write_error xs)
         | exception Not_found ->
           Ok d))

let message t ~doc ~response_handler =
  let write ~request_id =
    Writer.message t.writer ~db:t.config.db request_id doc
  in
  let response_handler = make_response_handler response_handler in
  request t ~write ~response_handler

(* TODO(anmonteiro): error_handlers for all of these *)
let insert ?ordered ?bypass_document_validation t ~response_handler doc_list =
  let bson =
    op_msg
      ?ordered
      ?bypass_document_validation
      ~op:("insert", t.config.collection)
      ("documents", doc_list)
  in
  let response_handler doc =
    response_handler
      (Result.map
         (fun d -> Int32.to_int (Bson.get_int32 (Bson.get_element "n" d)))
         doc)
  in
  let response_handler = write_ops_response_handler response_handler in
  message t ~response_handler ~doc:bson

type upserted =
  { index : int
  ; _id : string
  }

type update_output =
  { found : int
  ; updated : int
  ; upserted : upserted list
  }

let update
    t
    ?ordered
    ?bypass_document_validation
    ~upsert
    ~all
    ~selector
    ~response_handler
    update
  =
  let bson =
    op_msg
      ?ordered
      ?bypass_document_validation
      ~op:("update", t.config.collection)
      ( "updates"
      , [ Bson.empty
          |> Bson.add_element "q" (Bson.create_doc_element selector)
          |> Bson.add_element "u" (Bson.create_doc_element update)
          |> Bson.add_element "upsert" (Bson.create_boolean upsert)
          |> Bson.add_element "multi" (Bson.create_boolean all)
        ] )
  in
  let response_handler doc =
    response_handler
      (Result.map
         (fun doc ->
           { found = Bson.get_int32 (Bson.get_element "n" doc) |> Int32.to_int
           ; updated =
               Bson.get_int32 (Bson.get_element "nModified" doc) |> Int32.to_int
           ; upserted =
               (if upsert then
                  List.map
                    (fun upserted ->
                      let upserted = Bson.get_doc_element upserted in
                      { index =
                          Bson.get_int32 (Bson.get_element "index" upserted)
                          |> Int32.to_int
                      ; _id =
                          Bson.get_objectId (Bson.get_element "_id" upserted)
                      })
                    (Bson.get_list (Bson.get_element "upserted" doc))
               else
                 [])
           })
         doc)
  in
  let response_handler = write_ops_response_handler response_handler in
  message t ~doc:bson ~response_handler

let update_one t ?(upsert = false) ~selector update_doc =
  update t ~upsert ~all:false ~selector update_doc

let update_many t ?(upsert = false) ~selector update_doc =
  update t ~upsert ~all:true ~selector update_doc

let delete ?ordered t ~all ~selector ~response_handler =
  let bson =
    op_msg
      ?ordered
      ~op:("delete", t.config.collection)
      ( "deletes"
      , [ Bson.empty
          |> Bson.add_element "q" (Bson.create_doc_element selector)
          |> Bson.add_element
               "limit"
               (Bson.create_int32 (if all then 0l else 1l))
        ] )
  in
  let response_handler d =
    response_handler
      (Result.map
         (fun d -> Int32.to_int (Bson.get_int32 (Bson.get_element "n" d)))
         d)
  in
  message t ~doc:bson ~response_handler

let delete_one t selector = delete t ~all:false ~selector

let delete_many t selector = delete t ~all:true ~selector

let find ?skip ?limit ?filter ?sort ?projection t ~response_handler =
  let bson =
    Bson.empty
    |> Bson.add_element "find" (Bson.create_string t.config.collection)
    |> maybe_add_bson ~name:"filter" filter
    |> maybe_add_bson ~name:"sort" sort
    |> maybe_add_bson ~name:"projection" projection
    |> maybe_add_int ~name:"skip" skip
    |> maybe_add_int ~name:"limit" limit
  in
  message t ~doc:bson ~response_handler

let find_one ?skip ?filter ?projection t ~response_handler =
  find t ~response_handler ?skip ~limit:1 ?filter ?projection

let find_and_modify
    ?bypass_document_validation
    ?query
    ?sort
    ?remove
    ?update
    ?new_
    ?projection
    ?upsert
    t
    ~response_handler
  =
  let bson =
    Bson.empty
    |> Bson.add_element "findAndModify" (Bson.create_string t.config.collection)
    |> maybe_add_bson ~name:"query" query
    |> maybe_add_bson ~name:"sort" sort
    |> maybe_add_bool ~name:"remove" remove
    |> maybe_add_bson ~name:"update" update
    |> maybe_add_bool ~name:"new" new_
    |> maybe_add_bson ~name:"fields" projection
    |> maybe_add_bool ~name:"upsert" upsert
    |> maybe_add_bool
         ~name:"bypassDocumentValidation"
         bypass_document_validation
  in
  message t ~doc:bson ~response_handler

let count ?skip ?limit ?query t ~response_handler =
  let bson =
    Bson.empty
    |> Bson.add_element "count" (Bson.create_string t.config.collection)
    |> maybe_add_bson ~name:"query" query
    |> maybe_add_int ~name:"skip" skip
    |> maybe_add_int ~name:"limit" limit
  in
  let response_handler d =
    response_handler
      (Result.map
         (fun d -> int_of_float (Bson.get_double (Bson.get_element "n" d)))
         d)
  in
  message t ~doc:bson ~response_handler

let get_more t ?limit ?max_time_ms ~response_handler cursor =
  let bson =
    Bson.empty
    |> Bson.add_element "getMore" (Bson.create_int64 cursor)
    |> Bson.add_element "collection" (Bson.create_string t.config.collection)
    |> maybe_add_int ~name:"batchSize" limit
    |> maybe_add_int ~name:"maxTimeMS" max_time_ms
  in
  message t ~doc:bson ~response_handler

let kill_cursors t cursors ~response_handler =
  let bson =
    Bson.empty
    |> Bson.add_element "killCursors" (Bson.create_string t.config.collection)
    |> Bson.add_element
         "cursors"
         (Bson.create_list (List.map Bson.create_int64 cursors))
  in
  message t ~doc:bson ~response_handler

let drop_database t ~response_handler =
  message
    t
    ~doc:(Bson.add_element "dropDatabase" (Bson.create_int32 1l) Bson.empty)
    ~response_handler

let drop_collection t ~response_handler =
  let collection = t.config.collection in
  message
    t
    ~doc:(Bson.add_element "drop" (Bson.create_string collection) Bson.empty)
    ~response_handler

let get_indexes t ~response_handler =
  message
    t
    ~doc:
      (Bson.add_element
         "listIndexes"
         (Bson.create_string t.config.collection)
         Bson.empty)
    ~response_handler

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

let ensure_index t ~key ~options ~response_handler =
  let default_name () =
    let doc = Bson.get_element "key" key in
    List.fold_left
      (fun s (k, e) ->
        let i = Bson.get_int32 e in
        if s = "" then
          Printf.sprintf "%s_%ld" k i
        else
          Printf.sprintf "%s_%s_%ld" s k i)
      ""
      (Bson.all_elements (Bson.get_doc_element doc))
  in
  let has_name = ref false in
  let has_version = ref false in
  (* check all options *)
  let main_bson =
    List.fold_left
      (fun acc o ->
        match o with
        | Background b ->
          Bson.add_element "background" (Bson.create_boolean b) acc
        | Unique b ->
          Bson.add_element "unique" (Bson.create_boolean b) acc
        | Name s ->
          has_name := true;
          Bson.add_element "name" (Bson.create_string s) acc
        | DropDups b ->
          Bson.add_element "dropDups" (Bson.create_boolean b) acc
        | Sparse b ->
          Bson.add_element "sparse" (Bson.create_boolean b) acc
        | ExpireAfterSeconds i ->
          Bson.add_element
            "expireAfterSeconds"
            (Bson.create_int32 (Int32.of_int i))
            acc
        | V i ->
          if i <> 0 && i <> 1 then
            raise (Failure "Version number for index must be 0 or 1");
          has_version := true;
          Bson.add_element "v" (Bson.create_int32 (Int32.of_int i)) acc
        | Weight bson ->
          Bson.add_element "weights" (Bson.create_doc_element bson) acc
        | Default_language s ->
          Bson.add_element "default_language" (Bson.create_string s) acc
        | Language_override s ->
          Bson.add_element "language_override" (Bson.create_string s) acc)
      key
      options
  in
  (* check if then name has been set, create a default name otherwise *)
  let main_bson =
    if not !has_name then
      Bson.add_element "name" (Bson.create_string (default_name ())) main_bson
    else
      main_bson
  in
  (* check if the version has been set, set 1 otherwise *)
  let main_bson =
    if not !has_version then
      Bson.add_element "v" (Bson.create_int32 1l) main_bson
    else
      main_bson
  in
  let main_bson =
    Bson.add_element
      "ns"
      (Bson.create_string (t.config.db ^ "." ^ t.config.collection))
      main_bson
  in
  let system_indexes_m = with_collection t "system.indexes" in
  insert system_indexes_m [ main_bson ] ~response_handler

let ensure_simple_index ?(options = []) t field =
  let key =
    Bson.add_element
      "key"
      (Bson.create_doc_element
         (Bson.add_element field (Bson.create_int32 1l) Bson.empty))
      Bson.empty
  in
  ensure_index t ~key ~options

let ensure_multi_simple_index ?(options = []) t fields =
  let key =
    List.fold_left
      (fun acc f -> Bson.add_element f (Bson.create_int32 1l) acc)
      Bson.empty
      fields
  in
  let key = Bson.add_element "key" (Bson.create_doc_element key) Bson.empty in
  ensure_index t ~key ~options

let drop_index t index_name ~response_handler =
  let bson =
    Bson.empty
    |> Bson.add_element "dropIndexes" (Bson.create_string t.config.collection)
    |> Bson.add_element "index" (Bson.create_string index_name)
  in
  message t ~doc:bson ~response_handler

let drop_all_index t = drop_index t "*"

module Admin = struct
  let of_connection t = with_ t ~database:"admin" ~collection:"$cmd"

  let command t op ~response_handler =
    let bson = Bson.empty |> Bson.add_element op (Bson.create_int32 1l) in
    message (of_connection t) ~doc:bson ~response_handler

  let listDatabases t = command t "listDatabases"

  let buildInfo t = command t "buildInfo"

  let collStats t = command t "collStats"

  let connPoolStats t = command t "connPoolStats"

  let cursorInfo t = command t "cursorInfo"

  let getCmdLineOpts t = command t "getCmdLineOpts"

  let hostInfo t = command t "hostInfo"

  let listCommands t = command t "listCommands"

  let serverStatus t = command t "serverStatus"
end

let shutdown_reader t =
  Reader.force_close t.reader;
  if is_active t then
    drain t
  else
    wakeup_reader t

let shutdown_writer t =
  Writer.close t.writer;
  if is_active t then drain t

let shutdown t =
  drain t;
  shutdown_reader t;
  shutdown_writer t;
  wakeup_reader t;
  wakeup_writer t

let set_error_and_handle t error =
  Hashtbl.iter
    (fun _ (_resp_handler, error_handler) -> error_handler error)
    t.requests;
  shutdown t

let unexpected_eof t = set_error_and_handle t `Eof

let report_exn t exn =
  Format.eprintf
    "ERR %S@\n %s@."
    (Printexc.to_string exn)
    (Printexc.get_backtrace ());
  set_error_and_handle t (`Exn exn)

let next_read_operation t =
  if Reader.is_closed t.reader then shutdown_reader t;
  match Reader.next t.reader with
  | (`Read | `Close) as operation ->
    operation
  | `Error (`Parse (marks, message)) ->
    let message = String.concat "" [ String.concat ">" marks; ": "; message ] in
    set_error_and_handle t (`Protocol_error message);
    `Close

let read_with_more t bs ~off ~len more =
  Reader.read_with_more t.reader bs ~off ~len more

let read t bs ~off ~len = read_with_more t bs ~off ~len Incomplete

let read_eof t bs ~off ~len =
  let consumed = read_with_more t bs ~off ~len Complete in
  if is_active t then unexpected_eof t;
  consumed

let next_write_operation t = Writer.next t.writer

let report_write_result t result = Writer.report_result t.writer result
