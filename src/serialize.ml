module Bser = Bson.Serialize

let encode_header f { Header.message_len; request_id; response_to; op } =
  Bser.encode_int32 f message_len;
  Bser.encode_int32 f request_id;
  Bser.encode_int32 f response_to;
  Bser.encode_int32 f (Operation.to_code op)

module Request = struct
  let combine_header_body f request_id op body =
    let body_len = Bigstringaf.length body in
    encode_header f (Header.create_request_header body_len request_id op);
    Faraday.schedule_bigstring f body

  let insert f (db_name, collection_name) (request_id, flags) insert_doc_list =
    let body_f = Faraday.create 32 in
    Bser.encode_int32 body_f flags;
    Bser.encode_cstring body_f (db_name ^ "." ^ collection_name);
    List.iter (Bser.encode body_f) insert_doc_list;
    combine_header_body
      f
      request_id
      OP_INSERT
      (Faraday.serialize_to_bigstring body_f)

  let create_select_body_buf
      (db_name, collection_name) (_request_id, flags) selector_doc
    =
    let body_buf = Faraday.create 32 in
    Bser.encode_int32 body_buf 0l;
    Bser.encode_cstring body_buf (db_name ^ "." ^ collection_name);
    Bser.encode_int32 body_buf flags;
    Bser.encode body_buf selector_doc;
    body_buf

  let[@deprecated "use the OP_MSG opcode instead."] update
      f (db_name, collection_name) (request_id, flags) (selector_doc, update_doc)
    =
    let body_buf =
      create_select_body_buf
        (db_name, collection_name)
        (request_id, flags)
        selector_doc
    in
    Bser.encode body_buf update_doc;
    combine_header_body
      f
      request_id
      OP_UPDATE
      (Faraday.serialize_to_bigstring body_buf)

  let[@deprecated "use the OP_MSG opcode instead."] delete
      f (db_name, collection_name) (request_id, flags) selector_doc
    =
    let body_buf =
      create_select_body_buf
        (db_name, collection_name)
        (request_id, flags)
        selector_doc
    in
    combine_header_body
      f
      request_id
      OP_DELETE
      (Faraday.serialize_to_bigstring body_buf)

  let[@deprecated "use the OP_MSG opcode instead."] query
      f
      (db_name, collection_name)
      (request_id, flags, skip, return)
      (query_doc, selector_doc)
    =
    let body_buf = Faraday.create 32 in
    Bser.encode_int32 body_buf flags;
    Bser.encode_cstring body_buf (db_name ^ "." ^ collection_name);
    Bser.encode_int32 body_buf skip;
    Bser.encode_int32 body_buf return;
    Bser.encode body_buf query_doc;
    if not (Bson.is_empty selector_doc) then
      Bser.encode body_buf selector_doc;
    combine_header_body
      f
      request_id
      OP_QUERY
      (Faraday.serialize_to_bigstring body_buf)

  let[@deprecated "use the OP_MSG opcode instead."] get_more
      f (db_name, collection_name) (request_id, return) cursor
    =
    let body_buf = Faraday.create 32 in
    Bser.encode_int32 body_buf 0l;
    Bser.encode_cstring body_buf (db_name ^ "." ^ collection_name);
    Bser.encode_int32 body_buf return;
    Bser.encode_int64 body_buf cursor;
    combine_header_body
      f
      request_id
      OP_GET_MORE
      (Faraday.serialize_to_bigstring body_buf)

  let[@deprecated "use the OP_MSG opcode instead."] kill_cursors
      f request_id cursor_list
    =
    let body_buf = Faraday.create 32 in
    Bser.encode_int32 body_buf 0l;
    let cursor_buf = Faraday.create 12 in
    let rec create_cursor_buf num = function
      | [] ->
        num
      | hd :: tl ->
        Bser.encode_int64 cursor_buf hd;
        create_cursor_buf (num + 1) tl
    in
    let num = create_cursor_buf 0 cursor_list in
    Bser.encode_int32 body_buf (Int32.of_int num);
    Faraday.schedule_bigstring
      body_buf
      (Faraday.serialize_to_bigstring cursor_buf);
    combine_header_body
      f
      request_id
      OP_KILL_CURSORS
      (Faraday.serialize_to_bigstring body_buf)

  let message f ~db request_id section =
    let body_buf = Faraday.create 32 in
    (* flag bits *)
    Bser.encode_int32 body_buf 0l;
    (* subtype *)
    Bser.encode_char body_buf '\x00';
    (* Format.eprintf "SECION: %s %s@." db_name collection_name; *)
    Bser.encode
      body_buf
      (Bson.add_element "$db" (Bson.create_string db) section);
    combine_header_body
      f
      request_id
      OP_MSG
      (Faraday.serialize_to_bigstring body_buf)
end

module Writer = struct
  type t =
    { buffer : Bigstringaf.t
          (* The buffer that the encoder uses for buffered writes. Managed by the
           * control module for the encoder. *)
    ; encoder : Faraday.t
          (* The encoder that handles encoding for writes. Uses the [buffer]
           * referenced above internally. *)
    ; mutable drained_bytes : int
          (* The number of bytes that were not written due to the output stream
           * being closed before all buffered output could be written. Useful for
           * detecting error cases. *)
    ; mutable wakeup : Optional_thunk.t
          (* The callback from the runtime to be invoked when output is ready to be
           * flushed. *)
    }

  let create ?(buffer_size = 0x800) () =
    let buffer = Bigstringaf.create buffer_size in
    let encoder = Faraday.of_bigstring buffer in
    { buffer; encoder; drained_bytes = 0; wakeup = Optional_thunk.none }

  let faraday t = t.encoder

  let insert t (db_name, collection_name) (request_id, flags) insert_doc_list =
    Request.insert
      t.encoder
      (db_name, collection_name)
      (request_id, flags)
      insert_doc_list

  let create_select_body_buf
      t (db_name, collection_name) (request_id, flags) selector_doc
    =
    Request.insert
      t.encoder
      (db_name, collection_name)
      (request_id, flags)
      selector_doc

  let update
      t (db_name, collection_name) (request_id, flags) (selector_doc, update_doc)
    =
    Request.update
      t.encoder
      (db_name, collection_name)
      (request_id, flags)
      (selector_doc, update_doc)

  let delete t (db_name, collection_name) (request_id, flags) selector_doc =
    Request.delete
      t.encoder
      (db_name, collection_name)
      (request_id, flags)
      selector_doc

  let query
      t
      (db_name, collection_name)
      (request_id, flags, skip, return)
      (query_doc, selector_doc)
    =
    Request.query
      t.encoder
      (db_name, collection_name)
      (request_id, flags, skip, return)
      (query_doc, selector_doc)

  let get_more t (db_name, collection_name) (request_id, return) cursor =
    Request.get_more
      t.encoder
      (db_name, collection_name)
      (request_id, return)
      cursor

  let kill_cursors t request_id cursor_list =
    Request.kill_cursors t.encoder request_id cursor_list

  let message t = Request.message t.encoder

  let on_wakeup t k =
    if Faraday.is_closed t.encoder then
      failwith "on_wakeup on closed writer"
    else if Optional_thunk.is_some t.wakeup then
      failwith "on_wakeup: only one callback can be registered at a time"
    else
      t.wakeup <- Optional_thunk.some k

  let wakeup t =
    let f = t.wakeup in
    t.wakeup <- Optional_thunk.none;
    Optional_thunk.call_if_some f

  let flush t f = Faraday.flush t.encoder f

  let unyield t =
    (* This would be better implemented by a function that just takes the
       encoder out of a yielded state if it's in that state. Requires a change
       to the faraday library. *)
    flush t (fun () -> ())

  let yield t = Faraday.yield t.encoder

  let close t = Faraday.close t.encoder

  let close_and_drain t =
    Faraday.close t.encoder;
    let drained = Faraday.drain t.encoder in
    t.drained_bytes <- t.drained_bytes + drained;
    wakeup t

  let is_closed t = Faraday.is_closed t.encoder

  let drained_bytes t = t.drained_bytes

  let report_result t result =
    match result with
    | `Closed ->
      close_and_drain t
    | `Ok len ->
      Faraday.shift t.encoder len

  let next t =
    match Faraday.operation t.encoder with
    | `Close ->
      `Close (drained_bytes t)
    | `Yield ->
      `Yield
    | `Writev iovecs ->
      `Write iovecs
end
