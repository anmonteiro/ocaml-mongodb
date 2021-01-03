open Lwt.Infix
open Mongo
include Mongo_lwt_intf

module Client (Client_runtime : Gluten_lwt.Client) = struct
  type socket = Client_runtime.socket

  type runtime = Client_runtime.t

  type t =
    { connection : Connection.t
    ; runtime : runtime
    }

  let wrap
      :  t -> f:(Connection.t -> response_handler:('a -> unit) -> unit)
      -> 'a Lwt.t
    =
   fun t ~f ->
    let awaiter, awakener = Lwt.wait () in
    let response_handler r = Lwt.wakeup_later awakener r in
    f t.connection ~response_handler;
    awaiter

  let with_ ?collection ?database t =
    { t with connection = Connection.with_ ?collection ?database t.connection }

  let with_collection t collection = with_ t ~collection

  let message t ~doc = wrap t ~f:(Connection.message ~doc)

  let insert ?ordered ?bypass_document_validation t doc_list =
    wrap t ~f:(fun t ->
        Connection.insert ?ordered ?bypass_document_validation t doc_list)

  let update
      ?ordered ?bypass_document_validation t ~upsert ~all ~selector update
    =
    wrap t ~f:(fun t ->
        Connection.update
          t
          ?ordered
          ?bypass_document_validation
          ~upsert
          ~all
          ~selector
          update)

  let update_one t ?upsert ~selector update =
    wrap t ~f:(fun t -> Connection.update_one t ?upsert ~selector update)

  let update_many t ?upsert ~selector update =
    wrap t ~f:(fun t -> Connection.update_many t ?upsert ~selector update)

  let delete ?ordered t ~all selector =
    wrap t ~f:(Connection.delete ?ordered ~all ~selector)

  let delete_one t selector =
    wrap t ~f:(fun t -> Connection.delete_one t selector)

  let delete_many t selector =
    wrap t ~f:(fun t -> Connection.delete_many t selector)

  let find ?skip ?limit ?filter ?projection ?sort t =
    wrap t ~f:(Connection.find ?skip ?limit ?filter ?projection ?sort)

  let find_one ?skip ?filter ?projection t =
    wrap t ~f:(Connection.find_one ?skip ?filter ?projection)

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
    =
    wrap
      t
      ~f:
        (Connection.find_and_modify
           ?bypass_document_validation
           ?query
           ?sort
           ?remove
           ?update
           ?new_
           ?projection
           ?upsert)

  let count ?skip ?limit ?query t =
    wrap t ~f:(Connection.count ?skip ?limit ?query)

  let get_more t ?limit ?max_time_ms ~cursor =
    wrap t ~f:(Connection.get_more ?limit ?max_time_ms ~cursor)

  let kill_cursors t cursors =
    wrap t ~f:(fun t -> Connection.kill_cursors t cursors)

  let get_indexes t = wrap t ~f:Connection.get_indexes

  let ensure_index t ~key ~options =
    wrap t ~f:(Connection.ensure_index ~key ~options)

  let ensure_simple_index ?options t index =
    wrap t ~f:(fun t -> Connection.ensure_simple_index ?options t index)

  let ensure_multi_simple_index ?options t indices =
    wrap t ~f:(fun t -> Connection.ensure_multi_simple_index ?options t indices)

  let drop_index t index = wrap t ~f:(fun t -> Connection.drop_index t index)

  let drop_all_index t = wrap t ~f:Connection.drop_all_index

  let drop_collection t = wrap t ~f:Connection.drop_collection

  let drop_database t = wrap t ~f:Connection.drop_database

  let shutdown t = Client_runtime.shutdown t.runtime

  let is_closed t = Client_runtime.is_closed t.runtime

  module Admin = struct
    module Admin = Connection.Admin

    let of_connection t =
      { t with connection = Admin.of_connection t.connection }

    let command t name = wrap t ~f:(fun t -> Admin.command t name)

    let listDatabases t = wrap t ~f:Admin.listDatabases

    let buildInfo t = wrap t ~f:Admin.buildInfo

    let collStats t = wrap t ~f:Admin.collStats

    let connPoolStats t = wrap t ~f:Admin.connPoolStats

    let cursorInfo t = wrap t ~f:Admin.cursorInfo

    let getCmdLineOpts t = wrap t ~f:Admin.getCmdLineOpts

    let hostInfo t = wrap t ~f:Admin.hostInfo

    let listCommands t = wrap t ~f:Admin.listCommands

    let serverStatus t = wrap t ~f:Admin.serverStatus
  end

  module Auth = struct
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
            failwith
              (Printf.sprintf "split should have at least 2 parts: %s" part))
        []
        parts

    let create_query t bson =
      let t =
        { t with
          connection =
            Connection.with_ t.connection ~database:"admin" ~collection:"$cmd"
        }
      in
      message ~doc:bson t

    let create_sasl_start_message ~mechanism ~payload =
      Bson.empty
      |> Bson.add_element "saslStart" (Bson.create_int32 1l)
      |> Bson.add_element "mechanism" (Bson.create_string mechanism)
      |> Bson.add_element "payload" (Bson.create_user_binary payload)
      |> Bson.add_element "autoAuthorize" (Bson.create_int32 1l)

    let create_sasl_continue_message ~conversation_id ~payload =
      Bson.empty
      |> Bson.add_element "saslContinue" (Bson.create_int32 1l)
      |> Bson.add_element "conversationId" conversation_id
      |> Bson.add_element "payload" (Bson.create_user_binary payload)

    (* https://github.com/mongodb/specifications/blob/master/source/auth/auth.rst#id3 *)
    let auth_plain t username password =
      let open Lwt_result.Infix in
      let bson =
        create_sasl_start_message
          ~mechanism:"PLAIN"
          ~payload:(Printf.sprintf "\x00%s\x00%s" username password)
      in
      create_query t bson >|= fun _r -> ()

    (* https://github.com/mongodb/specifications/blob/master/source/auth/auth.rst#conversation *)
    (* Removed in 4.0 *)
    let auth_mongo_cr t username password =
      let open Lwt_result.Infix in
      let bson =
        Bson.empty |> Bson.add_element "getNonce" (Bson.create_int32 1l)
      in
      create_query t bson >>= fun doc ->
      let nonce = doc |> Bson.get_element "nonce" |> Bson.get_string in
      let password_digest =
        Printf.sprintf "%s:mongo:%s" username password
        |> Digest.string
        |> Digest.to_hex
      in
      let key =
        Printf.sprintf "%s%s%s" nonce username password_digest
        |> Digest.string
        |> Digest.to_hex
      in
      let auth_bson =
        Bson.empty
        |> Bson.add_element "authenticate" (Bson.create_int32 1l)
        |> Bson.add_element "nonce" (Bson.create_string nonce)
        |> Bson.add_element "user" (Bson.create_string username)
        |> Bson.add_element "key" (Bson.create_string key)
      in
      create_query t auth_bson >|= fun _r -> ()

    (* https://github.com/mongodb/specifications/blob/master/source/auth/auth.rst#scram-sha-1 *)

    (*
     * The client Proof:
     * AuthMessage := client-first-message-bare + "," +
        server-first-message + "," + client-final-message-without-proof
     * SaltedPassword := Hi(Normalize(password), salt, i)
     * ClientKey := HMAC(SaltedPassword, "Client Key")
     * ServerKey := HMAC(SaltedPassword, "Server Key")
     * StoredKey := H(ClientKey)
     * ClientSignature := HMAC(StoredKey, AuthMessage)
     * ClientProof := ClientKey XOR ClientSignature
     * ServerSignature := HMAC(ServerKey, AuthMessage) *)
    let auth_scram t mode username password =
      let open Lwt_result.Infix in
      (* TODO: make secure, look into NoCrypto.RNG *)
      let nonce = "abcdef" in
      let first_bare = Printf.sprintf "n=%s,r=%s" username nonce in
      let mechanism, dk_len =
        match mode with
        | `SHA1 ->
          "SCRAM-SHA-1", 20
        | `SHA256 ->
          "SCRAM-SHA-256", 32
        | #Mirage_crypto.Hash.hash ->
          failwith "Not supported"
      in
      let bson =
        create_sasl_start_message
          ~mechanism
          ~payload:(Printf.sprintf "n,,%s" first_bare)
      in
      create_query t bson >>= fun bson_res ->
      Format.eprintf "auth: %s@." (Bson.to_simple_json bson_res);
      let conversation_id = bson_res |> Bson.get_element "conversationId" in
      let res_payload =
        bson_res |> Bson.get_element "payload" |> Bson.get_generic_binary
      in
      let parsed_payload = parse_payload res_payload in
      let iterations = int_of_string (List.assoc "i" parsed_payload) in
      let salt = List.assoc "s" parsed_payload in
      let rnonce = List.assoc "r" parsed_payload in
      let without_proof = Printf.sprintf "c=biws,r=%s" rnonce in
      let password_digest =
        match mode with
        | `SHA1 ->
          Printf.sprintf "%s:mongo:%s" username password
          |> Digest.string
          |> Digest.to_hex
        | `SHA256 ->
          password
        | #Mirage_crypto.Hash.hash ->
          failwith "Not supported"
      in
      let salt = Base64.decode_exn salt |> Cstruct.of_string in
      let salted_password =
        Auth.hi password_digest salt iterations mode (Int32.of_int dk_len)
      in
      let client_key = Auth.hmac salted_password "Client Key" in
      let stored_key = Auth.h mode client_key in
      let auth_message =
        String.concat "," [ first_bare; res_payload; without_proof ]
      in
      let client_signature = Auth.hmac stored_key auth_message in
      let client_proof =
        Printf.sprintf "p=%s" (Auth.xor client_key client_signature)
      in
      let client_final = String.concat "," [ without_proof; client_proof ] in
      let bson =
        create_sasl_continue_message ~conversation_id ~payload:client_final
      in
      create_query t bson >>= fun bson_res ->
      let conversation_id = bson_res |> Bson.get_element "conversationId" in
      let final_bson =
        create_sasl_continue_message ~conversation_id ~payload:""
      in
      create_query t final_bson >|= fun _r -> ()

    let authenticate ~auth t =
      match auth with
      | `NoAuth ->
        Lwt.return_ok ()
      | `Plain (username, password) ->
        auth_plain t username password
      | `MongoCR (username, password) ->
        auth_mongo_cr t username password
      | `SCRAM_SHA_1 (username, password) ->
        auth_scram t `SHA1 username password
      | `SCRAM_SHA_256 (username, password) ->
        auth_scram t `SHA256 username password
  end

  let create_connection ~config ?(auth : Auth.auth = `NoAuth) socket =
    let connection = Connection.create ~config in
    Client_runtime.create
      ~read_buffer_size:0x1000
      ~protocol:(module Connection)
      connection
      socket
    >>= fun runtime ->
    let open Lwt_result.Infix in
    let t = { runtime; connection } in
    Auth.authenticate ~auth t >|= fun () -> t
end
