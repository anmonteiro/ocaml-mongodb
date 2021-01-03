module Bde = Bson.Parse
module MongoHeader = Header

module Header = struct
  let decode =
    let open Bson.Parse in
    Angstrom.lift4
      (fun message_len request_id response_to op_code ->
        { Header.message_len
        ; request_id
        ; response_to
        ; op = Operation.of_code op_code
        })
      decode_int32
      decode_int32
      decode_int32
      decode_int32
end

let decode_reply_doc n =
  let open Angstrom in
  let rec inner n acc =
    match n with
    | 0 ->
      return (List.rev acc)
    | _ ->
      Bde.decode_doc >>= fun doc -> inner (n - 1) (doc :: acc)
  in
  inner n []

let decode_reply =
  let open Angstrom in
  Header.decode >>= fun header ->
  match header.op with
  | OP_REPLY ->
    Bde.decode_int32 >>= fun flags ->
    Bde.decode_int64 >>= fun cursor ->
    Bde.decode_int32 >>= fun from ->
    Bde.decode_int32 >>= fun returned ->
    lift
      (fun doc_list ->
        { Response.header
        ; response_flags = flags
        ; cursor_id = cursor
        ; starting_from = from
        ; num_returned = returned
        ; document_list = doc_list
        })
      (decode_reply_doc (Int32.to_int returned))
  | OP_MSG ->
    Bde.decode_int32 >>= fun _flag_bits ->
    Angstrom.any_char >>= fun kind ->
    (match kind with
    | '\x00' ->
      lift
        (fun document_list ->
          { Response.header
          ; response_flags = 0l
          ; cursor_id = 0L
          ; starting_from = 0l
          ; num_returned = 1l
          ; document_list
          })
      (decode_reply_doc 1)
    | '\x01' | _ ->
      assert false)
  | _ ->
    assert false

module Reader = struct
  module AU = Angstrom.Unbuffered

  type error = [ `Parse of string list * string ]

  type 'error parse_state =
    | Done
    | Fail of 'error
    | Partial of
        (Bigstringaf.t
         -> off:int
         -> len:int
         -> AU.more
         -> (unit, 'error) result AU.state)

  type 'error t =
    { parser : (unit, 'error) result Angstrom.t
    ; mutable parse_state : 'error parse_state
          (* The state of the parse for the current request *)
    ; mutable closed : bool
          (* Whether the input source has left the building, indicating that no
           * further input will be received. *)
    ; mutable wakeup : Optional_thunk.t
    }

  type reply = error t

  let create parser =
    { parser; parse_state = Done; closed = false; wakeup = Optional_thunk.none }

  let is_closed t = t.closed

  let on_wakeup t k =
    if is_closed t then
      failwith "on_wakeup on closed reader"
    else if Optional_thunk.is_some t.wakeup then
      failwith "on_wakeup: only one callback can be registered at a time"
    else
      t.wakeup <- Optional_thunk.some k

  let wakeup t =
    let f = t.wakeup in
    t.wakeup <- Optional_thunk.none;
    Optional_thunk.call_if_some f

  let reply handler =
    let open Angstrom in
    let parser =
      skip_many (decode_reply <* commit >>| handler) >>| fun () -> Ok ()
    in
    create parser

  let transition t state =
    match state with
    | AU.Done (consumed, Ok ()) ->
      t.parse_state <- Done;
      consumed
    | AU.Done (consumed, Error error) ->
      t.parse_state <- Fail error;
      consumed
    | AU.Fail (consumed, marks, msg) ->
      t.parse_state <- Fail (`Parse (marks, msg));
      consumed
    | AU.Partial { committed; continue } ->
      t.parse_state <- Partial continue;
      committed

  and start t state =
    match state with
    | AU.Done _ ->
      failwith "httpaf.Parse.unable to start parser"
    | AU.Fail (0, marks, msg) ->
      t.parse_state <- Fail (`Parse (marks, msg))
    | AU.Partial { committed = 0; continue } ->
      t.parse_state <- Partial continue
    | _ ->
      assert false

  let rec read_with_more t bs ~off ~len more =
    let initial = match t.parse_state with Done -> true | _ -> false in
    let consumed =
      match t.parse_state with
      | Fail _ ->
        0
      | Done ->
        start t (AU.parse t.parser);
        read_with_more t bs ~off ~len more
      | Partial continue ->
        transition t (continue bs more ~off ~len)
    in
    (* Special case where the parser just started and was fed a zero-length
     * bigstring. Avoid putting them parser in an error state in this scenario.
     * If we were already in a `Partial` state, return the error. *)
    if initial && len = 0 then t.parse_state <- Done;
    match more with
    | Complete ->
      t.closed <- true;
      consumed
    | Incomplete ->
      (match t.parse_state with
      | Done when consumed < len ->
        let off = off + consumed
        and len = len - consumed in
        consumed + read_with_more t bs ~off ~len more
      | _ ->
        consumed)

  let force_close t = t.closed <- true

  let next t =
    match t.parse_state with
    | Fail failure ->
      `Error failure
    | _ when t.closed ->
      `Close
    | Done | Partial _ ->
      `Read
end
