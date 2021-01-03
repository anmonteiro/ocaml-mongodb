  type t =
    { message_len : int32
    ; request_id : int32
    ; response_to : int32
    ; op : Operation.t
    }

  let create_header body_len request_id response_to op =
    { message_len = Int32.of_int (body_len + (4 * 4))
    ; request_id
    ; response_to
    ; op
    }

  let create_request_header body_len request_id op =
    { message_len = Int32.of_int (body_len + (4 * 4))
    ; request_id
    ; response_to = 0l
    ; op
    }

  let to_string h =
    let buf = Buffer.create 64 in
    Buffer.add_string buf "message_len = ";
    Buffer.add_string buf (Int32.to_string h.message_len);
    Buffer.add_string buf "\n";
    Buffer.add_string buf "request_id = ";
    Buffer.add_string buf (Int32.to_string h.request_id);
    Buffer.add_string buf "\n";
    Buffer.add_string buf "response_to = ";
    Buffer.add_string buf (Int32.to_string h.response_to);
    Buffer.add_string buf "\n";
    Buffer.add_string buf "op = ";
    Buffer.add_string buf (Int32.to_string (Operation.to_code h.op));
    Buffer.add_string buf "\n";
    Buffer.contents buf


