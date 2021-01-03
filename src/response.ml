type t =
  { header : Header.t
  ; response_flags : int32
  ; cursor_id : int64
  ; starting_from : int32
  ; num_returned : int32
  ; document_list : Bson.t list
  }

let get_header r = r.header

let get_response_flags r = r.response_flags

let get_cursor r = r.cursor_id

let get_starting_from r = r.starting_from

let get_num_returned r = r.num_returned

let get_document_list r = r.document_list

let to_string r =
  let buf = Buffer.create 128 in
  Buffer.add_string buf (Header.to_string r.header);
  Buffer.add_string buf "response_flags = ";
  Buffer.add_string buf (Int32.to_string r.response_flags);
  Buffer.add_string buf "\n";
  Buffer.add_string buf "cursor_id = ";
  Buffer.add_string buf (Int64.to_string r.cursor_id);
  Buffer.add_string buf "\n";
  Buffer.add_string buf "starting_from = ";
  Buffer.add_string buf (Int32.to_string r.starting_from);
  Buffer.add_string buf "\n";
  Buffer.add_string buf "num_returned = ";
  Buffer.add_string buf (Int32.to_string r.num_returned);
  Buffer.add_string buf "\n";
  Buffer.add_string buf "doc list: ";
  Buffer.add_string buf "\n";
  let rec process_doc_list = function
    | [] ->
      ()
    | hd :: tl ->
      Buffer.add_string buf (Bson.to_simple_json hd);
      Buffer.add_string buf "\n";
      process_doc_list tl
  in
  process_doc_list r.document_list;
  Buffer.contents buf

let pp_hum fmt t = Format.fprintf fmt "%s" (to_string t)
