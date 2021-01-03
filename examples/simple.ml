open Lwt.Infix
module Client = Mongo_lwt_unix.Client

type user =
  { name : string }
[@@deriving bson]

let example () =
  let config = Mongo.Config.create ~db:"anm" ~collection:"users" () in
  Lwt_unix.getaddrinfo
    config.Mongo.Config.host
    (string_of_int config.port)
    (* https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml *)
    Unix.[ AI_CANONNAME; AI_PROTOCOL 6; AI_FAMILY PF_INET ]
  >>= fun addresses ->
  let sockaddr = (List.hd addresses).ai_addr in
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.connect fd sockaddr >>= fun () ->
  Mongo_lwt_unix.Client.create_connection
    ~auth:(`Plain ("admin", "password"))
    ~config
    fd
  >>= fun mongo ->
  Format.eprintf "ahoy?@.";
  let docs =
    [ user_to_bson {name="Antonio"} ]
  in
  Client.insert mongo docs;
  Client.find_one mongo >>= fun r ->
  Format.eprintf "foo: %s@." (Mongo.Response.to_string r);
  Lwt.return_unit

let () = Lwt_main.run (example ())
