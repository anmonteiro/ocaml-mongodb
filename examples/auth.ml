open Lwt.Infix

let print_alert where alert =
  Printf.eprintf "TLS ALERT (%s): %s\n%!"
    where (Tls.Packet.alert_type_to_string alert)

let print_fail where fail =
  Printf.eprintf "TLS FAIL (%s): %s\n%!"
    where (Tls.Engine.string_of_failure fail)

let scram_256 () =
  let port = 10255 in
  let host = "34073bd1-0ee0-4-231-b9ee.documents.azure.com" in
  let username = "34073bd1-0ee0-4-231-b9ee" in
  let password = "jzgJ9A0ea1KWvw5LeEkkG57FalMBk8aXFsaxgEGxC261yCnYAlEErWmFgmlOyDkhnmDsnhHK9MvnX0ctbkbdzw==" in
  Mongo_lwt.create ~ssl:true ~auth:(`SCRAM_SHA_1 (username, password)) host port "anm" "users" >>= fun mongo ->
  let docs = [
    Bson.empty
    |> Bson.add_element "name" (Bson.create_string "Antonio")
  ]
  in
  Mongo_lwt.insert mongo docs >>= fun () ->
  Mongo_lwt.find_one mongo >>= fun r ->
  Printf.eprintf "foo: %s\n%!" (MongoReply.to_string r);
  Lwt.return_unit

let () =
  try
    Lwt_main.run (scram_256 ())
  with
  | Tls_lwt.Tls_alert alert as exn ->
    print_alert "remote end" alert ; raise exn
  | Tls_lwt.Tls_failure alert as exn ->
    print_fail "our end" alert ; raise exn
