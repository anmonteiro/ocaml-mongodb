open Mongo

module Client : sig
  include
    Mongo_lwt.Client
      with type socket = Lwt_unix.file_descr
       and type runtime = Gluten_lwt_unix.Client.t

  module TLS : sig
    include
      Mongo_lwt.Client
        with type socket = Gluten_lwt_unix.Client.TLS.socket
         and type runtime = Gluten_lwt_unix.Client.TLS.t

    val create_connection_with_default
      :  config:Config.t
      -> Lwt_unix.file_descr
      -> (t, 'a list) Lwt_result.t
  end

  module SSL : sig
    include
      Mongo_lwt.Client
        with type socket = Gluten_lwt_unix.Client.SSL.socket
         and type runtime = Gluten_lwt_unix.Client.SSL.t

    val create_connection_with_default
      :  config:Config.t
      -> Lwt_unix.file_descr
      -> (t, 'a list) Lwt_result.t
  end
end
