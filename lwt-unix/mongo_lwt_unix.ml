open Lwt.Infix

module Client = struct
  include Mongo_lwt.Client (Gluten_lwt_unix.Client)

  module TLS = struct
    include Mongo_lwt.Client (Gluten_lwt_unix.Client.TLS)

    let create_connection_with_default ~config socket =
      Gluten_lwt_unix.Client.TLS.create_default socket >>= fun tls_client ->
      create_connection ~config tls_client
  end

  module SSL = struct
    include Mongo_lwt.Client (Gluten_lwt_unix.Client.SSL)

    let create_connection_with_default ~config socket =
      Gluten_lwt_unix.Client.SSL.create_default socket >>= fun ssl_client ->
      create_connection ~config ssl_client
  end
end
