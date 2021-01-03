type t =
  { db : string
  ; collection : string
  ; host : string
  ; port : int
  ; max_connections : int
  }

let create
    ?(host = "127.0.0.1")
    ?(port = 27017)
    ?(max_connections = 16)
    ~db
    ~collection
    ()
  =
  { db; collection; host; port; max_connections }
