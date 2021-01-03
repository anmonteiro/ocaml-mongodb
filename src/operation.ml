exception Unknown_op_code

(* TODO(anmonteiro): add support for OP_MSG (mongo 4.2) *)
type t =
  | OP_REPLY
  | OP_UPDATE
  | OP_INSERT
  | RESERVED
  | OP_QUERY
  | OP_GET_MORE
  | OP_DELETE
  | OP_KILL_CURSORS
  | OP_MSG

let to_code = function
  | OP_REPLY ->
    1l
  | OP_UPDATE ->
    2001l
  | OP_INSERT ->
    2002l
  | RESERVED ->
    2003l
  | OP_QUERY ->
    2004l
  | OP_GET_MORE ->
    2005l
  | OP_DELETE ->
    2006l
  | OP_KILL_CURSORS ->
    2007l
  | OP_MSG ->
    2013l

let of_code = function
  | 1l ->
    OP_REPLY
  | 2001l ->
    OP_UPDATE
  | 2002l ->
    OP_INSERT
  | 2003l ->
    RESERVED
  | 2004l ->
    OP_QUERY
  | 2005l ->
    OP_GET_MORE
  | 2006l ->
    OP_DELETE
  | 2007l ->
    OP_KILL_CURSORS
  | 2013l ->
    OP_MSG
  | _ ->
    raise Unknown_op_code
