open! Core

type t =
  { index : int
  ; action : string
  ; feature : string option
  ; file : string option
  ; other_files : string list option
  ; lines : int option
  ; hash : int option
  ; pos_line : int option
  ; pos_char : int option
  ; enqueue_time : Core.Time_ns.t
  ; request_time : Core.Time_ns.t option
  }

let create
  ~index
  ~action
  ?log_paths_and_features:_
  ?primary_uri:_
  ?other_uris:_
  ?text
  ?position
  ?request_time
  ()
  =
  let lines, hash =
    match text with
    | None -> None, None
    | Some text -> Some (String.count ~f:(Char.equal '\n') text), Some (String.hash text)
  in
  let pos_line, pos_char =
    match position with
    | None -> None, None
    | Some (position : Lsp.Types.Position.t) ->
      Some position.line, Some position.character
  in
  { index
  ; action
  ; feature = None
  ; file = None
  ; other_files = None
  ; lines
  ; hash
  ; pos_line
  ; pos_char
  ; enqueue_time = Time_ns.now ()
  ; request_time
  }
;;

let update_action ~suffix t = { t with action = t.action ^ suffix }
