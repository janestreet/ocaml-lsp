open! Core

type t = unit

let create () = Async.return ()
let close () = Async.return (Ok ())
let log_lsp_timing () _ ~category:_ ~message:_ _ _ = ()
let log_merlin_timing () _ ~category:_ ~message:_ _ _ = ()
let log_queue_stats () _ ~action:_ ~realtime_size:_ ~background_size:_ ~total_size:_ = ()
