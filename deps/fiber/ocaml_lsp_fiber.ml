include Core
module Cancel = Cancel
module Pool = Pool
module Stream = Stream
module Mvar = Mvar
module Svar = Svar
module Throttle = Throttle
module Mutex = Mutex
module Scheduler = Scheduler
module Lazy = Lazy

(* Jane Street: [ppx_let] support ([let%map.Fiber], [match%bind.Fiber], ...). The opam
   [fiber] package this vendored copy replaces provided [Let_syntax]; the upstream sources
   do not, so we add it here. This is shared by the [dune] and [jbuild] builds. *)
module Let_syntax = struct
  module Let_syntax = struct
    let return = return
    let bind = bind
    let map = map
    let both = both
  end
end

let run =
  let rec loop ~iter (s : _ Scheduler.step) =
    match s with
    | Done a -> a
    | Stalled w -> loop ~iter (Scheduler.advance w (iter ()))
  in
  fun t ~iter -> loop ~iter (Scheduler.start t)
;;

type fill = Scheduler.fill = Fill : 'a ivar * 'a -> fill

module Expert = struct
  type nonrec 'a k = 'a k

  let suspend f k = suspend f k
  let resume a x k = resume a x k
end
