module Fiber = Ocaml_lsp_fiber

let map_reduce_errors_first f ~on_error =
  let module Monoid = struct
    type t = unit

    let empty = ()
    let combine () () = ()
  end
  in
  Fiber.map_reduce_errors (module Monoid) f ~on_error
;;

let map_reduce_errors_with_monoid
  (type t)
  (module M : Ocaml_lsp_stdune.Monoid with type t = t)
  f
  ~on_error
  =
  Fiber.map_reduce_errors (module M) f ~on_error
;;

let collect_errors = Fiber.collect_errors
let with_error_handler = Fiber.with_error_handler
let close_fiber_pool t = Fiber.Pool.close t

module Var_optional = struct
  type 'a t = 'a option Fiber.Var.t

  let create_none () = Fiber.Var.create ()
  let get t = Fiber.map (Fiber.Var.get t) ~f:Option.join
  let set fv v f = Fiber.Var.set fv v f
end

module Mvar = struct
  type 'a t = 'a Fiber.Mvar.t

  let create = Fiber.Mvar.create
  let take = Fiber.Mvar.read
  let put = Fiber.Mvar.write
end

module Fiber_async = struct
  open Core
  open Async

  module Fiber = struct
    include Fiber

    include Monad.Make (struct
        include Fiber

        let map = `Custom map
      end)
  end

  (* Fiber-local storage. [Univ_map.Key] behind the scenes. *)
  let key = Var_optional.create_none ()

  (* This solution is adapted from the [Fiber_lwt] module in Dune. When the fiber
     scheduler reaches a [Fiber.Ivar.read] for an unfilled [Fiber.Ivar.t], it stalls, and
     when resumed it must be give a [Fiber.Fill (ivar, value)] for that ivar so it can
     enqueue jobs waiting on its result. If the [Fiber.Ivar.t] is filled but the fill is
     not communicated to the scheduler, the scheduler will not know the jobs waiting on it
     are now ready to run. Therefore, when we create a [Fiber.Ivar.t] inside
     [fiber_of_deferred] to hold the value when the deferred is filled, we also need some
     way to communicate with the fiber's scheduler. We do this via a pipe that the
     scheduler provides in fiber-local storage, but for that to work the fiber's scheduler
     must be the one produced by the complementary [deferred_of_fiber]. *)

  let fiber_of_deferred (type a) (deferred : a Deferred.t) : a Fiber.t =
    let ivar = Fiber.Ivar.create () in
    match%bind.Fiber Var_optional.get key with
    | None -> failwith "[fiber_of_deferred] invoked outside of [deferred_of_fiber]"
    | Some fill ->
      upon deferred (fun value -> fill (Fiber.Fill (ivar, value)));
      Fiber.Ivar.read ivar
  ;;

  let deferred_of_fiber fiber () =
    let reader, writer = Pipe.create () in
    let fiber =
      Var_optional.set key (Some (Pipe.write_without_pushback writer)) (fun () -> fiber)
    in
    let rec loop = function
      | Fiber.Scheduler.Done x -> return x
      | Fiber.Scheduler.Stalled s ->
        let%bind fill = Pipe.read_exn reader in
        loop (Fiber.Scheduler.advance s [ fill ])
    in
    let step = Fiber.Scheduler.start fiber in
    loop step
  ;;
end
