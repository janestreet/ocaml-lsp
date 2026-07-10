module Fiber = Ocaml_lsp_fiber
module Exn_with_backtrace = Ocaml_lsp_stdune.Exn_with_backtrace

let print_sexp sexp = Format.printf "%s@." (Base.Sexp.to_string_hum sexp)

module Scheduler : sig
  type t

  exception Never

  val create : unit -> t
  val run : t -> 'a Fiber.t -> 'a
end = struct
  type t = unit Fiber.Ivar.t Base.Queue.t

  let t_var = Ocaml_lsp_fiber_shims.Var_optional.create_none ()
  let create () = Base.Queue.create ()

  exception Never

  let run t fiber =
    let fiber = Ocaml_lsp_fiber_shims.Var_optional.set t_var (Some t) (fun () -> fiber) in
    Fiber.run fiber ~iter:(fun () ->
      let next =
        match Base.Queue.dequeue t with
        | None -> raise Never
        | Some e -> Fiber.Fill (e, ())
      in
      [ next ])
  ;;
end

let test ?(expect_never = false) sexp_of_result f =
  let never_raised = ref false in
  let f =
    let on_error exn =
      Format.eprintf "%a@." Exn_with_backtrace.pp_uncaught exn;
      Exn_with_backtrace.reraise exn
    in
    Ocaml_lsp_fiber_shims.with_error_handler f ~on_error
  in
  (try Scheduler.run (Scheduler.create ()) f |> sexp_of_result |> print_sexp with
   | Scheduler.Never -> never_raised := true);
  match !never_raised, expect_never with
  | false, false ->
    (* We don't raise in this case b/c we assume something else is being tested *)
    ()
  | true, true -> print_endline "[PASS] Never raised as expected"
  | false, true -> print_endline "[FAIL] expected Never to be raised but it wasn't"
  | true, false -> print_endline "[FAIL] unexpected Never raised"
;;
