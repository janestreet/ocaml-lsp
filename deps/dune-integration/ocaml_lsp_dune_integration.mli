(** Shim that funnels Dune integration dependencies through this library so only this
    library has direct dependencies on [diagnostic_parser] and [dune_rpc] *)

module Dune_rpc : sig
  module Progress : sig
    type t =
      | Waiting
      | In_progress of
          { complete : int
          ; remaining : int
          }
      | Failed
      | Interrupted
      | Success
  end

  module Diagnostic : sig
    type t

    module Id : sig
      type t

      val sexp_of_t : t -> Base.Sexp.t
      val compare : t -> t -> int
      val hash : t -> int
    end

    val id : t -> Id.t

    module Event : sig
      type nonrec t =
        | Add of t
        | Remove of t
    end
  end

  module Path : sig
    val dune_root : string
  end

  module Build : sig
    module Goal : sig
      module Alias : sig
        type t =
          { dir : string
          ; name : string
          ; promote : bool
          }
      end

      module Path : sig
        type t =
          { path : string
          ; promote : bool
          }
      end

      type t =
        | Path of Path.t
        | Directory of Path.t
        | Alias of Alias.t
        | Command_line of string
    end

    module Watch : sig
      module Request : sig
        type t =
          { goals : Goal.t list
          ; relative_to : string
          }
      end

      module Outcome : sig
        type t
      end
    end
  end

  module Sub : sig
    type 'a t

    val progress : Progress.t t
    val diagnostic : Diagnostic.Event.t list t

    module With_init : sig
      type ('init, 'a) t
    end

    val build_watch : (Build.Watch.Request.t, Build.Watch.Outcome.t) With_init.t
  end

  module Async : sig
    module Stream : sig
      type 'a t

      val next : here:Core.Source_code_position.t -> 'a t -> 'a option Async.Deferred.t
      val cancel : 'a t -> unit Async.Deferred.t
    end

    module Connection : sig
      type t

      module With_connection_result : sig
        type 'a t =
          | Ok of 'a
          | Server_not_running of { build_dir : File_path.Absolute.t }
          | Error of Core.Error.t
      end

      val with_connection
        :  ?cleanup_timeout:Core.Time_float.Span.t
        -> f:(t -> 'a Async.Deferred.t)
        -> unit
        -> 'a With_connection_result.t Async.Deferred.t

      val with_connection_or_error
        :  ?cleanup_timeout:Core.Time_float.Span.t
        -> f:(t -> 'a Async.Deferred.t)
        -> unit
        -> 'a Core.Or_error.t Async.Deferred.t

      val poll : t -> 'a Sub.t -> 'a Stream.t Async.Deferred.Or_error.t

      val poll_with_init
        :  t
        -> ('init, 'a) Sub.With_init.t
        -> 'init
        -> 'a Stream.t Async.Deferred.Or_error.t
    end
  end
end

module Diagnostic_parser : sig
  module Diagnostic : sig
    module Classification : sig
      type t
    end

    module Severity : sig
      type t =
        | Error
        | Warning
    end

    module Position : sig
      type t =
        { line : int
        ; character : int
        }
    end

    module Range : sig
      type t =
        { start : Position.t
        ; end_ : Position.t
        }
    end

    module Location : sig
      type t =
        { filename : File_path.Absolute.t
        ; range : Range.t
        }
    end

    module Related_information : sig
      type t =
        { location : Location.t
        ; message : string
        }
    end

    module Corrected_file : sig
      type t
    end

    type t =
      { classification : Classification.t
      ; severity : Severity.t
      ; message : string
      ; location : Location.t
      ; workspace_root : File_path.Absolute.t
      ; related_information : Related_information.t list
      ; corrected_file : Corrected_file.t option
      ; is_fresh : bool
      ; dependency_trace : string list
      ; execution_route : string list
      }
  end

  val of_dune_diagnostic : Dune_rpc.Diagnostic.t -> Diagnostic.t list
end
