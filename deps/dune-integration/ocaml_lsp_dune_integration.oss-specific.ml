open! Core

module Dune_rpc = struct
  module Progress = struct
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

  module Diagnostic = struct
    module Id = struct
      type t = int [@@deriving sexp, compare]

      let hash = Int.hash
    end

    type t = { id : Id.t }

    let id t = t.id

    module Event = struct
      type nonrec t =
        | Add of t
        | Remove of t
    end
  end

  module Path = struct
    let dune_root = "workspace_root"
  end

  module Build = struct
    module Goal = struct
      module Alias = struct
        type t =
          { dir : string
          ; name : string
          ; promote : bool
          }
      end

      module Path = struct
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

    module Watch = struct
      module Request = struct
        type t =
          { goals : Goal.t list
          ; relative_to : string
          }
      end

      module Outcome = struct
        type t = unit
      end
    end
  end

  module Sub = struct
    type 'a t = unit

    let progress = ()
    let diagnostic = ()

    module With_init = struct
      type ('init, 'a) t = unit
    end

    let build_watch = ()
  end

  module Async = struct
    module Stream = struct
      type 'a t = unit

      let next ~here:_ _ = Async.return None
      let cancel () = Async.return ()
    end

    module Connection = struct
      type t = unit

      module With_connection_result = struct
        type 'a t =
          | Ok of 'a
          | Server_not_running of { build_dir : File_path.Absolute.t }
          | Error of Core.Error.t
      end

      let with_connection ?cleanup_timeout:_ ~f () =
        Async.Deferred.map (f ()) ~f:(fun result -> With_connection_result.Ok result)
      ;;

      let with_connection_or_error ?cleanup_timeout:_ ~f () =
        Async.Deferred.map (f ()) ~f:(fun result -> Ok result)
      ;;

      let poll () () = Async.return (Ok ())
      let poll_with_init () () _ = Async.return (Ok ())
    end
  end
end

module Diagnostic_parser = struct
  module Diagnostic = struct
    module Classification = struct
      type t = unit
    end

    module Severity = struct
      type t =
        | Error
        | Warning
    end

    module Position = struct
      type t =
        { line : int
        ; character : int
        }
    end

    module Range = struct
      type t =
        { start : Position.t
        ; end_ : Position.t
        }
    end

    module Location = struct
      type t =
        { filename : File_path.Absolute.t
        ; range : Range.t
        }
    end

    module Related_information = struct
      type t =
        { location : Location.t
        ; message : string
        }
    end

    module Corrected_file = struct
      type t = unit
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

  let of_dune_diagnostic _ = failwith "Dune diagnostics are not available in the OSS shim"
end
