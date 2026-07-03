open Import

type init =
  | Uninitialized
  | Initialized of
      { params : InitializeParams.t
      ; workspaces : Workspaces.t
      ; exp_client_caps : Client.Experimental_capabilities.t
      ; diagnostics : Diagnostics.t
      ; dune_subscriptions : Dune_subscriptions.t
      ; manage_dune_connection : Fiber.Cancel.t
      ; position_encoding : [ `UTF16 | `UTF8 ]
      }

(** State specific to the hoverExtended request. *)
type hover_extended =
  { mutable history : (Uri.t * Position.t * int) option
  (** File, position, and verbosity level of the last call to hoverExtended. This value is
      used to pick a verbosity level when it is not specific by the client. *)
  }

type t =
  { store : Document_store.t
  ; merlin : Document.Single_pipeline.t
  ; merlin_config : Merlin_config.DB.t
  ; init : init
  ; detached : Fiber.Pool.t
  ; configuration : Configuration.t
  ; trace : TraceValue.t
  ; ocamlformat_rpc : Ocamlformat_rpc.t
  ; symbols_thread : Lev_fiber.Thread.t Lazy_fiber.t
  ; wheel : Lev_fiber.Timer.Wheel.t
  ; hover_extended : hover_extended
  ; feature_id : Feature_id.t option
  ; mutable remote_lsp_available : bool
  ; mutable event_index : int
  (** Local monotonic counter used for logging event indices when no [event_index] is
      injected by [ocaml-lsp-wrapper]. Also used for internal events such as build-driven
      diagnostic refreshes. *)
  ; mutable logging_session : Ocaml_lsp_logging.t option
  ; worker_name : string option
  (** Logical name of this ocaml-lsp worker, as passed via [-worker-name]. Included in
      logs and threaded into [Ocaml_lsp_logging.init] so that every log management entry
      and every structured logging row can be attributed to the worker that emitted it. *)
  }

val create
  :  store:Document_store.t
  -> merlin:Priority_lsp_executor.t
  -> detached:Fiber.Pool.t
  -> configuration:Configuration.t
  -> ocamlformat_rpc:Ocamlformat_rpc.t
  -> symbols_thread:Lev_fiber.Thread.t Lazy_fiber.t
  -> wheel:Lev_fiber.Timer.Wheel.t
  -> worker_name:string option
  -> t

val position_encoding : t -> [ `UTF16 | `UTF8 ]
val wheel : t -> Lev_fiber.Timer.Wheel.t

(** Updates [t.event_index] to [event_index], if provided (by [ocaml-lsp-wrapper]),
    otherwise to [t.event_index + 1], and returns it. *)
val decide_event_index : t -> event_index:int option -> int

val initialize_params : t -> InitializeParams.t

val initialize
  :  t
  -> position_encoding:[ `UTF16 | `UTF8 ]
  -> dune_subscriptions:Dune_subscriptions.t
  -> manage_dune_connection:Fiber.Cancel.t
  -> InitializeParams.t
  -> Workspaces.t
  -> Diagnostics.t
  -> t

val workspace_root : t -> Uri.t
val workspaces : t -> Workspaces.t
val modify_workspaces : t -> f:(Workspaces.t -> Workspaces.t) -> t

(** @return
      client capabilities passed from the client in [InitializeParams]; use
      [exp_client_caps] to get {i experimental} client capabilities.
    @raise Assertion_failure if the [t.init] is [Uninitialized] *)
val client_capabilities : t -> ClientCapabilities.t

(** @return experimental client capabilities *)
val experimental_client_capabilities : t -> Client.Experimental_capabilities.t

val diagnostics : t -> Diagnostics.t
val dune_subscriptions : t -> Dune_subscriptions.t option
val log_msg : t Server.t -> type_:MessageType.t -> message:string -> unit Fiber.t

(** @return (name,version) of the editor that initialized this LSP instance. *)
val get_editor : t -> string * string

val editor_is_vscode : t -> bool

(** Should we send fallback queries to the remote-lsp? *)
val should_fall_back : t -> bool

(** In which deployment stage this ocaml-lsp is running *)
val stage : t -> Stage.t
