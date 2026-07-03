module Fiber = Ocaml_lsp_fiber
module Code_error = Ocaml_lsp_stdune.Code_error
module Exn_with_backtrace = Ocaml_lsp_stdune.Exn_with_backtrace
module Fdecl = Ocaml_lsp_stdune.Fdecl
module Comparable = Core.Comparable
module Hashtbl = Core.Hashtbl
module Int = Core.Int
module Map = Core.Map
module Pid = Core.Pid
module Poly = Core.Poly
module Tuple = Core.Tuple
module Sexp = Core.Sexp

let sprintf = Printf.sprintf

include struct
  open Base
  module Queue = Queue

  module Array = struct
    include Base.Array

    let common_prefix_len ~equal (a : 'a array) (b : 'a array) : int =
      let i = ref 0 in
      let min_len = min (Array.length a) (Array.length b) in
      while !i < min_len && equal (Array.get a !i) (Array.get b !i) do
        Int.incr i
      done;
      !i
    ;;
  end
end

module List = Base.List

module Result = struct
  module O = struct
    let ( let+ ) x f = Base.Result.map x ~f
    let ( let* ) x f = Base.Result.bind x ~f
  end

  include Base.Result
end

module Option = struct
  module O = struct
    let ( let+ ) x f = Base.Option.map x ~f
    let ( let* ) x f = Base.Option.bind x ~f
  end

  include Base.Option
end

module String = struct
  include Base.String

  let capitalize_ascii = Base.String.capitalize
  let drop_prefix_if_exists = Base.String.chop_prefix_if_exists
  let lowercase_ascii = Base.String.lowercase
  let starts_with ~prefix s = Base.String.is_prefix s ~prefix
  let strip = strip
  let trim = Base.String.strip

  module Map = Core.String.Map
  module Table = Core.String.Table

  let findi =
    let rec loop s len ~f i =
      if Int.(i >= len)
      then None
      else if f (String.unsafe_get s i)
      then Some i
      else loop s len ~f (i + 1)
    in
    fun ?from s ~f ->
      let len = String.length s in
      let from =
        match from with
        | None -> 0
        | Some i ->
          if Int.(i > len - 1)
          then Code_error.raise_s [%message "findi: invalid from"]
          else i
      in
      loop s len ~f from
  ;;

  let rfindi =
    let rec loop s ~f i =
      if Int.(i < 0)
      then None
      else if f (String.unsafe_get s i)
      then Some i
      else loop s ~f (i - 1)
    in
    fun ?from s ~f ->
      let from =
        let len = String.length s in
        match from with
        | None -> len - 1
        | Some i ->
          if Int.(i > len - 1)
          then Code_error.raise_s [%message "rfindi: invalid from"]
          else i
      in
      loop s ~f from
  ;;
end

(* All modules from [Lsp] should be in the struct below. The modules are listed
   alphabetically. Try to keep the order. *)
include struct
  open Lsp
  module Client_notification = Client_notification
  module Client_request = Client_request
  module Server_request = Server_request
  module Text_document = Text_document

  module Uri = struct
    module T = struct
      include Uri

      let to_dyn t = Dyn.string (to_string t)
    end

    include T
    module Table = Core.Hashtbl.Make_plain (T)
  end
end

(* Misc modules *)
module Drpc = Ocaml_lsp_dune_integration.Dune_rpc
module Feature_id = Ocaml_lsp_remote_lsp.Feature_id
module Fiber_async = Ocaml_lsp_fiber_shims.Fiber_async
module File_path = File_path

(* OCaml frontend *)
module Ast_iterator = Ocaml_parsing.Ast_iterator
module Ast_helper = Ocaml_parsing.Ast_helper
module Ast_mapper = Ocaml_parsing.Ast_mapper
module Asttypes = Ocaml_parsing.Asttypes
module Cmt_format = Ocaml_typing.Cmt_format
module Ident = Ocaml_typing.Ident
module Env = Ocaml_typing.Env
module Merlin_parsing = Ocaml_parsing

module Loc = struct
  module T = struct
    include Ocaml_parsing.Location
    include Ocaml_parsing.Location_aux
  end

  include T

  module Map = Map.Make_plain (struct
      include T

      type nonrec t = Ocaml_parsing.Location.t = private
        { loc_start : Core.Source_code_position.t
        ; loc_end : Core.Source_code_position.t
        ; loc_ghost : Core.Bool.t
        }
      [@@deriving sexp_of]
    end)
end

include struct
  open Ocaml_parsing
  module Longident = Longident
  module Parsetree = Parsetree
  module Pprintast = Pprintast
end

include struct
  open Ocaml_typing
  module Path = Path
  module Typedtree = Typedtree
  module Types = Types
end

include struct
  open Merlin_kernel
  module Mconfig = Mconfig
  module Mconfig_dot = Mconfig_dot
  module Msource = Msource
  module Mbrowse = Mbrowse
  module Mpipeline = Mpipeline
  module Mreader = Mreader
  module Mtyper = Mtyper
end

module Warnings = Ocaml_utils.Warnings
module Browse_raw = Merlin_specific.Browse_raw
module Format = Merlin_utils.Std.Format

(* All modules from [Lsp_fiber] should be in the struct below. The modules are listed
   alphabetically. Try to keep the order. *)
include struct
  open Lsp_fiber
  module Log = Private.Log
  module Notify = Rpc.Notify
  module Reply = Rpc.Reply
  module Server = Server
  module Lazy_fiber = Lsp_fiber.Lazy_fiber
  module Json = Json
end

include struct
  open Priority_lsp_executor_lib
  module Priority_lsp_executor = Priority_lsp_executor
  module Priority = Priority
end

(* All modules from [Lsp.Types] should be in the struct below. The modules are listed
   alphabetically. Try to keep the order. *)
include struct
  open Lsp.Types

  module ClientCapabilities = struct
    include ClientCapabilities

    let markdown_support (client_capabilities : ClientCapabilities.t) ~field =
      match client_capabilities.textDocument with
      | None -> false
      | Some td ->
        (match field td with
         | None -> false
         | Some format ->
           let set = Option.value format ~default:[ MarkupKind.Markdown ] in
           List.mem set MarkupKind.Markdown ~equal:Poly.equal)
    ;;
  end

  module CallHierarchyIncomingCall = CallHierarchyIncomingCall
  module CallHierarchyIncomingCallsParams = CallHierarchyIncomingCallsParams
  module CallHierarchyItem = CallHierarchyItem
  module CallHierarchyOutgoingCallsParams = CallHierarchyOutgoingCallsParams
  module CallHierarchyOutgoingCall = CallHierarchyOutgoingCall
  module CallHierarchyPrepareParams = CallHierarchyPrepareParams
  module CodeAction = CodeAction
  module CodeActionKind = CodeActionKind
  module CodeActionOptions = CodeActionOptions
  module CodeActionParams = CodeActionParams
  module CodeActionResult = CodeActionResult
  module CodeActionRegistrationOptions = CodeActionRegistrationOptions
  module CodeLens = CodeLens
  module CodeLensOptions = CodeLensOptions
  module CodeLensParams = CodeLensParams
  module Command = Command
  module CompletionItem = CompletionItem
  module CompletionItemKind = CompletionItemKind
  module CompletionList = CompletionList
  module CompletionOptions = CompletionOptions
  module CompletionParams = CompletionParams
  module ConfigurationParams = ConfigurationParams
  module CreateFile = CreateFile
  module Diagnostic = Diagnostic
  module DiagnosticRelatedInformation = DiagnosticRelatedInformation
  module DiagnosticSeverity = DiagnosticSeverity
  module DiagnosticTag = DiagnosticTag
  module DidChangeConfigurationParams = DidChangeConfigurationParams
  module DidChangeWorkspaceFoldersParams = DidChangeWorkspaceFoldersParams
  module DidOpenTextDocumentParams = DidOpenTextDocumentParams
  module Diff = Lsp.Diff
  module DocumentFilter = DocumentFilter
  module DocumentHighlight = DocumentHighlight
  module DocumentHighlightKind = DocumentHighlightKind
  module DocumentHighlightParams = DocumentHighlightParams
  module DocumentSymbol = DocumentSymbol
  module DocumentUri = DocumentUri
  module ExecuteCommandOptions = ExecuteCommandOptions
  module ExecuteCommandParams = ExecuteCommandParams
  module FoldingRange = FoldingRange
  module FoldingRangeParams = FoldingRangeParams
  module Hover = Hover
  module HoverParams = HoverParams
  module InlayHint = InlayHint
  module InlayHintKind = InlayHintKind
  module InlayHintParams = InlayHintParams
  module InitializeParams = InitializeParams
  module InitializeResult = InitializeResult
  module Location = Location
  module LogMessageParams = LogMessageParams
  module MarkupContent = MarkupContent
  module MarkupKind = MarkupKind
  module MessageType = MessageType
  module OptionalVersionedTextDocumentIdentifier = OptionalVersionedTextDocumentIdentifier
  module ParameterInformation = ParameterInformation
  module PositionEncodingKind = PositionEncodingKind
  module PrepareRenameParams = PrepareRenameParams
  module ProgressParams = ProgressParams
  module ProgressToken = ProgressToken
  module PublishDiagnosticsParams = PublishDiagnosticsParams
  module PublishDiagnosticsClientCapabilities = PublishDiagnosticsClientCapabilities
  module ReferenceParams = ReferenceParams
  module Registration = Registration
  module RegistrationParams = RegistrationParams
  module RenameOptions = RenameOptions
  module RenameParams = RenameParams
  module SaveOptions = SaveOptions
  module SelectionRange = SelectionRange
  module SelectionRangeParams = SelectionRangeParams
  module SemanticTokens = SemanticTokens
  module SemanticTokensEdit = SemanticTokensEdit
  module SemanticTokensLegend = SemanticTokensLegend
  module SemanticTokensDelta = SemanticTokensDelta
  module SemanticTokensDeltaParams = SemanticTokensDeltaParams
  module SemanticTokenModifiers = SemanticTokenModifiers
  module SemanticTokensOptions = SemanticTokensOptions
  module SemanticTokensParams = SemanticTokensParams
  module SemanticTokenTypes = SemanticTokenTypes
  module ServerCapabilities = ServerCapabilities
  module Server_notification = Lsp.Server_notification
  module SetTraceParams = SetTraceParams
  module ShowDocumentClientCapabilities = ShowDocumentClientCapabilities
  module ShowDocumentParams = ShowDocumentParams
  module ShowDocumentResult = ShowDocumentResult
  module ShowMessageParams = ShowMessageParams
  module SignatureHelp = SignatureHelp
  module SignatureHelpOptions = SignatureHelpOptions
  module SignatureHelpParams = SignatureHelpParams
  module SignatureInformation = SignatureInformation
  module SymbolInformation = SymbolInformation
  module SymbolKind = SymbolKind
  module TextDocumentClientCapabilities = TextDocumentClientCapabilities
  module TextDocumentContentChangeEvent = TextDocumentContentChangeEvent
  module TextDocumentEdit = TextDocumentEdit
  module TextDocumentFilter = TextDocumentFilter
  module TextDocumentIdentifier = TextDocumentIdentifier
  module TextDocumentItem = TextDocumentItem
  module TextDocumentRegistrationOptions = TextDocumentRegistrationOptions
  module TextDocumentSyncKind = TextDocumentSyncKind
  module TextDocumentSyncOptions = TextDocumentSyncOptions
  module TextDocumentSyncClientCapabilities = TextDocumentSyncClientCapabilities
  module TextEdit = TextEdit

  (** deprecated *)
  module TraceValue = TraceValues

  module TraceValues = TraceValues
  module Unregistration = Unregistration
  module UnregistrationParams = UnregistrationParams
  module VersionedTextDocumentIdentifier = VersionedTextDocumentIdentifier
  module WorkDoneProgressBegin = WorkDoneProgressBegin
  module WorkDoneProgressCreateParams = WorkDoneProgressCreateParams
  module WorkDoneProgressEnd = WorkDoneProgressEnd
  module WorkDoneProgressReport = WorkDoneProgressReport
  module WorkspaceEdit = WorkspaceEdit
  module WorkspaceFolder = WorkspaceFolder
  module WorkspaceFoldersChangeEvent = WorkspaceFoldersChangeEvent
  module WorkspaceSymbolParams = WorkspaceSymbolParams
  module WorkspaceFoldersServerCapabilities = WorkspaceFoldersServerCapabilities
end

module Log_info = Ocaml_lsp_logging.Log_info
module Css_lsp = Ocaml_lsp_css_lsp
module Metrics = Ocaml_lsp_metrics
module Version = Ocaml_lsp_version

let task_if_running pool ~f =
  let open Fiber.O in
  let* running = Fiber.Pool.running pool in
  match running with
  | false -> Fiber.return ()
  | true -> Fiber.Pool.task pool ~f
;;

(* We implement fold over Fiber computations ourselves as Fiber does not expose modules
   like [Async.Deferred.List]. *)
let rec fold_left_fiber ~init ~f l =
  let open Fiber.O in
  match l with
  | [] -> Fiber.return init
  | x :: xs ->
    let* init = f init x in
    fold_left_fiber ~init ~f xs
;;

let inside_test = Env_vars._TEST () |> Option.value ~default:false
