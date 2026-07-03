open Import

module Resolve : sig
  type t

  val uri : t -> Uri.t

  (** if the completion item doesn't have [data] field, then we don't resolve but return
      it *)
  val of_completion_item : CompletionItem.t -> t option

  include Json.Jsonable.S with type t := t
end

module Complete_by_prefix : sig
  (** Use Merlin to obtain completions and perform some processing to make the result more
      suitable for returning by LSP. As an exception to this, in the case that Merlin
      reports that the [Position.t] is a function application context, do not squash
      function argument names into the completions list, as is normally necessary for
      [textDocument/completion] requests. Instead, return those function argument names as
      the [(string * string) list option]. *)
  val complete_nosquash
    :  log_info:Log_info.t
    -> State.t
    -> Document.Merlin.t
    -> prefix:string
    -> suffix:string
    -> Position.t
    -> deprecated:bool
    -> resolve:bool
    -> (CompletionItem.t list * (string * string) list option) Fiber.t
end

(** Creates a server response for ["textDocument/completion"]. *)
val complete
  :  log_info:Log_info.t
  -> State.t
  -> CompletionParams.t
  -> [> `CompletionList of CompletionList.t ] option Fiber.t

(** Creates a server response for ["completionItem/resolve"]. *)
val resolve
  :  log_info:Log_info.t
  -> Document.Merlin.t
  -> CompletionItem.t
  -> Resolve.t
  -> (Document.Merlin.t -> [> `Logical of int * int ] -> string option Fiber.t)
  -> markdown:bool
  -> CompletionItem.t Fiber.t

(** {v
 [prefix_of_position ~short_path source position] computes prefix before
    given [position]. A prefix is essentially a piece of code that refers to one
    thing eg a single infix operator "|>", a single reference to a function or
    variable: "List.map" a keyword "let" etc If there is semantically irrelivent
    whitespace it is removed eg "List. map"->"List.map"

    @param short_path
      determines whether we want full prefix or cut at ["."], e.g.
      [List.m<cursor>] returns
      - ["List.m"] when [short_path] is [`None]
      - ["m"] when [short_path] is [`Suffix]
      - ["List."] when [short_path] is [`Prefix]

    @return prefix of [position] in [source]
    v} *)
val prefix_of_position
  :  short_path:[ `None | `Prefix | `Suffix ]
  -> Msource.t
  -> [< Msource.position ]
  -> string

(** Similar to [prefix_of_position] but computes a suffix. *)
val suffix_of_position : Msource.t -> [< Msource.position ] -> string

(** [reconstruct_ident source position] returns the identifier at [position]. Note:
    [position] can be in the middle of the identifier.

    @return identifier unless none is found *)
val reconstruct_ident : Msource.t -> [< Msource.position ] -> string option
