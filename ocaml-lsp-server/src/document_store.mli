open Import

type t

type semantic_tokens_cache =
  { resultId : string
  ; tokens : int array
  }

val make : _ Server.t Fdecl.t -> Fiber.Pool.t -> t
val open_document : t -> Document.t -> unit Fiber.t
val change_document : t -> Uri.t -> f:(Document.t -> Document.t) -> Document.t
val get : t -> Uri.t -> Document.t
val get_opt : t -> Uri.t -> Document.t option
val unregister_promotions : t -> Uri.t list -> unit Fiber.t
val register_promotions : t -> Uri.t list -> unit Fiber.t

val update_semantic_tokens_cache
  :  t
  -> Uri.t
  -> resultId:string
  -> tokens:int array
  -> unit

val get_semantic_tokens_cache : t -> Uri.t -> semantic_tokens_cache option
val close_document : t -> Uri.t -> unit Fiber.t
val fold : t -> init:'acc -> f:(Document.t -> 'acc -> 'acc) -> 'acc

val docs_to_iter
  :  ?compare:(Document.t -> Document.t -> int)
  -> ?filter:(Document.t -> bool)
  -> t
  -> Document.t list

val parallel_iter
  :  ?filter:(Document.t -> bool)
  -> t
  -> f:(Document.t -> unit Fiber.t)
  -> unit Fiber.t

(** The optional argument [compare] is used to determine the order in which documents are
    iterated through. *)
val sequential_iter
  :  ?compare:(Document.t -> Document.t -> int)
  -> ?filter:(Document.t -> bool)
  -> t
  -> f:(Document.t -> unit Fiber.t)
  -> unit Fiber.t

val close_all : t -> unit Fiber.t
val last_used : t -> Uri.t -> Core.Time_ns.t option

(** Updates the [last_used] timestamp for the specified documents to the current time. *)
val update_last_used : t -> Uri.t -> unit
