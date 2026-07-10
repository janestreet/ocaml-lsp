type 'a t

val create : unit -> _ t
val fill : 'a t -> 'a -> unit

(** [read t] blocks until [t] is filled. *)
val read : 'a t -> 'a
