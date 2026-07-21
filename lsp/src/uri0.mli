open! Import

type t = Uri_lexer.t [@@deriving bin_io, sexp_of]

include Json.Jsonable.S with type t := t

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int
val to_path : t -> string

(** The [query] can sometimes confuse merlin as to what kind of file the buffer is
    (interface vs implementation), which can manifest as spurious type errors. *)
val drop_query : t -> t

val same_path : path:string -> t -> bool
val of_path : string -> t
val to_string : t -> string
val of_string : string -> t
val query : t -> string option
val fragment : t -> string option

(** Splits a [t] on [+share+]. The first segment in the second part is [+share+] *)
val split_on_share : t -> (File_path.Part.t list * File_path.Part.t list) option

module Private : sig
  val win32 : bool ref
end
