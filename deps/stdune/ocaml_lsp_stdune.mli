open! Core

(* Upstream [stdune] exposes [Left]/[Right] at the top level (used by the vendored
   [fiber]); [Core.Either] uses [First]/[Second]. *)
type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

module Either : sig
  type nonrec ('a, 'b) t = ('a, 'b) either =
    | Left of 'a
    | Right of 'b
end

module Nothing = Core.Nothing
module Result = Core.Result
module Option = Core.Option

module Code_error : sig
  type t =
    { message : Sexp.t
    ; loc : Source_code_position.t option
    }

  exception E of t

  val create : ?loc:Source_code_position.t -> Sexp.t -> t

  (** Raise with a sexp message. *)
  val raise_s : ?loc:Source_code_position.t -> Sexp.t -> 'a

  (** Upstream [stdune]'s [Code_error.raise]; used by the vendored [fiber]. *)
  val raise : string -> (string * Sexp.t) list -> 'a
end

module Exn_with_backtrace : sig
  type t =
    { exn : exn
    ; backtrace : Backtrace.t
    }

  val sexp_of_t : t -> Sexp.t
  val capture : exn -> t
  val try_with : (unit -> 'a) -> ('a, t) Result.t
  val reraise : t -> 'a
  val pp_uncaught : Format.formatter -> t -> unit
  val map_and_reraise : f:(exn -> exn) -> t -> 'a
end

module Fdecl : sig
  type 'a t

  val create : ('a -> Sexp.t) -> 'a t
  val set : 'a t -> 'a -> unit
  val get : here:Source_code_position.t -> 'a t -> 'a
end

module Bin : sig
  val which : string -> File_path.t option
end

module Array : sig
  val make : int -> 'a -> 'a array
  val to_list : 'a array -> 'a list
  val get : 'a array -> int -> 'a
  val set : 'a array -> int -> 'a -> unit
  val length : 'a array -> int
end

module List : sig
  val rev : 'a list -> 'a list
  val length : 'a list -> int
  val fold_left : 'a list -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
  val rev_partition_map : 'a list -> f:('a -> ('b, 'c) either) -> 'b list * 'c list
end

module Queue : sig
  type 'a t

  val create : unit -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a option
  val is_empty : 'a t -> bool
end

module Set : sig
  module type S = sig
    type elt
    type t

    val to_seq : t -> elt Seq.t
  end
end

module Appendable_list : sig
  type 'a t

  val empty : 'a t
  val singleton : 'a -> 'a t
  val ( @ ) : 'a t -> 'a t -> 'a t
  val to_list : 'a t -> 'a list
end

module type Monoid = sig
  type t

  val empty : t
  val combine : t -> t -> t
end

module Monoid : sig
  module Appendable_list (M : sig
      type t
    end) : Monoid with type t = M.t Appendable_list.t
end

module Univ_map : sig
  type t

  module Key : sig
    type 'a t

    val create : name:string -> ('a -> 'b) -> 'a t
  end

  val empty : t
  val find : t -> 'a Key.t -> 'a option
  val set : t -> 'a Key.t -> 'a -> t
  val remove : t -> 'a Key.t -> t
end
