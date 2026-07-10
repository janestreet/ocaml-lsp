open Core

(** Call [Core.List.split_n] with the same arguments, but guarantee a boxed return value *)
val split_n_boxed : 'a List.t -> int -> 'a List.t * 'a List.t

(** Set the value of [Base.Backtrace.elide] which controls the behavior of backtrace
    serialization functions such as to_string, to_string_list, and sexp_of_t. When set to
    false, these functions behave as expected, returning a faithful representation of
    their argument. When set to true, these functions will ignore their argument and
    return a message indicating that behavior.

    The default value is false. *)
val elide_backtrace : bool -> unit

(** Get a [ppx_string] [Config.t] by calling [config_for_string] and passing
    implementation-appropriate arguments *)
val ppx_string_config_for_string : Ppx_string.Config.t

(** Call [Set_once.set_if_none] and pass [~here] if supported by the underlying
    implementation *)
val set_once_set_if_none : 'a Set_once.t -> here:Source_code_position.t -> 'a -> unit

(** Create an optional flag with a default doc *)
val flag_optional_with_default_doc
  :  string
  -> 'a Command.Arg_type.t
  -> ('a -> Sexp.t)
  -> default:'a
  -> doc:string
  -> 'a Command.Param.t
