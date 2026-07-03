module Prod_or_dev : sig
  type t =
    | Prod
    | Dev
  [@@deriving enumerate, sexp]

  val to_string : t -> string
end

(** Whether this ocaml-lsp is running in a deployed configuration, or locally on a
    developer's machine. Here "developer" means ocaml-lsp-server developer, not
    ocaml-lsp-server user

    Chosing Localhost means that ocaml-lsp-server won't check the identity of
    remote-lsp-server, as well as connect to it on localhost.

    DO NOT USE LOCALHOST IN PROD/DEV *)
type t =
  | Deployed of Prod_or_dev.t
  | Localhost
[@@deriving sexp]
