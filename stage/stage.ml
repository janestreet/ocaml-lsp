module Prod_or_dev = struct
  type t =
    | Prod
    | Dev
  [@@deriving enumerate, sexp]

  let to_string = function
    | Prod -> "prod"
    | Dev -> "dev"
  ;;
end

type t =
  | Deployed of Prod_or_dev.t
  | Localhost
    (* DO NOT USE IN PROD. Indicates whether ocaml-lsp should use localhost remote-lsp
       running on the local machine *)
[@@deriving sexp]
