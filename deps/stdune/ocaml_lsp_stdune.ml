open! Core

(* Upstream [stdune] exposes [Left]/[Right] at the top level; [Core.Either] uses
   [First]/[Second]. The vendored [fiber] relies on the former. *)
type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

module Either = struct
  type nonrec ('a, 'b) t = ('a, 'b) either =
    | Left of 'a
    | Right of 'b
end

module Nothing = Core.Nothing
module Result = Core.Result
module Option = Core.Option

module Code_error = struct
  type t =
    { message : Sexp.t
    ; loc : Source_code_position.t option
    }

  exception E of t

  let create ?loc message = { message; loc }
  let raise_s ?loc message = raise (E (create ?loc message))

  let raise message pairs =
    let message =
      match pairs with
      | [] -> Sexp.Atom message
      | _ :: _ ->
        Sexp.List
          (Sexp.Atom message
           :: Core.List.map pairs ~f:(fun (k, v) -> Sexp.List [ Sexp.Atom k; v ]))
    in
    raise_s message
  ;;
end

module Exn_with_backtrace = struct
  type t =
    { exn : exn
    ; backtrace : Backtrace.t
    }

  let sexp_of_t { exn; backtrace } =
    Sexp.List
      [ Sexp.List [ Sexp.Atom "exn"; Exn.sexp_of_t exn ]
      ; Sexp.List [ Sexp.Atom "backtrace"; Backtrace.sexp_of_t backtrace ]
      ]
  ;;

  let capture exn = { exn; backtrace = Backtrace.Exn.most_recent () }

  let try_with f =
    match f () with
    | result -> Ok result
    | exception exn -> Error (capture exn)
  ;;

  let reraise { exn; backtrace } = Exn.raise_with_original_backtrace exn backtrace

  let pp_uncaught formatter { exn; backtrace } =
    Format.fprintf
      formatter
      "@[<v>Uncaught exception:@ %s@,%s@]"
      (Exn.to_string exn)
      (Backtrace.to_string backtrace)
  ;;

  let map t ~f = { t with exn = f t.exn }
  let map_and_reraise ~f t = reraise (map t ~f)
end

module Fdecl : sig
  type 'a t [@@deriving sexp_of]

  (** [create sexp_of_value] creates a forward declaration. The [sexp_of_value] parameter
      is used for reporting errors in [set], [set_idempotent] and [get]. *)
  val create : ('a -> Sexp.t) -> 'a t

  (** [set t x] sets the value that will be returned by [get t] to [x]. Raises if [set]
      was already called. *)
  val set : 'a t -> 'a -> unit

  (** [get t] returns the [x] if [set comp x] was called. Raises if [set] has not been
      called yet. *)
  val get : here:Core.Source_code_position.t -> 'a t -> 'a
end = struct
  type 'a state =
    | Unset
    | Set of 'a
  [@@deriving sexp_of]

  type 'a t =
    { mutable state : 'a state
    ; sexp_of_value : 'a -> Sexp.t
    }
  [@@deriving sexp_of]

  let create sexp_of_value = { state = Unset; sexp_of_value }

  let set t new_ =
    match t.state with
    | Unset -> t.state <- Set new_
    | Set old ->
      Code_error.raise_s
        [%message
          "Fdecl.set: already set"
            ~old:(t.sexp_of_value old : Sexp.t)
            ~new_:(t.sexp_of_value new_ : Sexp.t)]
  ;;

  let sexp_of_position_hum (p : Lexing.position) =
    [%sexp
      (p.pos_fname
       ^ ":"
       ^ Int.to_string p.pos_lnum
       ^ ":"
       ^ Int.to_string (p.pos_cnum - p.pos_bol)
       : string)]
  ;;

  let get ~here t =
    match t.state with
    | Unset ->
      Code_error.raise_s
        [%message "Fdecl.get: not set" ~here:(sexp_of_position_hum here : Sexp.t)]
    | Set x -> x
  ;;
end

module Bin = struct
  let path_sep = ':'

  let parse_path ?(sep = path_sep) s =
    String.split s ~on:sep
    |> List.filter_map ~f:(function
      | "" -> None
      | p -> Some (File_path.of_string p))
  ;;

  let _PATH = lazy (parse_path (Option.value ~default:"" (Core.Sys.getenv "PATH")))

  let exists fp =
    match Sys_unix.is_file (File_path.to_string fp) with
    | `Yes -> true
    | `No | `Unknown -> false
  ;;

  (** Search for [prog] in path if prog is relative, otherwise just return [prog].
      Emulates behaviour of unix [which] command *)
  let which_in_path ~path prog =
    match File_path.to_relative prog with
    | Some rel_prog ->
      List.find_map path ~f:(fun dir ->
        let fn = File_path.append dir rel_prog in
        Option.some_if (exists fn) fn)
    | None -> Option.some_if (exists prog) prog
  ;;

  let which prog = which_in_path ~path:(Lazy.force _PATH) (File_path.of_string prog)
end

(* The modules below provide the slice of the upstream [stdune] API that the vendored
   [ocaml_lsp_fiber] library relies on, implemented on top of [Core]; its sources
   [open! Ocaml_lsp_stdune] in place of the upstream [open Stdune]. *)

module Array = struct
  let make len x = Core.Array.init len ~f:(fun _ -> x)
  let to_list = Core.Array.to_list
  let get = Core.Array.get
  let set = Core.Array.set
  let length = Core.Array.length
end

module List = struct
  let rev = Core.List.rev
  let length = Core.List.length
  let fold_left l ~init ~f = Core.List.fold l ~init ~f

  let rev_partition_map l ~f =
    let rec loop l accl accr =
      match l with
      | [] -> accl, accr
      | x :: l ->
        (match (f x : (_, _) either) with
         | Left a -> loop l (a :: accl) accr
         | Right b -> loop l accl (b :: accr))
    in
    loop l [] []
  ;;
end

module Queue = struct
  type 'a t = 'a Core.Queue.t

  let create () = Core.Queue.create ()
  let push t x = Core.Queue.enqueue t x
  let pop t = Core.Queue.dequeue t
  let is_empty t = Core.Queue.is_empty t
end

module Set = struct
  module type S = sig
    type elt
    type t

    val to_seq : t -> elt Seq.t
  end
end

module Appendable_list = struct
  type 'a t = 'a list

  let empty = []
  let singleton x = [ x ]
  let ( @ ) = Core.List.append
  let to_list t = t
end

module type Monoid = sig
  type t

  val empty : t
  val combine : t -> t -> t
end

module Monoid = struct
  module Appendable_list (M : sig
      type t
    end) : Monoid with type t = M.t Appendable_list.t = struct
    type t = M.t Appendable_list.t

    let empty = Appendable_list.empty
    let combine = Appendable_list.( @ )
  end
end

module Univ_map = struct
  type t = Core.Univ_map.t

  module Key = struct
    type 'a t = 'a Core.Univ_map.Key.t

    (* Upstream describes the stored value with a [_ -> Dyn.t] (for debugging);
       [Core.Univ_map.Key.create] wants a [_ -> Sexp.t]. The description is only used for
       debugging, so we ignore the one supplied by [fiber]. *)
    let create ~name (_ : 'a -> _) : 'a t =
      Core.Univ_map.Key.create ~name (fun _ -> Sexp.Atom name)
    ;;
  end

  let empty = Core.Univ_map.empty
  let find t key = Core.Univ_map.find t key
  let set t key data = Core.Univ_map.set t ~key ~data
  let remove t key = Core.Univ_map.remove t key
end
