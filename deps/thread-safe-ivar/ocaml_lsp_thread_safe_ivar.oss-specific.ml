type 'a t =
  { mutex : Mutex.t
  ; condition : Condition.t
  ; mutable value : 'a option
  }

let create () = { mutex = Mutex.create (); condition = Condition.create (); value = None }

let fill t value =
  Mutex.lock t.mutex;
  match t.value with
  | Some _ ->
    Mutex.unlock t.mutex;
    invalid_arg "Ocaml_lsp_thread_safe_ivar.fill: already filled"
  | None ->
    t.value <- Some value;
    Condition.broadcast t.condition;
    Mutex.unlock t.mutex
;;

let read t =
  Mutex.lock t.mutex;
  let rec loop () =
    match t.value with
    | Some value -> value
    | None ->
      Condition.wait t.condition t.mutex;
      loop ()
  in
  let value = loop () in
  Mutex.unlock t.mutex;
  value
;;
