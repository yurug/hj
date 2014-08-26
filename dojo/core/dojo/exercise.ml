open Lwt
open Entity
open InMemory
open Identifier
open ExtPervasives
open ExtProcess

let path = Identifier.from_strings [ "exercise" ]

let exercise_module = "hackojo.exercise <here@hackojo.org>"

let up () = VFS.(
  if not (exists path) then
    create exercise_module path
  else
    return (`OK ())
)

let exercise_identifier name =
  Identifier.(identifier_of_path (concat path (path_of_string name)))

type code_state =
  | Valid of Timestamp.t * Aka.t
  | NoCode
  | Error of Timestamp.t * string
deriving (Json)

type internal_state = {
  contributors : identifier list;
  code         : code_state
} deriving (Json)

type public_change =
  | UpdateCode

include Entity.Make (struct

  type data = internal_state deriving (Json)

  type change = public_change

  let react state mdeps cs later =
    let update_code content =
      let return_error msg =
        return { content with code = Error (Timestamp.current (), msg) }
      in
      let return_valid a =
        return { content with code = Valid (Timestamp.current (), a) }
      in
      OnDisk.load_resource (identifier state) "source.aka" >>= function
        | `OK (source, _) ->
          (try_lwt
             lwt (cst, _) = Aka.compile (Resource.content source) in
             return_valid cst
           with AkaError.Parse pos ->
             return_error (Position.string_of_pos pos ^ ": Syntax error."))
        | `KO _ ->
          (* FIXME: More informative message. *)
          return_error "internal error while loading source.aka"
    in

    let apply_change content = function
      | UpdateCode -> update_code content
    in

    (* FIXME: Common pattern to be factorized out. *)
    let content0 = content state in
    lwt content = Lwt_list.fold_left_s apply_change content0 cs in
    if content == content0 then
      return NoUpdate
    else
      return (UpdateContent content)

  let current_version = "1.0"

  let converters = []

  let string_of_change = function
    | UpdateCode -> "update code"

end)

let create who name =
  let id = exercise_identifier name in
  make id >>= function
    | `OK _ -> return (`KO (`AlreadyExists (path_of_identifier id)))
    | `KO _ ->
      User.is_teacher who >>= function
        | true ->
          let data = {
            contributors = [ User.identifier who ];
            code = NoCode
          }
          in
          let init = (data, empty_dependencies, []) in
          make ~init id
        | false ->
          return (`KO `StudentsCannotCreateExercise)

(* FIXME: This is a common pattern here: change + block observation...
   FIXME: make it a combinator. *)
let update who exo =
  let now = Timestamp.current () in
  let uptodate t = Timestamp.compare now t <= 0 in
  change ~who:(User.identifier who) exo UpdateCode >>
    let rec wait () =
      observe ~who:(User.identifier who) exo
        (fun data -> return (content data).code) >>= function
          | Valid (t, _) when uptodate t -> return (`OK ())
          | Error (t, e) when uptodate t -> return (`KO (`InvalidCode e))
          | _ -> Lwt_unix.yield () >> wait ()
    in
    wait ()

let load_module id =
  make id >>= function
    | `OK exo -> begin observe exo
      (fun data -> return (content data).code) >>= function
        | Valid (_, a) -> return (`OK a)
        | _ -> return `KO
    end
    | `KO _ -> return `KO

let questions id uid =
  load_module id >>= function
    | `OK cst ->
      let final_env = Aka.execute cst in
      assert false

    | `KO ->
      return `KO

let initialization =
  AkaCST.set_loader load_module
