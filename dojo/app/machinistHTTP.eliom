(* -*- tuareg -*- *)

open Lwt
open ExtPervasives
open User
open Machinist
open Identifier
open HTTP
open UserHTTP

let machinist_create = HTTP.(
  api_service "machinist_create" "machinist"
    (string "name")
    (string "status")
    "Create a fresh machinist."
    (fun name ->
      (teacher_only () >>>= fun user ->
       Machinist.create (identifier_of_string name)
      ) >>= function
        | `OK _ -> completed ()
        | `KO `NotLogged -> error "not_logged"
        | `KO `FailedLogin -> error "not_logged"
        | `KO (`AlreadyExists _) -> error "already_exists"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
        | `KO `StudentsCannotCreateExercise -> error "teacher_only"
        | `KO `ForbiddenService -> error "teacher_only"
        | `KO (`UndefinedEntity id) ->
          error ("undefined:" ^ (string_of_identifier id)))
)

let (upload_resource, download_resource, ls_resource) =
  EntityHTTP.create_resource_management_api
    (module Machinist)
    "machinist_upload"
    "machinist_download"
    "machinist_ls"
    "machinist"

let machinist_set_logins = HTTP.(
  api_service "machinist_set_logins" "machinist"
    (string "identifier" ** string "logins")
    (string "status")
    "Update the list of machinist's logins."
    (fun (name, logins) ->
      try Str.(
        let logins = split (regexp ",") logins in
        let logins = List.map (fun f ->
          match split (regexp ":") f with
            | [login; key] -> (login, key)
            | _ -> raise Not_found
        ) logins
        in
        (teacher_only () >>>= fun user ->
         Machinist.make (identifier_of_string name) >>>= fun machinist ->
         Machinist.set_logins machinist logins >> return (`OK ())
        ) >>= function
          | `OK _ -> completed ()
          | `KO (`InvalidCode e) -> success e
          | `KO `NotLogged -> error "not_logged"
          | `KO `FailedLogin -> error "not_logged"
          | `KO (`AlreadyExists _) -> error "already_exists"
          | `KO (`SystemError e) -> error ("system:" ^ e)
          | `KO (`InternalError e) ->
            error ("internal:" ^ (Printexc.to_string e))
          | `KO `StudentsCannotCreateExercise -> error "teacher_only"
          | `KO `ForbiddenService -> error "teacher_only"
          | `KO (`UndefinedEntity id) ->
            error ("undefined:" ^ (string_of_identifier id))
      ) with Not_found -> error "error:invalid_logins"
    ))

let machinist_set_addresses = HTTP.(
  api_service "machinist_set_addresses" "machinist"
    (string "identifier" ** string "addresses")
    (string "status")
    "Update the list of machinist's addresses."
    (fun (name, addrs) ->
      try Str.(
        let addrs = split (regexp ",") addrs in
        let addrs = List.map (fun f ->
          match split (regexp ":") f with
            | [ip; port] -> (ip, int_of_string port)
            | _ -> raise Not_found
        ) addrs
        in
        (teacher_only () >>>= fun user ->
         Machinist.make (identifier_of_string name) >>>= fun machinist ->
         Machinist.set_addresses machinist addrs >> return (`OK ())
        ) >>= function
          | `OK _ -> completed ()
          | `KO (`InvalidCode e) -> success e
          | `KO `NotLogged -> error "not_logged"
          | `KO `FailedLogin -> error "not_logged"
          | `KO (`AlreadyExists _) -> error "already_exists"
          | `KO (`SystemError e) -> error ("system:" ^ e)
          | `KO (`InternalError e) ->
            error ("internal:" ^ (Printexc.to_string e))
          | `KO `StudentsCannotCreateExercise -> error "teacher_only"
          | `KO `ForbiddenService -> error "teacher_only"
          | `KO (`UndefinedEntity id) ->
            error ("undefined:" ^ (string_of_identifier id)))
      with _ -> error "error:invalid_addresses"
      ))


let machinist_execute = HTTP.(
  api_service "machinist_execute" "machinist"
    (string "identifier" ** string "command")
    (string "status")
    "Execute a command through a machinist's sandbox."
    (fun (name, command) ->
      (teacher_only () >>>= fun user ->
       let open Sandbox in
       let buffer = Buffer.create 13 in
       let put s = Buffer.add_string buffer s in
       Machinist.make (identifier_of_string name) >>>= fun mc ->
       lwt sandbox =
         Machinist.provide_sandbox_interface mc false (fun _ -> return ())
       in
       let observer = function
         | WaitingForSandbox _ -> return (put "> waiting...")
         | FileModification _ -> return (put "> a file was modified...")
         | WriteStdout (_, s) -> return (put ("stdout> " ^ s ^ "\n"))
         | WriteStderr (_, s) -> return (put ("stderr> " ^ s ^ "\n"))
         | Exited s -> return ()
       in
       exec_on_sandbox command true sandbox [] observer
       >> return (`OK (Buffer.contents buffer))
      ) >>= function
        | `OK e -> success e
        | `KO `NotLogged -> error "not_logged"
          | `KO `FailedLogin -> error "not_logged"
        | `KO (`AlreadyExists _) -> error "already_exists"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
        | `KO `StudentsCannotCreateExercise -> error "teacher_only"
        | `KO `ForbiddenService -> error "teacher_only"
        | `KO (`UndefinedEntity id) ->
          error ("undefined:" ^ (string_of_identifier id)))
)
