(* -*- tuareg -*- *)

(** Errors, and their user messages. *)

open CORE_identifier

let string_of_error : [< CORE_errors.all ] -> string = function
  | `SystemError e ->
    "System: " ^ e
  | `AlreadyExists p ->
    I18N.String.the_following_file_already_exists (string_of_path p)
  | `DirectoryDoesNotExist p ->
    I18N.String.the_following_directory_does_not_exist (string_of_path p)
  | `UndefinedEntity id ->
    I18N.String.does_not_exist (string_of_identifier id)
