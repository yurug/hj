(* -*- tuareg -*- *)

(** Errors, and their user messages. *)

{shared{

open CORE_identifier
open CORE_errors

let string_of_position p =
  Printf.sprintf "File \"\", line %d, characters %d" p.line p.character

let string_of_position' p1 p2 =
  Printf.sprintf "File \"\", line %d, characters %d-%d"
    p1.line p1.character p2.character

let string_of_error : [< CORE_errors.all ] -> string = function
  | `SystemError e ->
    "System: " ^ e
  | `AlreadyExists p ->
    I18N.String.the_following_file_already_exists (string_of_path p)
  | `DirectoryDoesNotExist p ->
    I18N.String.the_following_directory_does_not_exist (string_of_path p)
  | `UndefinedEntity id ->
    I18N.String.does_not_exist (string_of_identifier id)
  | `InvalidLabel _ ->
    I18N.String.invalid_label_in_identifier
  | `MaximalNumberOfLoginAttemptsReached ->
    I18N.String.you_reach_the_maximal_number_of_login_attempts
  | `BadLoginPasswordPair ->
    I18N.String.bad_login_password_pair
  | `NoSuchSandbox ->
    I18N.String.no_such_sandbox
  | `SyntaxError (start, stop, msg) ->
    string_of_position' start stop ^ ": " ^ msg
  | `TypeError (p, msg) ->
    I18N.String.type_error (string_of_position p) msg
  | `NeedAnnotation p ->
    I18N.String.need_annotation (string_of_position p)
  | `UnboundVariable (p, v) ->
    I18N.String.unbound_variable (string_of_position p) v
  | `BadApplication p ->
    I18N.String.illtyped_application (string_of_position p)
  | `EvalError ->
    I18N.String.runtime_error
  | `AssertFailure s ->
    s

}}

let fatal_error e =
  Ocsigen_messages.errlog (
    Printf.sprintf "Fatal error: %s\n" (string_of_error e)
  );
  exit 1

let warn e =
  Ocsigen_messages.errlog (
    Printf.sprintf "Warning: %s\n" (string_of_error e)
  )

let bad_assumption e =
  Ocsigen_messages.errlog (
    Printf.sprintf "Bad assumption: %s\n" e
  )
