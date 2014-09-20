(* -*- tuareg -*- *)

{shared{

(** All the messages of the interfaces must be built using the
    following constants. *)
module type Text =
sig
  (** Words. *)
  val the_hacking_dojo : string
  val assignments : string
  val submission : string
  val submissions : string
  val name_label : string
  val friends : string
  val filename_label : string
  val upload_label : string
  val not_yet_submitted : string
  val please_login : string
  val please_navigate : string
  val unreadable_submission : string
  val diagnostic : string
  val link : string
  val answer : string
  val evaluation : string
  val state : string
  val waiting_state : string
  val processing_state : string
  val finished_state : string
  val total : string
  val mandatory : string
  val see : string
  val hide : string
  val action : string
  val question : string
  val questions : string
  val username : string
  val password : string
  val email : string
  val run : string
  val run_all : string
  val share : string
  val import : string
  val description : string
  val status : string
  val connect : string
  val disconnect : string
  val new_submission : string
  val autotesting_title : string
  val consistent : string
  val entity : string
  val subscribe : string
  val firstname : string
  val surname : string
  val score : string
  val trace : string
  val about : string
  val home : string
  val student: string
  val master: string
  val logout : string
  val exercises : string
  val must_do : string
  val can_do : string
  val should_do : string
  val new_ : string
  val identifier : string
  val yes : string
  val no : string
  val logins : string
  val addresses : string
  val sandboxes : string
  val about : string
  val notifications : string
  val read : [`Male | `Female] -> string
  val update : string
  val edit : string

  (** Forms. *)
  val not_ : string -> string
  val many : string -> string

  (** Sentences. *)
  val master_corner: string
  val relaunch_all: string
  val download: string
  val download_all: string
  val download_pdf: string
  val last_connection: string
  val sorry_autotesting_is_disabled : string
  val the_following_operation_is_broken : string -> string -> string
  val the_server_is_up : string
  val the_asynchronous_communication_layer_is_ok : string
  val the_vfs_is_coherent : string
  val there_is_no_repository_at_ressource_root : string
  val the_following_files_are_untracked : string list -> string
  val the_filesystem_is_consistent: string
  val the_following_file_already_exists : string -> string
  val the_following_directory_does_not_exist : string -> string
  val this_field_must_not_be_empty : string
  val does_not_exist : string -> string
  val do_you_want_to_create_it: string
  val the_entity_subsystem_works : string
  val saving : string -> string
  val created : string -> string -> string
  val create : string -> string -> string
  val you_reach_the_maximal_number_of_login_attempts : string
  val bad_login_password_pair : string
  val never_connected_before : string
  val invalid_label_in_identifier : string
  val parse_error: string
  val lexing_unexpected_character: string
  val lexing_eof_in_raw: string
  val do_you_really_want_to_create_a_question_named : string -> string
  val no_title: string
  val answer_expected : string -> string
  val invalid_answer : string
  val in_a_file_named : string -> string
  val no_such_sandbox : string
  val type_error : string -> string -> string
  val need_annotation : string -> string
  val unbound_variable : string -> string -> string
  val illtyped_application: string -> string
  val runtime_error : string
  val to_be_provided : string
  val last_submitted_file : string
  val evaluation_needed_for_exercise : string -> string
  val wait_for_master_evaluation : string
  val access_denied : string
  val provided_file : string
  val please_reload_the_page : string
  val password_reset_email_body : string -> string -> string
  val password_reset_email_subject : string
  val choose_a_password : string
  val password_reset_sent_by_email : string -> string -> string
  val reset_password : string
  val password_reset_sent_by_email : string -> string -> string
  val you_do_not_exist : string -> string
end

}}
