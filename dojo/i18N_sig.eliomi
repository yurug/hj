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
  val filename_label : string
  val upload_label : string
  val not_yet_submitted : string
  val please_login : string
  val unreadable_submission : string
  val diagnostic : string
  val link : string
  val answer : string
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
  val about : string
  val home : string

  (** Sentences. *)
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
  val does_not_exist : string -> string
  val the_entity_subsystem_works : string
  val saving : string -> string
  val created : string -> string -> string
  val create : string -> string -> string
  val you_reach_the_maximal_number_of_login_attempts : string
  val bad_login_password_pair : string
  val never_connected_before : string
end

}}
