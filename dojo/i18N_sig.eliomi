(* -*- tuareg -*- *)

(** All the messages of the interfaces must be built using the
    following constants. *)
module type Text =
sig
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
  val action : string
  val question : string
  val questions : string
  val username : string
  val password : string
  val run : string
  val run_all : string
  val description : string
  val status : string
  val connect : string
  val disconnect : string
  val new_submission : string
  val sorry_autotesting_is_disabled : string
  val autotesting_title : string
  val the_server_is_up : string
  val the_asynchronous_communication_layer_is_ok : string
end
