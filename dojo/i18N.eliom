(* -*- tuareg -*- *)

let cap = String.capitalize

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
  val connect : string
  val disconnect : string
  val new_submission : string
  val sorry_autotesting_is_disabled : string
  val autotesting_title : string
end

module Fr : Text = struct

  let the_hacking_dojo = "Le Dojo de la Programmation"

  let assignments = "exercices"

  let submission = "soumission"

  let submissions = "soumissions"

  let name_label = "Nom"

  let filename_label = "Fichier"

  let upload_label = "OK"

  let not_yet_submitted = "Pas de soumission pour le moment."

  let please_login = "Vous devez vous connecter pour soumettre une réponse."

  let unreadable_submission = "(illisible)"

  let diagnostic = "diagnostique"

  let link = "lien"

  let answer = "réponse"

  let state = "état"

  let waiting_state = "en attente"

  let processing_state = "en cours"

  let finished_state = "terminé"

  let total = "total"

  let mandatory = "obligatoires"

  let see = "voir"

  let action = "action"

  let question = "question"

  let questions = "questions"

  let username = "login"

  let password = "mot de passe"

  let connect = "OK"

  let disconnect = "déconnecte"

  let new_submission = "Nouvelle soumission :"

  let sorry_autotesting_is_disabled =
    "Désolé, le mode autotest n'est pas activé."

  let autotesting_title = "Tests"

end

module String = (val
    match CORE_config.current_language () with
      | CORE_config.French -> (module Fr : Text)
)
