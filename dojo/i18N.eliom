(* -*- tuareg -*- *)

{shared{

let cap = String.capitalize

module Fr : I18N_sig.Text = struct

  let form things singular plural =
    if List.length things > 1 then plural else singular

  let of' things =
    form things "du" "des"

  let mark' things =
    form things "" "s"

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

  let hide = "cacher"

  let action = "action"

  let question = "question"

  let questions = "questions"

  let username = "login"

  let password = "mot de passe"

  let connect = "OK"

  let disconnect = "déconnecte"

  let run = "exécute"

  let run_all = "tout"

  let status = "status"

  let description = "description"

  let new_submission = "Nouvelle soumission :"

  let sorry_autotesting_is_disabled =
    "Désolé, le mode autotest n'est pas activé."

  let autotesting_title = "Tests"

  let the_server_is_up =
    "Le serveur se lance correctement."

  let the_asynchronous_communication_layer_is_ok =
    "La couche de communication client-serveur fonctionne."

  let the_vfs_is_coherent =
    "Le système de fichier journalisé est dans un état cohérent."

  let there_is_no_repository_at_ressource_root =
    "Il n'y a pas de dépôt GIT à la racine de l'emplacement des ressources."

  let the_following_files_are_untracked fs =
    Printf.sprintf
      "L'histoire %s fichier%s suivant%s n'est pas pris en charge: `%s'"
      (of' fs)
      (mark' fs)
      (mark' fs)
      (String.concat " " fs)

  let the_filesystem_is_consistent =
    "Le système de fichier est cohérent."

  let consistent =
    "cohérent"

end

module String = (val
    match CORE_config.current_language () with
      | CORE_config.French -> (module Fr : I18N_sig.Text)
)


}}
