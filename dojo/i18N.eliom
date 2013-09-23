(* -*- tuareg -*- *)

{shared{

let cap s =
  if s.[0] = '\195' && s.[1] = '\160' then (
    let s' = String.copy s in
    s'.[1] <- '\128';
    s'
  )
  else String.capitalize s

module Fr : I18N_sig.Text = struct

  let form things singular plural =
    if List.length things > 1 then plural else singular

  let of' things =
    form things "du" "des"

  let mark' things =
    form things "" "s"

  let the_hacking_dojo = "Le Dojo de Programmation"

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

  let email = "email"

  let password = "mot de passe"

  let connect = "OK"

  let disconnect = "déconnecte"

  let run = "exécute"

  let run_all = "tout"

  let status = "status"

  let description = "description"

  let subscribe = "s'inscrire"

  let firstname = "prénom"

  let surname = "nom"

  let about = "à propos"

  let home = "accueil"

  let logout = "déconnecte"

  let exercises = "exercices"

  let must_do = "à faire"

  let should_do = "suggérés"

  let can_do = "accessibles"

  let new_ = "nouveau"

  let yes = "oui"

  let no = "non"

  let identifier = "identifier"

  let new_submission = "Nouvelle soumission :"

  let sorry_autotesting_is_disabled =
    "Désolé, le mode autotest n'est pas activé."

  let autotesting_title = "Tests"

  let the_following_operation_is_broken operation reason =
    Printf.sprintf
      "L'opération '%s' est défaillante. (%s)"
      operation
      reason

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

  let entity =
    "entité"

  let the_following_file_already_exists fname =
    Printf.sprintf
      "Le fichier `%s' existe déjà."
      fname

  let the_following_directory_does_not_exist dname =
    Printf.sprintf
      "Le répertoire `%s' n'existe pas."
      dname

  let does_not_exist name =
    Printf.sprintf
      "`%s' n'existe pas."
      name

  let do_you_want_to_create_it =
    "Est-ce vous souhaitez le créer?"

  let the_entity_subsystem_works =
    "Le sous-système des entités semble fonctionner."

  let saving fname =
    Printf.sprintf
      "Sauvegarde de `%s'."
      fname

  let created what name =
    Printf.sprintf
      "%s `%s' créé(e)."
      (cap what) name

  let create what name =
    Printf.sprintf
      "Création de %s `%s'."
      what name

  let you_reach_the_maximal_number_of_login_attempts =
    "Vous avez atteint le nombre maximal de tentatives autorisé."

  let bad_login_password_pair =
    "Le login ou le mot de passe est invalide."

  let never_connected_before =
    "Jamais connecté auparavant."

  let this_field_must_not_be_empty =
    "Ce champ ne peut pas être vide."

  let invalid_label_in_identifier =
    "Identificateur invalide lexicalement."

  let parse_error =
    "Erreur de syntaxe."

end

module String = (val
    match CORE_config.current_language () with
      | CORE_config.French -> (module Fr : I18N_sig.Text)
)


}}
