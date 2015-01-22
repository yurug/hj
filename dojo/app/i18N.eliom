(* -*- tuareg -*- *)

{shared{

let cap s =
  if s.[0] = '\195' && s.[1] = '\160' then (
    let s' = String.copy s in
    s'.[1] <- '\128';
    s'
  )
  else if s.[0] = '\195' && s.[1] = '\169' then (
    let s' = String.copy s in
    s'.[1] <- '\137';
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

  let many s =
    s ^ "s"

  let update = "mise à jour"

  let the_hacking_dojo = "Le Dojo de Programmation"

  let assignments = "exercices"

  let submission = "soumission"

  let submissions = "soumissions"

  let name_label = "Nom"

  let friends = "amis"

  let filename_label = "Fichier"

  let upload_label = "OK"

  let not_yet_submitted = "Pas de soumission pour le moment."

  let please_login = "Vous devez vous connecter pour soumettre une réponse."

  let please_navigate = "Cliquez sur une question pour commencer."

  let unreadable_submission = "(illisible)"

  let diagnostic = "diagnostique"

  let link = "lien"

  let answer = "réponse"

  let evaluation = "évaluation"

  let score = "score"

  let trace = "trace"

  let state = "état"

  let waiting_state = "en attente"

  let processing_state = "en cours"

  let finished_state = "terminé"

  let total = "total"

  let mandatory = "obligatoires"

  let see = "voir"

  let read = function
    | `Male -> "lu"
    | `Female -> "lue"

  let not_ w = "non " ^ w

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

  let share = "partager"

  let import = "importer"

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

  let logins = "Logins"

  let addresses = "Adresses"

  let sandboxes = "Bacs à sable alloués"

  let identifier = "identifiant"

  let master_corner = "Coin du maître"

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

  let about =
    "à propos"

  let notifications =
    "notifications"

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

  let invalid_answer =
    "Réponse invalide."

  let lexing_unexpected_character =
    "Caractère surprenant."

  let lexing_eof_in_raw =
    "Définition non terminée. (Il doit manquer une accolade fermante.)"

  let do_you_really_want_to_create_a_question_named id =
    Printf.sprintf
      "Souhaitez-vous vraiment créer une question nommée `%s' ?" id

  let no_title =
    "Sans titre"

  let answer_expected what =
    Printf.sprintf "Réponse attendue %s." what

  let in_a_file_named what =
    Printf.sprintf "dans un fichier nommé `%s'" what

  let no_such_sandbox =
    "Désolé, nous n'avons pas de quoi exécuter votre programme."

  let type_error p msg =
    p ^ ": Erreur de typage.\n  " ^ msg

  let need_annotation p =
    p ^ ": Type trop difficile à deviner."

  let unbound_variable p v =
    p ^ ": Variable indéfinie `" ^ v ^ "'."

  let illtyped_application p =
    p ^ ": Application de fonction mal typée."

  let error =
    "Erreur"

  let runtime_error =
    "Erreur à l'exécution."

  let to_be_provided =
    " à fournir."

  let student =
    "étudiant"

  let master =
    "maître"

  let last_connection =
    "dernière connexion"

  let last_submitted_file =
    "dernier fichier soumis"

  let relaunch_all =
    "tout réévaluer"

  let edit =
    "modifier"

  let download =
    "télécharger"

  let download_all =
    "tout télécharger"

  let download_pdf =
    "télécharger le PDF"

  let evaluation_needed_for_exercise e =
    Printf.sprintf
      "Une évaluation de votre part est requise pour l'exercise %s"
      e

  let wait_for_master_evaluation =
    "En attente d'une évaluation du maître."

  let access_denied =
    "Accès interdit."

  let provided_file =
    "Fichier fourni avec le sujet."

  let please_reload_the_page =
    "Veuillez recharger la page, s'il vous plait."

  let password_reset_email_body login url firstname surname =
    Printf.sprintf "\
    Bonjour %s %s,

    vous avez demandé la mise à jour du mot de passe pour l'identifiant `%s'.

    Pour accomplir cette procédure, suivez humblement le lien suivant:

    %s

    Bonne programmation à vous!
    -- Hackojo
    "
      firstname surname login url

  let password_reset_email_subject =
    "Mise à jour du mot de passe"

  let choose_a_password =
    "Choisissez un mot de passe."

  let reset_password =
    "Nouveau mot de passe"

  let password_reset_sent_by_email email root_email firstname surname =
    Printf.sprintf "
    Un message a été envoyé à `%s'. \
    Si ce n'est pas votre email, écrivez à `%s'."
      email root_email

  let you_do_not_exist =
    Printf.sprintf "Vous n'existez pas... Contactez donc `%s'!"

  let remind_not_enough_users url rurl sid idx limit expected given firstname surname =
    Printf.sprintf "\n\
    Bonjour %s %s,\n\
    \n\
    votre équipe %d du sujet %s n'est pas complète. Il faut %d membres \n\
    et vous n'en avez que %d.\n\
    \n\
    Après s'être connecté sur le Dojo, n'importe qui peut s'inscrire dans votre \
    équipe en cliquant sur le lien suivant:\n\
    %s\n\
    \n\
    Sinon, rendez-vous sur %s pour ajouter manuellement un nouveau membre à votre équipe.\n\
    \n\
    ATTENTION, vous devez effectuer cette action avant le :\n\
    \n\
    %s\n\
    \n\
    sans quoi votre réservation sera annulée.\n\
    \n\
    Bonne programmation à vous!\n\
    -- Hackojo
    "
      firstname surname idx sid expected given rurl url limit

  let remind_not_enough_users_subject =
    "Rappel : il manque des membres à votre équipe de projet!"

  let remind_confirmation_needed url sid idx limit firstname surname =
    Printf.sprintf "\
    Bonjour %s %s,\n\
    \n\
    vous avez été inscrit dans l'équipe %d du projet `%s'. Pour que cette \n\
    inscription soit définitive, il faut vous rendre à l'adresse \n\
    suivante pour confirmer votre inscription:\n\
    \n\
    %s\n\
    \n\
    ATTENTION, vous devez effectuer cette action avant le :\n\
    \n\
    %s\n\
    \n\
    sans quoi la réservation de ce sujet par votre équipe sera annulée.\n\
    \n\
    Bonne programmation à vous!\n\
    -- Hackojo
    "
      firstname surname idx sid url limit


  let remind_confirmation_needed_subject =
    "Demande de confirmation pour l'inscription à une équipe de projet"

  let cancellation_email url sid idx firstname surname =
    Printf.sprintf "\
    Bonjour,\n\
    \n\
    votre réservation de l'équipe %d sur le sujet '%s' de\n\
    %s\n\
    est annulée car vous n'avez pas répondu à temps.\n\
    \n\
    -- Hackojo
    "
      (succ idx) sid url

  let cancellation_email_subject =
    "Annulation de la réservation d'une équipe de projet"

  let withdraw_warning url sid idx wfirstname wsurname expiration is_complete firstname surname =
    let extra =
    Printf.sprintf "
    ATTENTION, votre équipe est incomplète! Vous devez compléter votre équipe avant le :\n\
    \n\
    %s\n\
    \n\
    sans quoi la réservation de ce sujet par votre équipe sera annulée.\n\
    \n\
    " expiration
    in
    Printf.sprintf "\
    Bonjour %s %s,\n\
    \n\
    Pour information, l'utilisateur %s %s s'est désincrit de votre équipe %d du sujet %s\n\
    \n\
    %s
    Bonne programmation à vous!\n\
    -- Hackojo
    "
      firstname surname wfirstname wsurname idx sid
      (if is_complete then extra else "")

  let withdraw_warning_subject =
    "Désinscription d'un membre de votre équipe"

end

module String = (val
    match Config.current_language () with
      | Config.French -> (module Fr : I18N_sig.Text)
)


}}
