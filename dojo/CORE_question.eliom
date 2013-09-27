(** -*- tuareg -*- *)

(** Question entities. *)

open Lwt

open CORE_entity
open CORE_identifier
open CORE_error_messages
open COMMON_pervasives

module C = CORE_description_CST

type description = {
  title     : string;
}
deriving (Json)

include CORE_entity.Make (struct

  type data = description deriving (Json)

  let react = passive

end)

{client{
  type reference = CORE_identifier.t deriving (Json)
}}

let (statement_filename, statement_source, statement_retrieve)
    = source "statement.txt"

let initial_source_filenames = [ statement_filename ]

let change_from_user_description q def =
  lwt source = statement_source q in
  CORE_source.set_content source C.(def.statement.node);
  change q (fun data -> return { title = C.(def.title.node) })
  >> return (`OK ())

let make_blank id =
  let data  = { title = I18N.String.no_title }
  and deps  = CORE_inmemory_entity.empty_dependencies
  and psets = CORE_property.empty in
  make ~init:(data, deps, psets, initial_source_filenames) id

let statement x =
  observe x (fun _ ->
    lwt source = statement_source x in
    return (CORE_source.content source)
  )

let create_service ok_page ko_page =
  Eliom_registration.Redirection.register_service
    ~path:["create_question"]
    ~get_params:Eliom_parameter.(suffix (list "id" (string "label")))
    (fun id () ->
      try_lwt
        let id = identifier_of_string_list id in
        make_blank id >>= function
          | `OK e ->
            return (ok_page e)
          | `KO e ->
            return (ko_page (string_of_error e))
      with InvalidLabel _ ->
       return (ko_page (string_of_error (`InvalidLabel (String.concat "/" id))))
    )
