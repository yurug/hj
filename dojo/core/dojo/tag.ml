open ExtPervasives

type tag = Tag of string * Identifier.t * int

deriving (Json)

module Set = Rb.Dict (struct
  type key = string deriving (Json)
  type image = (string * int) list deriving (Json)
  let compare = String.compare
end)

type set = Set.t deriving (Json)

let tag who tags difficulty tagset =
  List.fold_left (fun tagset t ->
    let taggers = try Set.lookup t tagset with Not_found -> [] in
    Set.update t (update_assoc who difficulty taggers) tagset
  ) tagset tags
