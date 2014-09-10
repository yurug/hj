open ExtPervasives

type tag = Tag of string * Identifier.t * int

deriving (Json)

module Set = Rb.Dict (struct
  type key = string deriving (Json)
  type image = (string * int) list deriving (Json)
  let compare = String.compare
end)

type set = Set.t deriving (Json)

let abs_of_neg t =
  String.(sub t 1 (length t - 1))

let negate t =
  if t = "" then t
  else if t.[0] = '-' then abs_of_neg t
  else "-" ^ t

let tag who tags difficulty tagset =
  List.fold_left (fun tagset t ->
    if t = "" then tagset
    else if t.[0] = '-' then
      Set.remove (abs_of_neg t) tagset
    else
      let taggers = try Set.lookup t tagset with Not_found -> [] in
      Set.update t (update_assoc who difficulty taggers) tagset
  ) tagset tags

let has_tag t tags =
  try
    let taggers = Set.lookup t tags in
    List.exists (fun (_, v) -> v > 0) taggers
  with Not_found ->
    false
