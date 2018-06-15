include Map.Make(String)

let get key' dict =
  try
    Some (find key' dict)
  with
  | _ -> None

let update key' update_fn dict =
  let prev_value = get key' dict in
  match update_fn prev_value with
  | None -> dict
  | Some new_value -> add key' new_value dict
