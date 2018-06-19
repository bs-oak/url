(* helpers *)

let is_empty str =
  str = ""

let is_prefix str prefix =
  try 
    prefix = (String.sub str 0 (String.length prefix))
  with
    _ -> false

let drop_left num str =
  if num > String.length str then 
    ""
  else
    String.sub str num ((String.length str) - num)

let indexes str chr  =
  let rec loop pos =
    try
      let idx = String.index_from str pos chr in
      idx :: loop (idx + 1)
    with
      _ ->  []
  in
  loop 0

let string_to_int_safe str =
  try 
    Some (int_of_string str)
  with 
    | _ -> None

let left str n =
  if n < 1 then 
    ""
  else if n > String.length str then
    str
  else
    String.sub str 0 n

(* url *)

type protocol = Http | Https

type url =
  { protocol : protocol
  ; host : string
  ; port : int option
  ; path : string
  ; query : string option
  ; fragment : string option
  }

let chomp_before_path protocol path query fragment str =
  if is_empty str || String.contains str '@' then
    None
  else
    match indexes str ':' with
    | [] -> 
      Some {protocol; host = str; port = None; path; query; fragment}
    
    | i :: [] -> 
      (match string_to_int_safe (drop_left (i + 1) str) with
      | None -> 
        None
      
      | Some port -> 
        Some {protocol; host = left str i; port = Some port; path; query; fragment})

    | _ -> 
      None

let chomp_before_query protocol query fragment str =
  if is_empty str then
    None
  else
    match indexes str '/' with
    | [] ->
      chomp_before_path protocol "/" query fragment str
    
    | i :: _ ->
      chomp_before_path protocol (drop_left i str) query fragment (left str i)

let chomp_before_fragment protocol fragment str =
  if is_empty str then
    None
  else 
    match indexes str '?' with
    | [] -> 
      chomp_before_query protocol None fragment str

    | i :: _ ->
      chomp_before_query protocol (Some (drop_left (i + 1) str)) fragment (left str i)

let chomp_after_protocol protocol str =
  if is_empty str then 
    None
  else
    match indexes str '#' with
    | [] -> 
      chomp_before_fragment protocol None str

    | i :: _ -> 
      chomp_before_fragment protocol (Some (drop_left (i + 1) str)) (left str i)

let from_string str =
  if is_prefix str "http://" then
    chomp_after_protocol Http (drop_left 7 str)
  else if is_prefix str "https://" then
    chomp_after_protocol Https (drop_left 8 str)
  else
    None

let add_port port_option starter =
  match port_option with
    | None -> starter
    | Some port -> starter ^ ":" ^ string_of_int port

let add_prefixed prefix segment_option starter =
  match segment_option with
    | None -> starter
    | Some segment -> starter ^ prefix ^ segment

let of_string url =
  let protocol = 
    match url.protocol with
    | Http -> "http://"
    | Https -> "https://" 
  in
  add_port url.port (protocol ^ url.host) ^ url.path
    |> add_prefixed "?" url.query
    |> add_prefixed "#" url.fragment

let percent_encode =
  Js.Global.encodeURIComponent

let percent_decode str =
  try
    Some (Js.Global.decodeURIComponent str)
  with
  | _ -> None