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
  if Ext.String.is_empty str || String.contains str '@' then
    None
  else
    match Ext.String.indexes str ':' with
    | [] -> 
      Some {protocol; host = str; port = None; path; query; fragment}
    
    | i :: [] -> 
      (match Ext.String.to_int (Ext.String.drop_left (i + 1) str) with
      | None -> 
        None
      
      | Some port -> 
        Some {protocol; host = Ext.String.left str i; port = Some port; path; query; fragment})

    | _ -> 
      None

let chomp_before_query protocol query fragment str =
  if Ext.String.is_empty str then
    None
  else
    match Ext.String.indexes str '/' with
    | [] ->
      chomp_before_path protocol "/" query fragment str
    
    | i :: _ ->
      chomp_before_path protocol (Ext.String.drop_left i str) query fragment (Ext.String.left str i)

let chomp_before_fragment protocol fragment str =
  if Ext.String.is_empty str then
    None
  else 
    match Ext.String.indexes str '?' with
    | [] -> 
      chomp_before_query protocol None fragment str

    | i :: _ ->
      chomp_before_query protocol (Some (Ext.String.drop_left (i + 1) str)) fragment (Ext.String.left str i)

let chomp_after_protocol protocol str =
  if Ext.String.is_empty str then 
    None
  else
    match Ext.String.indexes str '#' with
    | [] -> 
      chomp_before_fragment protocol None str

    | i :: _ -> 
      chomp_before_fragment protocol (Some (Ext.String.drop_left (i + 1) str)) (Ext.String.left str i)

let from_string str =
  if Ext.String.is_prefix str "http://" then
    chomp_after_protocol Http (Ext.String.drop_left 7 str)
  else if Ext.String.is_prefix str "https://" then
    chomp_after_protocol Https (Ext.String.drop_left 8 str)
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