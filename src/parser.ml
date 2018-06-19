(* helpers *)

let string_to_int_safe str =
  try 
    Some (int_of_string str)
  with 
    | _ -> None

(* query *)

module Query = Parser_query

type 'a state  =
  { visited : string list
  ; unvisited : string list
  ; params : string list Dict.t
  ; frag : string option
  ; value : 'a
  }

type ('a,'b) t = ('a state -> 'b state list)

let custom string_to_something state =
  match state.unvisited with
  | [] -> []
  | next :: rest ->
    (match string_to_something next with
    | Some next_value -> 
      [{ visited = next :: state.visited
      ; unvisited = rest
      ; params = state.params
      ; frag = state.frag
      ; value = state.value next_value
      }]
    | None -> [])

let string state =
    match state.unvisited with
  | [] -> []
  | next :: rest ->
    [{ visited = next :: state.visited
    ; unvisited = rest
    ; params = state.params
    ; frag = state.frag
    ; value = state.value next
    }]

let int state =
  match state.unvisited with
  | [] -> []
  | next :: rest ->
    (match string_to_int_safe next with
    | Some next_value -> 
      [{ visited = next :: state.visited
      ; unvisited = rest
      ; params = state.params
      ; frag = state.frag
      ; value = state.value next_value
      }]
    | None -> [])

let s str state =
  match state.unvisited with
  | [] -> []
  | next :: rest ->
    if next == str then
      [{state with visited = next :: state.visited 
      ; unvisited = rest
      }]
    else
      []

let (</>) before after state =
  before state
  |> List.map after
  |> List.concat

let map_state fn state =
  {state with value = fn state.value}

let map sub_value parse_arg state = 
  {state with value = sub_value}
  |> parse_arg
  |> List.map (map_state state.value) 

let one_of parsers state =
  parsers
  |> List.map (fun parser -> parser state) 
  |> List.concat

let top state =
  [state]

let query query_parser state =
  [{state with value = state.value (Query.parse query_parser state.params)}]

let (<?>) parser query_parser =
  parser </> (query query_parser)

let fragment to_frag state =
  [{state with value = state.value (to_frag state.frag)}]

(* Prepare Path *)

let rec remove_final_empty = function
  | [] -> []
  | "" :: [] -> []
  | segment :: rest -> segment :: remove_final_empty rest

let prepare_path path =
  match (Array.to_list (Js.String.split "/" path)) with
  | "" :: segments -> remove_final_empty segments
  | segments -> remove_final_empty segments

(* Prepare Query *)

let add_to_parameters_help value = function
  | None -> Some [value]
  | Some list -> Some (value :: list)

let add_param segment dict =
  match Array.to_list (Js.String.split "=" segment) with
  | [raw_key; raw_value] ->
    (match Url.percent_decode raw_key with
    | None -> dict
    | Some key -> 
      (match Url.percent_decode raw_value with
      | None -> dict
      | Some value -> Dict.update key (add_to_parameters_help value) dict
      )
    )
  | _ -> dict

let prepare_query = function
  | None -> 
    Dict.empty
  | Some query -> 
    List.fold_right 
      add_param 
      (Js.String.split "&" query |> Array.to_list) 
      Dict.empty

(* Parse *)

let rec get_first_match = function
  | [] -> None
  | state :: rest ->
    (match state.unvisited with
    | [] -> Some state.value
    | [""] -> Some state.value
    | _ -> get_first_match rest
    )

let parse parser (url : Url.url) =
  { visited = [] 
  ; unvisited = prepare_path url.path
  ; params = prepare_query url.query
  ; frag = url.fragment 
  ; value = fun x -> x
  }
  |> parser
  |> get_first_match

let parse_hash parser (url : Url.url) =
  { visited = [] 
  ; unvisited = prepare_path (Belt.Option.getWithDefault url.fragment "")
  ; params = prepare_query url.query
  ; frag = None
  ; value = fun x -> x
  }
  |> parser
  |> get_first_match  