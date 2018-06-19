(* helpers *)

let rec join delim = function
  | x :: [] -> x
  | x :: xs -> x ^ delim ^ (join delim xs)
  | [] -> ""

(* builder *)

type query_parameter = string * string

type root = 
  | Absolute
  | Relative
  | CrossOrigin of string

let root_to_pre_path = function
  | Absolute -> "/"
  | Relative -> ""
  | CrossOrigin pre_path -> pre_path ^ "/"

let to_query_pair (key, value) =
  key ^ "=" ^ value

let to_query parameters =
  match parameters with
  | [] -> ""
  | _ -> "?" ^ join "&" (List.map to_query_pair parameters)

let absolute path_segments parameters =
  "/" ^ (join "/" path_segments) ^ to_query parameters

let relative path_segments parameters =
  (join "/" path_segments) ^ to_query parameters

let cross_origin pre_path path_segments parameters =
  pre_path ^ "/" ^ (join "/" path_segments) ^ to_query parameters

let custom root path_segments parameters optional_fragment =
  let fragmentless =
    (root_to_pre_path root) ^ (join "/" path_segments) ^ (to_query parameters)
  in
  match optional_fragment with
  | None -> fragmentless
  | Some fragment -> fragmentless ^ "#" ^ fragment

let string key value =
  (Url.percent_encode key), (Url.percent_encode value)

let int key value =
  (Url.percent_encode key), (string_of_int value)