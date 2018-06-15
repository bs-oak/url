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
  
let to_int str =
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

let rec join delim = function
  | x :: [] -> x
  | x :: xs -> x ^ delim ^ (join delim xs)
  | [] -> ""