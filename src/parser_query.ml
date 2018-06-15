type 'a t = (string list Dict.t) -> 'a

let parse parser dict =
  parser dict

let custom key fn dict =
    Dict.get key dict
    |> Ext.Option.with_default []
    |> fn
    
let string key =
  let fn = function
    | [str] -> Some str
    | _ -> None
  in
  custom key fn

let int key =
  let fn = function
    | [str] -> Ext.String.to_int str
    | _ -> None
  in
  custom key fn

let enum key dict =
  let fn = function
    | [str] -> Dict.get str dict
    | _ -> None
  in
  custom key fn

let map fn a dict =
  fn (a dict)

let map2 fn a b dict =
  fn (a dict) (b dict)

let map3 fn a b c dict =
  fn (a dict) (b dict) (c dict)

let map4 fn a b c d dict =
  fn (a dict) (b dict) (c dict) (d dict)

let map5 fn a b c d e dict =
  fn (a dict) (b dict) (c dict) (d dict) (e dict)

let map6 fn a b c d e f dict =
  fn (a dict) (b dict) (c dict) (d dict) (e dict) (f dict)

let map7 fn a b c d e f g dict =
  fn (a dict) (b dict) (c dict) (d dict) (e dict) (f dict) (g dict)

let map8 fn a b c d e f g h dict =
  fn (a dict) (b dict) (c dict) (d dict) (e dict) (f dict) (g dict) (h dict)