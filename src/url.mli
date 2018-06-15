type protocol = Http | Https

type url =
  { protocol : protocol
  ; host : string
  ; port : int option
  ; path : string
  ; query : string option
  ; fragment : string option
  }

val from_string : string -> url option

val of_string : url -> string

val percent_encode : string -> string

val percent_decode : string -> string option