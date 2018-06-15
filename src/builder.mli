type query_parameter = string * string

type root = 
  | Absolute
  | Relative
  | CrossOrigin of string

val absolute : string list -> query_parameter list -> string 

val relative : string list -> query_parameter list -> string

val cross_origin : string -> string list -> query_parameter list -> string

val custom : root  -> string list -> query_parameter list -> string option -> string 

val string : string -> string -> query_parameter 

val int : string -> int -> query_parameter 

val to_query : query_parameter list -> string 