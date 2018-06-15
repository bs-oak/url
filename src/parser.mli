type ('a,'b) t

val string : ((string -> 'a),'a) t

val int : ((int -> 'a),'a) t

val s : string -> ('a, 'a) t

val (</>) : ('a,'b) t -> ('b,'c) t -> ('a,'c) t

val map : 'a -> ('a,'b) t -> ('b -> 'c, 'c) t

val one_of : ('a,'b) t list -> ('a,'b) t

val top : ('a,'a) t

val custom : (string -> 'a option) -> (('a -> 'b),'b) t

val (<?>) : ('a, ('query -> 'b)) t -> 'query Parser_query.t -> ('a, 'b) t

val query : 'query Parser_query.t -> (('query -> 'a),'a) t

val fragment : (string option -> 'fragment) -> (('fragment -> 'a),'a) t

val parse : (('a -> 'a),'a) t -> Url.url -> 'a option

val parse_hash : (('a -> 'a),'a) t -> Url.url -> 'a option

module Query = Parser_query