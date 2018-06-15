type 'a t

val parse: 'a t -> string list Dict.t -> 'a

val string: string -> string option t

val int: string -> int option t

val enum: string -> 'a Dict.t -> 'a option t

val custom: string -> (string list -> 'a) -> 'a t

val map: ('a -> 'b) -> 'a t -> 'b t

val map2: 
  ('a -> 'b -> 'c) -> 
  'a t -> 
  'b t -> 
  'c t

val map3: 
  ('a -> 'b -> 'c -> 'd) -> 
  'a t -> 
  'b t -> 
  'c t ->
  'd t

val map4: 
  ('a -> 'b -> 'c -> 'd -> 'e) -> 
  'a t -> 
  'b t -> 
  'c t ->
  'd t ->
  'e t

val map5: 
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 
  'a t -> 
  'b t -> 
  'c t ->
  'd t ->
  'e t ->
  'f t

val map6: 
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) -> 
  'a t -> 
  'b t -> 
  'c t ->
  'd t ->
  'e t ->
  'f t ->
  'g t

val map7: 
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) -> 
  'a t -> 
  'b t -> 
  'c t ->
  'd t ->
  'e t ->
  'f t ->
  'g t ->
  'h t

val map8: 
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i) -> 
  'a t -> 
  'b t -> 
  'c t ->
  'd t ->
  'e t ->
  'f t ->
  'g t ->
  'h t ->
  'i t