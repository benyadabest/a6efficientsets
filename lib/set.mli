type 'a t

val empty : 'a t

val is_empty: 'a t -> bool

val mem : 'a -> 'a t -> bool

val insert : 'a -> 'a t  -> 'a t

val to_string : ('a -> string) -> 'a t -> string

val rep_ok : 'a t -> 'a t