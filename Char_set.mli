type t

include Base.Comparable.S with type t := t

val equal : t -> t -> bool
val compare : t -> t -> int
val pp : t Fmt.t
val union : t -> t -> t
val inter : t -> t -> t
val diff : t -> t -> t
val empty : t
val any : t
val singleton : char -> t
val complement : t -> t
val is_empty : t -> bool
val mem : char -> t -> bool
