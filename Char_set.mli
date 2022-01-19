type t

include Base.Comparable.S with type t := t

val pp : t Fmt.t
val union : t -> t -> t
val inter : t -> t -> t
val diff : t -> t -> t
val complement : t -> t
val empty : t
val singleton : char -> t
val any : t
val is_empty : t -> bool
val is_any : t -> bool
val mem : t -> char -> bool
