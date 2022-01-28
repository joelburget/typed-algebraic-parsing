type t

include Base.Comparable.S with type t := t
include Base.Sexpable.S with type t := t

val pp : t Fmt.t
val empty : t
val any : t
val singleton : Uchar.t -> t
val of_char : char -> t
val range : Uchar.t -> Uchar.t -> t
val of_list : Uchar.t list -> t
val of_string : string -> t
val negate : t -> t
val union : t -> t -> t
val inter : t -> t -> t
val mem : t -> Uchar.t -> bool
val is_empty : t -> bool
val choose : t -> Uchar.t option
val choose_exn : t -> Uchar.t
