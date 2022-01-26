type t

val pp : t Fmt.t
val empty : t
val any : t
val eps : t
val ( >>> ) : t -> t -> t
val chr : char -> t
val str : string -> t
val star : t -> t
val plus : t -> t
val ( || ) : t -> t -> t
val ( && ) : t -> t -> t
val complement : t -> t
val nullable : t -> bool
val delta : Uchar.t -> t -> t
val string_delta : string -> t -> t

module Vector : sig
  type t

  val delta : Uchar.t -> t -> t
  val string_delta : string -> t -> t
  val nullable : t -> bool
end
