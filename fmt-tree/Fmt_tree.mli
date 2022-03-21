type t =
  { label : string
  ; children : t list
  }

val mk : string -> t list -> t
val pp : ?max_depth:int -> t Fmt.t
