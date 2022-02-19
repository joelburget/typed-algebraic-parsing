module Set = struct
  module type S = sig
    type element
    type t

    val pp : t Fmt.t
    val empty : t
    val singleton : element -> t
    val is_empty : t -> bool
    val ( = ) : t -> t -> bool
    val inter : t -> t -> t
    val union : t -> t -> t
    val mem : element -> t -> bool
  end
end

module type S = sig
  type t

  val ( = ) : t -> t -> bool
  val pp : t Fmt.t

  module Set : Set.S with type element = t
end
