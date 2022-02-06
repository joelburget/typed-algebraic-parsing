module type Stream = sig
  type element
  type t

  val peek : t -> element option
  val junk : t -> unit
  val of_string : string -> t
end

module type Token = sig
  type t

  val ( = ) : t -> t -> bool
  val pp : t Fmt.t

  module Set : sig
    type element = t
    type t

    val pp : t Fmt.t
    val empty : t
    val singleton : element -> t
    val is_empty : t -> bool
    val ( = ) : t -> t -> bool
    val inter : t -> t -> t
    val union : t -> t -> t
    val mem : t -> element -> bool
  end
end

module type Token_stream = sig
  type token
  type stream

  module Token : Token with type t = token
  module Stream : Stream with type element = token and type t = stream
end
