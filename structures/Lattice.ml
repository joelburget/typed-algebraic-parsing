module type S = sig
  type t

  val top : t
  val bottom : t

  module Infix : sig
    val ( = ) : t -> t -> bool
    val ( && ) : t -> t -> t
    val ( || ) : t -> t -> t
  end
end

module type Laws = sig
  type t

  (** [a + a = a] *)
  val idempotent_union : t -> bool

  (** [a * a = a] *)
  val idempotent_inter : t -> bool

  (** [a + 0 = a] *)
  val join_bot : t -> bool

  (** [a * 1 = a] *)
  val meet_top : t -> bool

  (** [a + (a * b) = a] *)
  val absorption_1 : t -> t -> bool

  (** [a * (a + b) = a] *)
  val absorption_2 : t -> t -> bool

  (** [a + (b * c) = (a + b) * (a + c)] *)
  val distribute_over_union : t -> t -> t -> bool

  (** [a * (b + c) = (a * b) + (a * c)] *)
  val distribute_over_inter : t -> t -> t -> bool
end
