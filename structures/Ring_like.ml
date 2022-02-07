module type S = sig
  type t

  val additive_ident : t
  val multiplicative_ident : t
  val ( = ) : t -> t -> bool
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
end

module type Laws = sig
  type t

  (** [(a + b) + c = a + (b + c)] *)
  val plus_associative : t -> t -> t -> bool

  (** [a + b = b + a] *)
  val plus_commutative : t -> t -> bool

  (** [a + 0 = a] *)
  val plus_ident : t -> bool

  (** [a * (-a) = 0] *)
  val mul_inverse : t -> bool

  (** [(a * b) * c = a * (b * c)] *)
  val mul_associative : t -> t -> t -> bool

  (** [a * b = b * a] *)
  val mul_commutative : t -> t -> bool

  (** [a * 1 = a] *)
  val mul_ident : t -> bool

  (** [a * (b + c) = (a * b) + (a * c)] *)
  val left_distributive : t -> t -> t -> bool

  (** [(b + c) * a = (b * a) + (c * a)] *)
  val right_distributive : t -> t -> t -> bool
end
