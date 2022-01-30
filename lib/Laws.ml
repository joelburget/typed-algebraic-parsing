module type S = sig
  type t

  module Ring : sig
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

  module Lattice : sig
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

  (** [-(-a) = a] *)
  val double_negation : t -> bool
end

module Make (T : sig
  type t

  module Infix : sig
    val ( + ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( = ) : t -> t -> bool
  end

  val additive_ident : t
  val multiplicative_ident : t
  val bottom : t
  val top : t
  val negate : t -> t
end) =
struct
  open T
  open Infix

  type nonrec t = t

  module Ring = struct
    let plus_associative a b c = a + b + c = a + (b + c)
    let plus_commutative a b = a + b = b + a
    let plus_ident a = a + additive_ident = a
    let mul_inverse a = a * negate a = additive_ident
    let mul_associative a b c = a * b * c = a * (b * c)
    let mul_commutative a b = a * b = b * a
    let mul_ident a = a * multiplicative_ident = a
    let left_distributive a b c = a * (b + c) = (a * b) + (a * c)
    let right_distributive a b c = (b + c) * a = (b * a) + (c * a)
  end

  module Lattice = struct
    let idempotent_union a = a + a = a
    let idempotent_inter a = a * a = a
    let join_bot a = a + bottom = a
    let meet_top a = a * top = a
    let absorption_1 a b = a + (a * b) = a
    let absorption_2 a b = a * (a + b) = a
    let distribute_over_union a b c = a + (b * c) = (a + b) * (a + c)
    let distribute_over_inter a b c = a * (b + c) = (a * b) + (a * c)
  end

  let double_negation a = negate (negate a) = a
end
