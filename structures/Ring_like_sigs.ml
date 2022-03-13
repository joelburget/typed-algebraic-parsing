(** Multiplication distributes over addition. *)
module type Distributive = sig
  type t

  (** [a * (b + c) = (a * b) + (a * c)] *)
  val left_distributive : t -> t -> t -> bool

  (** [(b + c) * a = (b * a) + (c * a)] *)
  val right_distributive : t -> t -> t -> bool
end

module type Semiring_laws = sig
  type t

  (** Multiplication distributes over addition. *)
  include Distributive with type t := t

  (** A commutative monoid wrt addition. *)
  module Addition : Group_like_sigs.Commutative_monoid_laws

  (** A monoid wrt multiplication. *)
  module Multiplication : Group_like_sigs.Monoid_laws
end

module type Rng_laws = sig
  type t

  (** Multiplication distributes over addition. *)
  include Distributive with type t := t

  (** An abelian group wrt addition. *)
  module Addition : Group_like_sigs.Abelian_group_laws

  (** A semigroup wrt multiplication. *)
  module Multiplication : Group_like_sigs.Semigroup_laws
end

module type Ring_laws = sig
  type t

  (** Multiplication distributes over addition. *)
  include Distributive with type t := t

  (** An abelian group wrt addition. *)
  module Addition : Group_like_sigs.Abelian_group_laws

  (** A semigroup wrt multiplication. *)
  module Multiplication : Group_like_sigs.Monoid_laws
end

module type Base = sig
  type t

  val pp : t Fmt.t

  module Infix : sig
    val ( = ) : t -> t -> bool
  end
end

(** A {!Ring} without negation. *)
module type Semiring = sig
  include Base

  (** A binary operation analogous to addition. *)
  val ( + ) : t -> t -> t

  (** A binary operation analogous to multiplication. *)
  val ( * ) : t -> t -> t

  (** An additive identity: analogous to [0]. *)
  val additive_ident : t

  (** A multiplicative identity: analogous to [1]. *)
  val multiplicative_ident : t
end

module type Rig = Semiring

(** A {!Ring} without multiplicative identity.

    - (t, +) *)
module type Rng = sig
  include Base

  (** A binary operation analogous to addition. *)
  val ( + ) : t -> t -> t

  (** A binary operation analogous to multiplication. *)
  val ( * ) : t -> t -> t

  (** Negation wrt addition. *)
  val negate : t -> t

  (** An additive identity: analogous to [0]. *)
  val additive_ident : t
end

(** A ring (with a multiplicative identity, see {!Rng} for the version without). *)
module type Ring = sig
  type t

  include Semiring with type t := t
  include Rng with type t := t
end
