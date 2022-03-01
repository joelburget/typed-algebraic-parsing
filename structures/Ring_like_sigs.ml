module type Semiring_laws = sig
  type t

  (* include Group_like_sigs.Commutative_monoid *)
end

module type Rng_laws = sig
  type t
end

module type Ring_laws = sig
  include Semiring_laws
  include Rng_laws with type t := t
end

(** A {!Ring} without negation. *)
module type Semiring = sig
  type t

  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val additive_ident : t
  val multiplicative_ident : t
end

module type Rig = Semiring

(** A {!Ring} without multiplicative identity.

    - (t, +) *)
module type Rng = sig
  type t

  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val additive_ident : t
end

(** (with a multiplicative identity) *)
module type Ring = sig end
