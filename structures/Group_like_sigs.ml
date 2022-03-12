(** A magma is a type with a single operation and no laws. *)
module type Magma = sig
  type t

  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> t
end

(** Laws for {!Semigroup}. *)
module type Semigroup_laws = sig
  type t

  (** [(a <> b) <> c = a <> (b <> c)] *)
  val associative : t -> t -> t -> bool
end

(** A semigroup is an associative {!Magma}.

    In other words, types with a single operation which must be associative.

    See {!Semigroup_laws}. *)
module type Semigroup = sig
  include Magma
end

(** Laws for {!Monoid}. *)
module type Monoid_laws = sig
  include Semigroup_laws

  (** [e <> a = a] *)
  val left_ident : t -> bool

  (** [a <> e = a] *)
  val right_ident : t -> bool
end

(** A monoid is a {!Semigroup} with an identity element.

    See {!Monoid_laws}. *)
module type Monoid = sig
  include Semigroup

  val identity : t
end

(** Laws for {!Commutative_monoid}. *)
module type Commutative_monoid_laws = sig
  include Monoid_laws

  (** [a <> b = b <> a] *)
  val commutative : t -> t -> bool
end

(** A {!Monoid} whose operation is commutative.

    See {!Commutative_monoid_laws}. *)
module type Commutative_monoid = sig
  include Monoid
end

(** Laws for {!Group}. *)
module type Group_laws = sig
  include Monoid_laws

  (** [inv a <> a = e] *)
  val left_inverse : t -> bool

  (** [a <> inv a = e] *)
  val right_inverse : t -> bool
end

(** A group is a {!Monoid} with inverses.

    See {!Group_laws}. *)
module type Group = sig
  include Monoid

  val inverse : t -> t
end

(** Laws for {!Abelian_group}. *)
module type Abelian_group_laws = sig
  include Group_laws

  (** [a <> b = b <> a] *)
  val commutative : t -> t -> bool
end

(** An abelian group is a {!Group} whose operation is commutative.

    See {!Abelian_group_laws}. *)
module type Abelian_group = sig
  include Group
end
