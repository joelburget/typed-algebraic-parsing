(** A magma is a type with a single operation and no laws. *)
module type Magma = sig
  type t

  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> t

  module Laws : sig
    (* no laws! *)
  end
end

(** A semigroup is an associative {!Magma}.

    In other words, types with a single operation which must be associative. *)
module type Semigroup = sig
  include Magma

  module Laws : sig
    include module type of Laws

    (** [(a <> b) <> c = a <> (b <> c)] *)
    val associative : t -> t -> t -> bool
  end
end

(** A monoid is a {!Semigroup} with an identity element. *)
module type Monoid = sig
  include Semigroup

  val identity : t

  module Laws : sig
    include module type of Laws

    (** [e <> a = a] *)
    val left_ident : t -> bool

    (** [a <> e = a] *)
    val right_ident : t -> bool
  end
end

(** A group is a {!Monoid} with inverses. *)
module type Group = sig
  include Monoid

  val inverse : t -> t

  module Laws : sig
    include module type of Laws

    (** [inv a <> a = e] *)
    val left_inverse : t -> bool

    (** [a <> inv a = e] *)
    val right_inverse : t -> bool
  end
end

(** An abelian group is a {!Group} whose operation is commutative. *)
module type Abelian_group = sig
  include Group

  module Laws : sig
    include module type of Laws

    (** [a <> b = b <> a] *)
    val commutative : t -> t -> bool
  end
end
