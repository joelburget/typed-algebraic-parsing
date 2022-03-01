module type Meet_semilattice_laws = sig
  type t

  include Ordered_set_sigs.Partial_order_laws with type t := t

  (** [a && (b && c) = (a && b) && c] *)
  val meet_associative : t -> t -> t -> bool

  (** [a && b = b && a] *)
  val meet_commutative : t -> t -> bool

  (** [a && a = a] *)
  val meet_idempotent : t -> bool
end

module type Bounded_meet_semilattice_laws = sig
  include Meet_semilattice_laws

  (** [a && top = a] *)
  val meet_top : t -> bool
end

module type Join_semilattice_laws = sig
  type t

  include Ordered_set_sigs.Partial_order_laws with type t := t

  (** [a || (b || c) = (a || b) || c] *)
  val join_associative : t -> t -> t -> bool

  (** [a || b = b || a] *)
  val join_commutative : t -> t -> bool

  (** [a || a = a] *)
  val join_idempotent : t -> bool
end

module type Bounded_join_semilattice_laws = sig
  include Join_semilattice_laws

  (** [a || bot = a] *)
  val join_bot : t -> bool
end

module type Lattice_laws = sig
  include Meet_semilattice_laws
  include Join_semilattice_laws with type t := t

  (** [a || (a && b) = a] *)
  val absorption_1 : t -> t -> bool

  (** [a && (a || b) = a] *)
  val absorption_2 : t -> t -> bool
end

module type Bounded_lattice_laws = sig
  include Bounded_meet_semilattice_laws
  include Bounded_join_semilattice_laws with type t := t
  include Lattice_laws with type t := t
end

module type Complemented_lattice_laws = sig
  include Bounded_lattice_laws

  (** [a || complement a = 1] *)
  val top_complement : t -> bool

  (** [a && complement a = 0] *)
  val bot_complement : t -> bool
end

module type Distributive_lattice_laws = sig
  include Lattice_laws

  (** [a || (b && c) = (a || b) && (a || c)] *)
  val distribute_over_join : t -> t -> t -> bool

  (** [a && (b || c) = (a && b) || (a && c)] *)
  val distribute_over_meet : t -> t -> t -> bool
end

module type Bounded_distributive_lattice_laws = sig
  include Bounded_lattice_laws
  include Distributive_lattice_laws with type t := t
end

module type Heyting_algebra_laws = sig
  include Bounded_distributive_lattice_laws

  (** [a => a = 1] *)
  val self_implication : t -> bool

  (** [a && (a => b) = a && b] *)
  val absorption_3 : t -> t -> bool

  (** [b && (a => b) = b] *)
  val absorption_4 : t -> t -> bool

  (** [a => (b && c) = (a => b) && (a => c)] *)
  val distribute_over_implication : t -> t -> t -> bool
end

module type Boolean_algebra_laws = sig
  include Heyting_algebra_laws
  include Complemented_lattice_laws with type t := t

  (** [complement (complement a) <= a] *)
  val stable : t -> bool
end

(** A partially ordered set with a meet operation. *)
module type Meet_semilattice = sig
  include Ordered_set_sigs.Preorder

  (** Meet, ie greatest lower bound, infimum, "and", or intersection. *)
  val ( && ) : t -> t -> t

  (* module Laws : Meet_semilattice_laws *)
end

(** A bounded {!Meet_semilattice} with a top element. *)
module type Bounded_meet_semilattice = sig
  include Meet_semilattice

  (** A greatest element (top). *)
  val top : t

  (* module Laws : Bounded_meet_semilattice_laws *)
end

(** A partially ordered set with a join operation. *)
module type Join_semilattice = sig
  include Ordered_set_sigs.Preorder

  (** Join; ie least upper bound, supremum, "or", or union. *)
  val ( || ) : t -> t -> t

  (* module Laws : Join_semilattice_laws *)
end

(** A bounded {!Join_semilattice} with a least element. *)
module type Bounded_join_semilattice = sig
  include Join_semilattice

  (** A least element (bottom). *)
  val bot : t

  (* module Laws : Bounded_join_semilattice_laws *)
end

(** A partially ordered set where every pair of elements has both a join
    ({!Join_semilattice.( || )}) and a meet ({!Meet_semilattice.( && )}). *)
module type Lattice = sig
  include Meet_semilattice
  include Join_semilattice with type t := t
  (* module Laws : Lattice_laws *)
end

module type Bounded_lattice = sig
  include Bounded_meet_semilattice
  include Bounded_join_semilattice with type t := t
  (* module Laws : Bounded_lattice_laws *)
end

module type Complemented_lattice = sig
  include Bounded_lattice

  val complement : t -> t
  (* module Laws : Complemented_lattice_laws *)
end

(** A {!Lattice} which satisfies {!Distributive_lattice_laws}. *)
module type Distributive_lattice = sig
  include Lattice
  (* module Laws : Distributive_lattice_laws *)
end

(** Both a {!Bounded_lattice} and a {!Distributive_lattice}. *)
module type Bounded_distributive_lattice = sig
  include Bounded_lattice
  include Distributive_lattice with type t := t

  (* module Laws : Bounded_distributive_lattice_laws *)
end

(** A {!Bounded_distributive_lattice} with a notion of implication. *)
module type Heyting_algebra = sig
  include Bounded_distributive_lattice

  val ( => ) : t -> t -> t
  (* module Laws : Heyting_algebra_laws *)
end

module type Boolean_algebra = sig
  include Heyting_algebra
  include Complemented_lattice with type t := t
end
