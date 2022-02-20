module type Meet_semilattice_laws = sig
  type t

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
  type t

  include Bounded_meet_semilattice_laws with type t := t
  include Bounded_join_semilattice_laws with type t := t

  (** [a || (a && b) = a] *)
  val absorption_1 : t -> t -> bool

  (** [a && (a || b) = a] *)
  val absorption_2 : t -> t -> bool
end

module type Distributive_lattice_laws = sig
  include Lattice_laws

  (** [a || (b && c) = (a || b) && (a || c)] *)
  val distribute_over_join : t -> t -> t -> bool

  (** [a && (b || c) = (a && b) || (a && c)] *)
  val distribute_over_meet : t -> t -> t -> bool
end

(** See {!Meet_semilattice} / {!Join_semilattice}. *)
module type Semilattice_base = sig
  type t

  val ( = ) : t -> t -> bool
end

(** A partially ordered set with a meet operation. *)
module type Meet_semilattice = sig
  include Semilattice_base

  (** Meet, ie greatest lower bound, infimum, "and", or intersection. *)
  val ( && ) : t -> t -> t

  module Laws : Meet_semilattice_laws
end

(** A bounded {!Meet_semilattice} with a top element. *)
module type Bounded_meet_semilattice = sig
  include Meet_semilattice

  (** A greatest element (top). *)
  val top : t

  module Laws : Bounded_meet_semilattice_laws
end

(** A partially ordered set with a join operation. *)
module type Join_semilattice = sig
  include Semilattice_base

  (** Join; ie least upper bound, supremum, "or", or union. *)
  val ( || ) : t -> t -> t

  module Laws : Join_semilattice_laws
end

(** A bounded {!Join_semilattice} with a least element. *)
module type Bounded_join_semilattice = sig
  include Join_semilattice

  (** A least element (bottom). *)
  val bot : t

  module Laws : Bounded_join_semilattice_laws
end

(** A partially ordered set where every pair of elements has both a join
    ({!Join_semilattice.( || )}) and a meet ({!Meet_semilattice.( && )}). *)
module type Lattice = sig
  include Meet_semilattice
  include Join_semilattice with type t := t
  module Laws : Lattice_laws
end

(** A {!Lattice} which satisfies {!Distributive_lattice_laws}. *)
module type Distributive_lattice = sig
  include Lattice
  module Laws : Distributive_lattice_laws
end

(* TODO: complete lattice, Heyting algebra, Boolean algebra *)
