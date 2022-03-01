open Lattice_like_sigs

module Meet_semilattice_laws (T : Meet_semilattice) :
  Meet_semilattice_laws with type t = T.t = struct
  open Util.Make (T)
  include T
  include Ordered_set.Partial_order_laws (T)

  let meet_associative a b c = equal3 a b c (a && b && c) (a && b && c)
  let meet_commutative a b = equal2 a b (a && b) (b && a)
  let meet_idempotent a = equal1 a (a && a) a
end

module Bounded_meet_semilattice_laws (T : Bounded_meet_semilattice) :
  Bounded_meet_semilattice_laws with type t = T.t = struct
  open Util.Make (T)
  include T
  include Meet_semilattice_laws (T)

  let meet_top a = equal1 a (a && top) a
end

module Join_semilattice_laws (T : Join_semilattice) :
  Join_semilattice_laws with type t = T.t = struct
  open Util.Make (T)
  include T
  include Ordered_set.Partial_order_laws (T)

  let join_associative a b c = equal3 a b c (a || b || c) (a || b || c)
  let join_commutative a b = equal2 a b (a || b) (b || a)
  let join_idempotent a = equal1 a (a || a) a
end

module Bounded_join_semilattice_laws (T : Bounded_join_semilattice) :
  Bounded_join_semilattice_laws with type t = T.t = struct
  open Util.Make (T)
  include T
  include Join_semilattice_laws (T)

  let join_bot a = equal1 a (a || bot) a
end

module Lattice_laws (T : Lattice) : Lattice_laws with type t = T.t = struct
  open Util.Make (T)
  include T
  include Meet_semilattice_laws (T)
  include Join_semilattice_laws (T)

  let absorption_1 a b = equal2 a b (a || (a && b)) a
  let absorption_2 a b = equal2 a b (a && (a || b)) a
end

module Bounded_lattice_laws (T : Bounded_lattice) :
  Bounded_lattice_laws with type t = T.t = struct
  open Util.Make (T)
  include T
  include Bounded_meet_semilattice_laws (T)
  include Bounded_join_semilattice_laws (T)

  let absorption_1 a b = equal2 a b (a || (a && b)) a
  let absorption_2 a b = equal2 a b (a && (a || b)) a
end

module Complemented_lattice_laws (T : Complemented_lattice) :
  Complemented_lattice_laws with type t = T.t = struct
  open Util.Make (T)
  include T
  include Bounded_lattice_laws (T)

  let top_complement a = equal1 a (a || complement a) top
  let bot_complement a = equal1 a (a && complement a) bot
end

module Distributive_lattice_laws (T : Distributive_lattice) :
  Distributive_lattice_laws with type t = T.t = struct
  open Util.Make (T)
  include T
  include Lattice_laws (T)

  let distribute_over_join a b c = equal3 a b c (a || (b && c)) ((a || b) && (a || c))
  let distribute_over_meet a b c = equal3 a b c (a && (b || c)) ((a && b) || (a && c))
end

module Heyting_algebra_laws (T : Heyting_algebra) :
  Heyting_algebra_laws with type t = T.t = struct
  open Util.Make (T)
  include T
  include Bounded_lattice_laws (T)
  include Distributive_lattice_laws (T)

  let self_implication a = equal1 a (a => a) top
  let absorption_3 a b = equal2 a b (a && a => b) (a && b)
  let absorption_4 a b = equal2 a b (b && a => b) b
  let distribute_over_implication a b c = equal3 a b c (a => (b && c)) (a => b && a => c)
end

module Boolean_algebra_laws (T : Boolean_algebra) :
  Boolean_algebra_laws with type t = T.t = struct
  open Util.Make (T)
  include T
  include Heyting_algebra_laws (T)
  include Complemented_lattice_laws (T)

  let stable a = op1 ( <= ) a (complement (complement a)) a
end
