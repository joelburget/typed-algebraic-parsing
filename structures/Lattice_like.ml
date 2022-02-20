module Meet_semilattice_laws (T : Lattice_like_sigs.Meet_semilattice) = struct
  open T
  open Util.Make (T)

  let meet_associative a b c = mk3 a b c (a && b && c) (a && b && c)
  let meet_commutative a b = mk2 a b (a && b) (b && a)
  let meet_idempotent a = mk1 a (a && a) a
end

module Bounded_meet_semilattice_laws (T : Lattice_like_sigs.Bounded_meet_semilattice) =
struct
  open T
  open Util.Make (T)
  include Meet_semilattice_laws (T)

  let meet_top a = mk1 a (a && top) a
end

module Join_semilattice_laws (T : Lattice_like_sigs.Join_semilattice) = struct
  open T
  open Util.Make (T)

  let join_associative a b c = mk3 a b c (a || b || c) (a || b || c)
  let join_commutative a b = mk2 a b (a || b) (b || a)
  let join_idempotent a = mk1 a (a || a) a
end

module Bounded_join_semilattice_laws (T : Lattice_like_sigs.Bounded_join_semilattice) =
struct
  open T
  open Util.Make (T)
  include Join_semilattice_laws (T)

  let join_bot a = mk1 a (a || bot) a
end

module Lattice_laws (T : Lattice_like_sigs.Lattice) = struct
  open T
  open Util.Make (T)
  include Meet_semilattice_laws (T)
  include Join_semilattice_laws (T)

  let absorption_1 a b = mk2 a b (a || (a && b)) a
  let absorption_2 a b = mk2 a b (a && (a || b)) a
end

module Bounded_lattice_laws (T : Lattice_like_sigs.Bounded_lattice) = struct
  open T
  open Util.Make (T)
  include Bounded_meet_semilattice_laws (T)
  include Bounded_join_semilattice_laws (T)

  let absorption_1 a b = mk2 a b (a || (a && b)) a
  let absorption_2 a b = mk2 a b (a && (a || b)) a
end

module Complemented_lattice_laws (T : Lattice_like_sigs.Complemented_lattice) = struct
  open T
  open Util.Make (T)
  include Bounded_lattice_laws (T)

  let top_complement a = mk1 a (a || complement a) top
  let bot_complement a = mk1 a (a && complement a) bot
end

module Distributive_lattice_laws (T : Lattice_like_sigs.Distributive_lattice) = struct
  open T
  open Util.Make (T)
  include Lattice_laws (T)

  let distribute_over_join a b c = mk3 a b c (a || (b && c)) ((a || b) && (a || c))
  let distribute_over_meet a b c = mk3 a b c (a && (b || c)) ((a && b) || (a && c))
end

module Heyting_algebra_laws (T : Lattice_like_sigs.Heyting_algebra) = struct
  open T
  open Util.Make (T)
  include Distributive_lattice_laws (T)

  let self_implication a = mk1 a (a => a) top
  let absorption_3 a b = mk2 a b (a && a => b) (a && b)
  let absorption_4 a b = mk2 a b (b && a => b) b
  let distribute_over_implication a b c = mk3 a b c (a => (b && c)) (a => b && a => c)
end

module Boolean_algebra_laws (T : Lattice_like_sigs.Boolean_algebra) = struct
  include Heyting_algebra_laws (T)
  include Complemented_lattice_laws (T)
end
