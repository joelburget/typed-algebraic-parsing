module type S = sig
  type t

  module Ring : Ring_like.Laws with type t := t
  module Lattice : Lattice_like_sigs.Distributive_lattice_laws with type t := t

  (* TODO: this is the same as stability *)

  (** [-(-a) = a] *)
  val double_negation : t -> bool
end

module Make (T : sig
  type t

  val pp : t Fmt.t
  val ( || ) : t -> t -> t
  val ( && ) : t -> t -> t

  module Infix : sig
    val ( + ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( = ) : t -> t -> bool
    val ( <= ) : t -> t -> bool
  end

  val additive_ident : t
  val multiplicative_ident : t
  val negate : t -> t
  (* val bot : t *)
  (* val top : t *)
end) =
struct
  open T
  open Infix
  open Util.Make (T)

  type nonrec t = t

  module Ring = struct
    let plus_associative a b c = equal3 a b c (a + b + c) (a + (b + c))
    let plus_commutative a b = equal2 a b (a + b) (b + a)
    let plus_ident a = equal1 a (a + additive_ident) a
    let mul_inverse a = equal1 a (a * negate a) additive_ident
    let mul_associative a b c = equal3 a b c (a * b * c) (a * (b * c))
    let mul_commutative a b = equal2 a b (a * b) (b * a)
    let mul_ident a = equal1 a (a * multiplicative_ident) a
    let left_distributive a b c = equal3 a b c (a * (b + c)) ((a * b) + (a * c))
    let right_distributive a b c = equal3 a b c ((b + c) * a) ((b * a) + (c * a))
  end

  module Lattice = Lattice_like.Distributive_lattice_laws (T)

  let double_negation a = equal1 a (negate (negate a)) a
end
