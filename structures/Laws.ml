module type S = sig
  type t

  module Ring : Ring_like_sigs.Ring_laws with type t := t
  module Lattice : Lattice_like_sigs.Distributive_lattice_laws with type t := t

  (** [-(-a) = a] *)
  val stability : t -> bool
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
  open Util.Make (T)

  type t = T.t

  module Ring = Ring_like.Ring_laws (struct
    include T
    include Infix
  end)

  module Lattice = Lattice_like.Distributive_lattice_laws (T)

  let stability a = equal1 a (negate (negate a)) a
end
