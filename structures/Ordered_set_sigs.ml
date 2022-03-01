module type Preorder = sig
  type t

  val pp : t Fmt.t

  module Infix : sig
    val ( = ) : t -> t -> bool
    val ( <= ) : t -> t -> bool
  end
end

module type Preorder_laws = sig
  type t

  (** [a <= a] *)
  val reflexivity : t -> bool

  (** [(a <= b && b <= c) => (a <= c)] *)
  val transitivity : t -> t -> t -> bool
end

module type Partial_order = sig
  include Preorder
end

module type Partial_order_laws = sig
  type t

  include Preorder_laws with type t := t

  (** [a <= b && b <= a => a = b] *)
  val antisymmetry : t -> t -> bool
end

module type Equivalence = sig
  include Preorder
end

module type Equivalence_laws = sig
  include Preorder_laws

  (** [a <= b => b <= a] *)
  val symmetry : t -> t -> bool
end

module type Total_order = sig
  include Partial_order
end

module type Total_order_laws = sig
  include Partial_order_laws

  (** [a <= b or b <= a] *)
  val totality : t -> t -> bool
end
