module Monoid = struct
  module type S = Group_like_sigs.Monoid

  module Free (T : sig
    type t

    val ( = ) : t -> t -> bool
  end) =
  struct
    type t = T.t list

    let identity = []
    let ( <> ) xs ys = xs @ ys
    let ( = ) = List.equal T.( = )
  end
end

module Commutative_monoid = struct
  module type S = Group_like_sigs.Commutative_monoid
end
