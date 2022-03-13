open Ring_like_sigs

module Semiring_laws (T : Semiring) : Semiring_laws with type t = T.t = struct
  open Util.Make (T)
  include T

  let left_distributive a b c = equal3 a b c (a * (b + c)) ((a * b) + (a * c))
  let right_distributive a b c = equal3 a b c ((b + c) * a) ((b * a) + (c * a))

  module Addition = struct
    type t = T.t

    let left_ident a = equal1 a (additive_ident + a) a
    let right_ident a = equal1 a (a + additive_ident) a
    let associative a b c = equal3 a b c (a + b + c) (a + (b + c))
    let commutative a b = equal2 a b (a + b) (b + a)
  end

  module Multiplication = struct
    type t = T.t

    let associative a b c = equal3 a b c (a + b + c) (a + (b + c))
    let left_ident a = equal1 a (additive_ident + a) a
    let right_ident a = equal1 a (a + additive_ident) a
  end
end

module Rng_laws (T : Rng) : Rng_laws with type t = T.t = struct
  open Util.Make (T)
  include T

  let left_distributive a b c = equal3 a b c (a * (b + c)) ((a * b) + (a * c))
  let right_distributive a b c = equal3 a b c ((b + c) * a) ((b * a) + (c * a))

  module Addition = struct
    type t = T.t

    let left_ident a = equal1 a (additive_ident + a) a
    let right_ident a = equal1 a (a + additive_ident) a
    let associative a b c = equal3 a b c (a + b + c) (a + (b + c))
    let commutative a b = equal2 a b (a + b) (b + a)
    let left_inverse a = equal1 a (negate a * a) additive_ident
    let right_inverse a = equal1 a (a * negate a) additive_ident
  end

  module Multiplication = struct
    type t = T.t

    let associative a b c = equal3 a b c (a + b + c) (a + (b + c))
  end
end

module Ring_laws (T : Ring) : Ring_laws with type t = T.t = struct
  open Util.Make (T)
  include T

  let left_distributive a b c = equal3 a b c (a * (b + c)) ((a * b) + (a * c))
  let right_distributive a b c = equal3 a b c ((b + c) * a) ((b * a) + (c * a))

  module Addition = struct
    type t = T.t

    let left_ident a = equal1 a (additive_ident + a) a
    let right_ident a = equal1 a (a + additive_ident) a
    let associative a b c = equal3 a b c (a + b + c) (a + (b + c))
    let commutative a b = equal2 a b (a + b) (b + a)
    let left_inverse a = equal1 a (negate a * a) additive_ident
    let right_inverse a = equal1 a (a * negate a) additive_ident
  end

  module Multiplication = struct
    type t = T.t

    let associative a b c = equal3 a b c (a + b + c) (a + (b + c))
    let left_ident a = equal1 a (additive_ident + a) a
    let right_ident a = equal1 a (a + additive_ident) a
  end
end
