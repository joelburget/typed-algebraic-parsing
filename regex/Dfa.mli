open Base

module type S = sig
  type t

  include Base.Comparable.S with type t := t

  val pp : t Fmt.t
  val class' : t -> (Char_class.t, Char_class.comparator_witness) Set.t
  val nullable : t -> bool
  val delta : Uchar.t -> t -> t
end

module Make (T : S) : sig
  type t =
    { state_numbers : (int * T.t) list
    ; accepting : int list
    ; transitions : ((int * int) * Char_class.t) list
    }

  val pp : t Fmt.t
  val make : T.t -> t
end
