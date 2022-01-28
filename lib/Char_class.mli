type t

include Base.Comparable.S with type t := t
include Base.Sexpable.S with type t := t

val pp : t Fmt.t
val empty : t
val any : t
val singleton : Uchar.t -> t
val of_char : char -> t
val range : Uchar.t -> Uchar.t -> t
val of_list : Uchar.t list -> t
val of_string : string -> t
val negate : t -> t
val union : t -> t -> t
val inter : t -> t -> t
val mem : t -> Uchar.t -> bool
val is_empty : t -> bool
val choose : t -> Uchar.t option
val choose_exn : t -> Uchar.t

module Ring_laws : sig
  (** [(a + b) + c = a + (b + c)] *)
  val plus_associative : t -> t -> t -> bool

  (** [a + b = b + a] *)
  val plus_commutative : t -> t -> bool

  (** [a + 0 = a] *)
  val plus_ident : t -> bool

  (** [a + (-a) = 0] *)
  val plus_inverse : t -> bool

  (** [(a * b) * c = a * (b * c)] *)
  val mul_associative : t -> t -> t -> bool

  (** [a * 1 = a] *)
  val mul_ident : t -> bool

  (** [a * (b + c) = (a * b) + (a * c)] *)
  val left_distributive : t -> t -> t -> bool

  (** [(b + c) * a = (b * a) + (c * a)] *)
  val right_distributive : t -> t -> t -> bool
end
