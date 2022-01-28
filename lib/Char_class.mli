(** Represents a set of characters. Character classes can be positive or negative:

    - [\[abc\]] (positive) represents the characters 'a', 'b', and 'c'
    - [\[^abc\]] (negative) represents all characters other than 'a', 'b', and 'c'

    Fun note: Character classes form a ring with zero [empty], one [any], binary
    operations [union] and [inter], and negation. *)
type t

include Base.Comparable.S with type t := t
include Base.Sexpable.S with type t := t

val pp : t Fmt.t

(** The empty character class [\[\]] *)
val empty : t

(** Any character [.]. *)
val any : t

(** A single unicode character. *)
val singleton : Uchar.t -> t

(** A single (ASCII) character. *)
val of_char : char -> t

(** Inclusive range. *)
val range : Uchar.t -> Uchar.t -> t

(** Inclusive range. *)
val crange : char -> char -> t

(** Any of the characters in the list. *)
val of_list : Uchar.t list -> t

(** Any of the characters in the string. *)
val of_string : string -> t

(** [\[^...\] -> \[...\]], [\[...\] -> \[^...\]] *)
val negate : t -> t

(** Eg [\[ab\] || \[ace\] -> \[abce\]] *)
val union : t -> t -> t

(** Eg [\[ab\] && \[ace\] -> \[a\]] *)
val inter : t -> t -> t

(** Is the character contained in this class? *)
val mem : t -> Uchar.t -> bool

(** Is this class empty? *)
val is_empty : t -> bool

(** Choose any character from this class. *)
val choose : t -> Uchar.t option

(** Choose any character from this class. *)
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
