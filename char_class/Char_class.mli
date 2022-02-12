(** Represents a set of characters. Character classes can be positive or negative:

    - [\[abc\]] (positive) represents the characters 'a', 'b', and 'c'
    - [\[^abc\]] (negative) represents all characters other than 'a', 'b', and 'c'

    Fun note: Character classes form a ring with zero [empty], one [any], binary
    operations [union] and [inter], and negation. *)
type t

type element = Uchar.t
type interval

include Base.Comparable.S with type t := t
include Base.Sexpable.S with type t := t

module Infix : sig
  include Base.Comparable.Infix with type t := t

  (** [union] *)
  val ( + ) : t -> t -> t

  (** [inter] *)
  val ( * ) : t -> t -> t

  (** [asymmetric_diff] *)
  val ( - ) : t -> t -> t
end

module Interval : sig
  val x : interval -> Uchar.t
  val y : interval -> Uchar.t
end

module Laws : Laws.S with type t = t

val pp : t Fmt.t
val pp_char : Uchar.t Fmt.t

(** The empty character class [\[\]] *)
val empty : t

(** Any character [.]. *)
val any : t

(** A single unicode character. *)
val singleton : Uchar.t -> t

(** Inclusive range. *)
val range : Uchar.t -> Uchar.t -> t

(** Any of the characters in the list. *)
val of_list : Uchar.t list -> t

(** Any of the characters in the string. *)
val of_string : string -> t

(** Asymmetric diff, examples:

    - [\[cd\] - d = c]
    - [\[^c\] - d = \[^cd\]]
    - [\[cd\] - \[^d\] = d]
    - [\[^c\] - \[^cd\] = d] *)
val asymmetric_diff : t -> t -> t

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

(** Is [a] a subset of [b]? *)
val is_subset : t -> t -> bool

(** Choose any character from this class. *)
val choose : t -> Uchar.t option

(** Choose any character from this class. *)
val choose_exn : t -> Uchar.t

(** A list of all intervals *)
val intervals : t -> interval list

module Char : sig
  type element = char

  (** A single (ASCII) character. *)
  val singleton : char -> t

  (** Inclusive range. *)
  val range : char -> char -> t

  (** Is the character contained in this class? *)
  val mem : t -> char -> bool
end
