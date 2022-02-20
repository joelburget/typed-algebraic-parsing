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

  (** {!union} *)
  val ( + ) : t -> t -> t

  (** {!inter} *)
  val ( * ) : t -> t -> t

  (** {!asymmetric_diff} *)
  val ( - ) : t -> t -> t
end

module Interval : sig
  type t = interval

  val first : interval -> Uchar.t
  val last : interval -> Uchar.t
  val to_tuple : interval -> Uchar.t * Uchar.t
end

module Laws : Structures.Laws.S with type t = t

val pp : t Fmt.t
val pp_char : Uchar.t Fmt.t

(** The empty character class [\[\]] *)
val empty : t

(** Any character ([.]). *)
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

(** Negate the given class. [\[^...\] -> \[...\]], [\[...\] -> \[^...\]] *)
val negate : t -> t

(** Union of classes, eg [\[ab\] || \[ace\] -> \[abce\]] *)
val union : t -> t -> t

(** Intersection of classes, eg [\[ab\] && \[ace\] -> \[a\]] *)
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

(** Helpers for non-unicode [char]s. *)
module Char : sig
  type element = char

  (** A single (ASCII) character. *)
  val singleton : char -> t

  (** Inclusive range. *)
  val range : char -> char -> t

  (** Is the character contained in this class? *)
  val mem : t -> char -> bool

  (** Any of the characters in the list. *)
  val of_list : char list -> t
end

(** Common ranges (digit, alphanum, etc). *)
module Common : sig
  (** '0' - '9' *)
  val digit : t

  (** 'a' - 'z' *)
  val lowercase : t

  (** 'A' - 'Z' *)
  val uppercase : t

  (** 'a' - 'z', 'A' - 'Z' *)
  val alpha : t

  (** 'a' - 'z', 'A' - 'Z', '0' - '9' *)
  val alphanum : t

  (** Print characters (ASCII 32 (' ') - 126 ('~')). *)
  val print : t

  (** ' ', '\t', '\r', '\n' *)
  val whitespace : t
end
