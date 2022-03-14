type t

include Base.Comparable.S with type t := t
include Base.Sexpable.S with type t := t

module Infix : sig
  include Base.Comparable.Infix with type t := t

  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
end

module Laws : Structures.Laws.S with type t = t

val pp : t Fmt.t

(** The empty regex doesn't accept any string. *)
val empty : t

(** Accepts any string. *)
val any : t

(** Accepts only the empty string. *)
val eps : t

(** Sequence of two regexes. *)
val ( >>> ) : t -> t -> t

(** A set of characters. *)
val char_class : Char_class.t -> t

(** A set of characters. *)
val char_class_of_string : string -> t

(** A single character. *)
val chr : char -> t

(** A single character. *)
val uchar : Uchar.t -> t

(** A single string. *)
val str : string -> t

(** 0 or more repetitions of a regex. *)
val star : t -> t

(** 1 or more repetitions of a regex. *)
val plus : t -> t

(** Matches when either regex matches. *)
val ( || ) : t -> t -> t

(** Matches when both regexes match. *)
val ( && ) : t -> t -> t

(** The complement of a regex. Matches when the original fails to match and vice-versa. *)
val complement : t -> t

(** A regex is nullable if it accepts the empty string *)
val nullable : t -> bool

(** Take the one-character derivative of a regex. *)
val delta : Uchar.t -> t -> t

(** Take the iterated derivative of a regex. *)
val string_delta : string -> t -> t

(** Does the regex match this entire string? *)
val is_full_match : string -> t -> bool

(** Does the regex match a prefix of this string? *)
val matches_prefix : string -> t -> bool

(** Try to match a prefix of the string, returning the position of the first non-matching
    character. *)
val match_prefix : string -> t -> int option

(** Try to match a prefix of the string, starting at the given position, returning the
    position of the first non-matching character. *)
val match_prefix' : string -> int -> t -> int option

(** Compute an approximate derivative class. *)
val class' : t -> Derivative_class.t
