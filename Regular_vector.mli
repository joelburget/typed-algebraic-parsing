(** A list of regular expressions, used to handle multiple regular expressions in
    parallel, as in a lexer. *)
type t = Regex.t list

include Base.Comparable.S with type t := t
include Base.Sexpable.S with type t := t

val pp : t Fmt.t

(** Take the derivative of each regex in parallel. *)
val delta : Uchar.t -> t -> t

(** The iterated derivative of each regex in parallel. *)
val string_delta : string -> t -> t

(** Does the regular vector contain a nullable regex? This is equivalent to being
    accepting. *)
val nullable : t -> bool

(** Compute an approximate derivative class. *)
val class' : t -> Derivative_class.t

val derivatives : t -> (t * Char_class.t) list
