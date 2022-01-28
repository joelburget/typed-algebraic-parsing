(** Equivalence classes of symbols for a given regex.

    Examples:

    - For regex [\[a-z\]+] the classes are [\[a-z\]] and [\[^a-z\]]
    - For regex [a + ba + c] the classes are [\[ac\]], [\[b\]], and [\[^abc\]]. *)
type t = (Char_class.t, Char_class.comparator_witness) Base.Set.t

val pp : t Fmt.t

(** Trivial derivative class, the singleton set of any character ([.]). *)
val trivial : t

(** Cross product (intersection) of two sets of character sets. *)
val cross : t -> t -> t
