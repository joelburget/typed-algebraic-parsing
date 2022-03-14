type 'a action =
  | Skip
  | Error of string
  | Return of 'a

(** A lexer is a list of regex, action pairs.

    If serveral regexes match a prefix of the input, we use the longest match. If there
    are multiple longest matches we use the earlier rule. These are the same precedence
    rules that ocamllex uses. *)
type 'a t = (Regex.t * 'a action) list

(** Location of a matched token. [start] is the location of the first character matched,
    [finish] is one past the end. *)
type match_location =
  { start : int
  ; finish : int
  }

type 'a match_ = match_location * 'a action

(** Lex an entire string. *)
val lex : 'a t -> string -> ('a match_ list, int) Result.t

(** Lex starting from the given point in the string. *)
val lex' : 'a t -> string -> int -> ('a match_ list, int) Result.t
