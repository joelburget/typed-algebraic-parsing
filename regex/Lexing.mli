type 'a action =
  | Skip
  | Error of string
  | Return of 'a

(** A lexer is a list of regex, action pairs.

    If serveral regexes match a prefix of the input, we use the longest match. If there
    are multiple longest matches we use the earlier rule. These are the same precedence
    rules that ocamllex uses. *)
type 'a t = (Regex.t * 'a action) list

val lex : 'a t -> string -> ('a action list, int) Result.t
