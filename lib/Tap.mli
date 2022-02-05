type 'a parser = char Stream.t -> 'a

module Type : sig
  type t

  val bot : t
  val eps : t
  val chr : char -> t
  val alt : t -> t -> t
  val seq : t -> t -> t
  val star : t -> t
  val fix : (t -> t) -> t
end

module Parse : sig
  val eps : unit parser
  val chr : char -> char parser
  val bot : _ parser
  val seq : 'a parser -> 'b parser -> ('a * 'b) parser
  val alt : Type.t -> 'a parser -> Type.t -> 'a parser -> 'a parser
  val map : ('a -> 'b) -> 'a parser -> 'b parser
end

module Var : sig
  type ('ctx, 'a) t =
    | Z : ('a * 'ctx, 'a) t
    | S : ('rest, 'a) t -> ('b * 'rest, 'a) t
end

module Grammar : sig
  type ('ctx, 't, 'd) t
end

module type Env_s = sig
  type 'a elem_t

  type 'ctx t =
    | [] : unit t
    | ( :: ) : 'a elem_t * 'ctx t -> ('a * 'ctx) t

  val lookup : 'ctx t -> ('ctx, 'a) Var.t -> 'a elem_t

  type fn = { f : 'a. 'a elem_t -> 'a elem_t }

  val map : fn -> 'ctx t -> 'ctx t
end

module Env (T : sig
  type 'a t
end) : Env_s

module Type_env : Env_s
module Parse_env : Env_s

val typeof : 'ctx Type_env.t -> ('ctx, 'a, 'd) Grammar.t -> Type.t
val parse : ('ctx, 'a, Type.t) Grammar.t -> 'ctx Parse_env.t -> 'a parser

module Construction : sig
  type 't t

  val eps : unit t
  val chr : char -> char t
  val bot : 'a t
  val seq : 'a t -> 'b t -> ('a * 'b) t
  val alt : 'a t -> 'a t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fix : ('b t -> 'b t) -> 'b t
  val star : 'a t -> 'a list t

  module Library : sig
    type assoc =
      | Left
      | Right

    val ( ++ ) : 'a t -> 'b t -> ('a * 'b) t
    val ( ==> ) : 'a t -> ('a -> 'b) -> 'b t
    val any : 'a t list -> 'a t
    val option : 'a t -> 'a option t
    val plus : 'a t -> 'a list t
    val charset : string -> char t
    val lower : char t
    val upper : char t
    val infixr : ('a -> 'a -> 'a) t -> 'a t -> 'a t
    val infixl : ('a -> 'a -> 'a) t -> 'a t -> 'a t
    val infix : (assoc * ('a -> 'a -> 'a) t) list -> 'a t -> 'a t
    val sep_by : 'a t -> 'b t -> 'b list t
    val sep_by1 : 'a t -> 'b t -> 'b list t

    module Sexp : sig
      type sexp =
        | Sym of string
        | Seq of sexp list

      val pp : sexp Fmt.t
      val paren : 'a t -> 'a t
      val sexp : sexp t
    end

    module Arith : sig
      val num : float t
      val arith : float t
    end
  end
end
