(* val typeof : type ctx a d. ctx tp_env -> (ctx, a) Grammar.t -> Tp.t *)
(* val parse : type ctx a d.  (ctx, a) Grammar.t -> ctx parse_env -> a parser *)
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
  type ('ctx, 't) t
end

module Env (T : sig
  type 'a t
end) : sig
  type 'ctx t =
    | Empty : unit t
    | Nonempty : 'a T.t * 'ctx t -> ('a * 'ctx) t

  val lookup : 'ctx t -> ('ctx, 'a) Var.t -> 'a T.t

  type fn = { f : 'a. 'a T.t -> 'a T.t }

  val map : fn -> 'ctx t -> 'ctx t
end

type _ type_env
type _ parse_env

val typeof : 'ctx type_env -> ('ctx, 'a) Grammar.t -> Type.t
val parse : ('ctx, 'a) Grammar.t -> 'ctx parse_env -> 'a parser

module Hoas : sig
  type 't t

  val eps : unit t
  val chr : char -> char t
  val bot : 'a t
  val seq : 'a t -> 'b t -> ('a * 'b) t
  val alt : 'a t -> 'a t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fix : ('b t -> 'b t) -> 'b t

  module Library : sig
    type assoc =
      | Left
      | Right

    val any : 'a t list -> 'a t
    val option : 'a t -> 'a option t
    val star : 'a t -> 'a list t
    val plus : 'a t -> 'a list t
    val charset : string -> char t
    val lower : char t
    val upper : char t
    val infixr : ('a -> 'a -> 'a) t -> 'a t -> 'a t
    val infixl : ('a -> 'a -> 'a) t -> 'a t -> 'a t
    val infix : (assoc * ('a -> 'a -> 'a) t) list -> 'a t -> 'a t

    module Sexp : sig
      type token =
        | SYMBOL of string
        | LPAREN
        | RPAREN

      val symbol : token t
      val lparen : token t
      val rparen : token t
      val token : token t

      type sexp =
        | Sym
        | Seq of sexp list

      val paren : 'a t -> 'a t
      val sexp : sexp t
    end

    module Arith : sig
      val num : float t
      val arith : float t
    end
  end
end

(*
  exception Type_error of string

  val parser : 'a t -> char Stream.t -> 'a
     *)
