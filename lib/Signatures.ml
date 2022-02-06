module type Stream = sig
  type element
  type t

  val peek : t -> element option
  val junk : t -> unit
end

module type Token = sig
  type t

  val ( = ) : t -> t -> bool
  val pp : t Fmt.t

  module Set : sig
    type element = t
    type t

    val pp : t Fmt.t
    val empty : t
    val singleton : element -> t
    val is_empty : t -> bool
    val ( = ) : t -> t -> bool
    val inter : t -> t -> t
    val union : t -> t -> t
    val mem : t -> element -> bool
  end
end

module type Token_stream = sig
  type token
  type stream

  module Token : Token with type t = token
  module Stream : Stream with type element = token and type t = stream
end

module type Parser = sig
  type token
  type stream
  type 'a parser = stream -> 'a

  exception Parse_error of string

  module Stream : Stream with type element = token and type t = stream
  module Token : Token with type t = token

  (* TODO: expose fewer modules *)
  module Type : sig
    type t

    val pp : t Fmt.t
    val bot : t
    val eps : t
    val tok : token -> t
    val alt : t -> t -> t
    val seq : t -> t -> t
    val star : t -> t
    val fix : (t -> t) -> t
  end

  module Parse : sig
    val eps : unit parser
    val tok : token -> token parser
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

  module Grammar : sig
    type ('ctx, 't, 'd) t

    val typeof : 'ctx Type_env.t -> ('ctx, 'a, 'd) t -> ('ctx, 'a, Type.t) t
  end

  val typeof : 'ctx Type_env.t -> ('ctx, 'a, 'd) Grammar.t -> Type.t
  val parse : ('ctx, 'a, Type.t) Grammar.t -> 'ctx Parse_env.t -> 'a parser

  module Construction : sig
    module Ctx : Env_s

    (* TODO: don't expose this type *)
    type 'a t = { tdb : 'ctx. 'ctx Ctx.t -> ('ctx, 'a, unit) Grammar.t }

    val eps : unit t
    val tok : token -> token t
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

      val always : 'a -> 'b -> 'a
      val ( ++ ) : 'a t -> 'b t -> ('a * 'b) t
      val ( ==> ) : 'a t -> ('a -> 'b) -> 'b t
      val choice : 'a t list -> 'a t
      val option : 'a t -> 'a option t
      val plus : 'a t -> 'a list t
      val infixr : ('a -> 'a -> 'a) t -> 'a t -> 'a t
      val infixl : ('a -> 'a -> 'a) t -> 'a t -> 'a t
      val infix : (assoc * ('a -> 'a -> 'a) t) list -> 'a t -> 'a t
      val sep_by : 'a t -> 'b t -> 'b list t
      val sep_by1 : 'a t -> 'b t -> 'b list t
      val ( <* ) : 'a t -> _ t -> 'a t
      val ( *> ) : _ t -> 'a t -> 'a t
    end
  end
end

module type String_parsers = sig
  include
    Parser with type token = Uchar.t and type stream = (Uutf.decoder * Uchar.t option) ref

  module Stream : sig
    include Stream with type element = token and type t = stream

    val of_string : string -> t
  end

  val ctok : char -> Uchar.t Construction.t
  val charset : string -> Uchar.t Construction.t
  val lower : Uchar.t Construction.t
  val upper : Uchar.t Construction.t

  module Sexp : sig
    type sexp =
      | Sym of string
      | Seq of sexp list

    val pp : sexp Fmt.t
    val paren : 'a Construction.t -> 'a Construction.t
    val sexp : sexp Construction.t
  end

  module Arith : sig
    val num : float Construction.t
    val arith : float Construction.t
  end
end
