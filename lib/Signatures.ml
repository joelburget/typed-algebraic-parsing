(** A stream of elements. *)
module type Stream = sig
  type element
  type t

  val peek : t -> element option
  val junk : t -> unit
end

(** A single token and set of tokens. *)
module type Token = sig
  type t
  type tag
  type set
  type interval

  val compare : tag -> tag -> int
  val ( = ) : tag -> tag -> bool
  val tag : t -> tag
  val pp : t Fmt.t
  val pp_tag : tag Fmt.t
  val pp_set : set Fmt.t

  module Set : sig
    type t = set

    val pp : t Fmt.t
    val any : t
    val empty : t
    val singleton : tag -> t
    val is_empty : t -> bool
    val ( = ) : t -> t -> bool
    val inter : t -> t -> t
    val union : t -> t -> t
    val mem : t -> tag -> bool
    val is_subset : t -> t -> bool
    val of_list : tag list -> t
    val intervals : t -> interval list

    module Infix : sig
      (** [asymmetric_diff] *)
      val ( - ) : t -> t -> t
    end
  end

  module Interval : sig
    type t = interval

    val to_tuple : t -> tag * tag
  end
end

(** A stream of tokens. *)
module type Token_stream = sig
  type token
  type token_tag
  type token_set
  type stream

  module Token :
    Token with type t = token and type tag = token_tag and type set = token_set

  module Stream : Stream with type element = token and type t = stream
end

(** The type of parsers. *)
module type Type = sig
  module Token : Token

  type t =
    { first : Token.Set.t
    ; flast : Token.Set.t
    ; null : bool
    ; guarded : bool
    }

  val pp : t Fmt.t
  val bot : t
  val eps : t
  val tok : Token.Set.t -> t
  val alt : t -> t -> t
  val seq : t -> t -> t
  val star : t -> t
  val fix : (t -> t) -> t
end

(** Constructing parsers. *)
module type Parse = sig
  type 'a t
  type token_tag
  type type_

  val eps : 'a -> 'a t
  val tok : token_tag -> token_tag t
  val bot : _ t
  val seq : 'a t -> 'b t -> ('a * 'b) t
  val alt : type_ -> 'a t -> type_ -> 'a t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

(** A [Var]-indexed set of elements. *)
module type Env = sig
  type 'a elem_t

  type 'ctx t =
    | [] : unit t
    | ( :: ) : 'a elem_t * 'ctx t -> ('a * 'ctx) t

  (* val lookup : 'ctx t -> ('ctx, 'a) Var.t -> 'a elem_t *)

  (* type fn = { f : 'a. 'a elem_t -> 'a elem_t } *)

  (* val map : fn -> 'ctx t -> 'ctx t *)
end

(** First-order (GADT) representation of a grammar, indexed by the type of value the
    parser will produce. *)
module type Grammar = sig
  type _ type_env
  type type_

  (** - ['ctx]: context
      - ['a]: return type
      - ['d]: data *)
  type ('ctx, 'a, 'd) t

  val typeof : 'ctx type_env -> ('ctx, 'a, 'd) t -> ('ctx, 'a, type_) t
end

module type Construction = sig
  type 'a ctx
  type ('ctx, 'a, 'd) grammar
  type token
  type token_tag

  (** Values produced by parsers *)
  type 'a v

  (* TODO: don't expose this type *)
  type 'a t = { tdb : 'ctx. 'ctx ctx -> ('ctx, 'a, unit) grammar }

  val eps : 'a v -> 'a t
  val tok : token_tag list -> token t
  val bot : 'a t
  val seq : 'a t -> 'b t -> ('a * 'b) t
  val alt : 'a t -> 'a t -> 'a t
  val map : ('a v -> 'b v) -> 'a t -> 'b t
  val fix : ('b t -> 'b t) -> 'b t
  val star : 'a t -> 'a list t
end

module type Library = sig
  type 'a t

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

module type Parser = sig
  type token
  type token_tag
  type stream
  type 'a parser
  type 'a v

  module Token : Token with type t = token and type tag = token_tag
  module Stream : Stream with type element = Token.t and type t = stream
  module Type : Type with module Token = Token
  module Type_env : Env
  module Parse_env : Env
  module Grammar : Grammar with type 'a type_env := 'a Type_env.t and type type_ := Type.t

  module Construction : sig
    module Ctx : Env

    include
      Construction
        with type 'a ctx = 'a Ctx.t
         and type ('ctx, 'a, 'd) grammar = ('ctx, 'a, 'd) Grammar.t
         and type token = token
         and type token_tag = token_tag
         and type 'a v = 'a v
  end

  val typeof : 'ctx Type_env.t -> ('ctx, 'a, 'd) Grammar.t -> Type.t
  val parse : ('ctx, 'a, Type.t) Grammar.t -> 'ctx Parse_env.t -> 'a parser
end

type string_stream = (Uutf.decoder * Uchar.t option) ref

module type String_parser = sig
  include
    Parser
      with type token = Uchar.t
       and type token_tag = Uchar.t
       and type stream = string_stream
       and type 'a v = 'a
       and type 'a parser = string_stream -> 'a

  module Library : Library with type 'a t := 'a Construction.t

  module Stream : sig
    include Stream with type element = Token.t and type t = stream

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
