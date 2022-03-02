open Ppxlib
open Taparse

type _ code = expression

module type Stream = sig
  type element
  type t

  (** Information used during codegen. *)
  type context

  val peek : t -> element option
  val junk : t -> unit

  val staged_peek
    :  loc:location
    -> context
    -> (context -> element code option -> 'a code)
    -> 'a code

  val staged_junk : context -> (context -> 'a code) -> 'a code

  (* val return : t -> 'a code -> 'a return code *)
  val init : loc:location -> (context -> _ code) -> _ code

  type 'a mkcall = { mkcall : 'b. context -> (context -> 'a code -> 'b code) -> 'b code }

  val genfun : loc:location -> (context -> 'a mkcall -> 'a code) -> unit
end

module type Interval = sig
  type t
  type tag

  val to_tuple : t -> tag * tag
  val to_pattern : loc:location -> t list -> pattern * expression option
end

module type Token_set = sig
  include Signatures.Token_set

  type interval

  val intervals : t -> interval list
  val any : t
end

module type Token = sig
  include Signatures.Token

  type interval

  val unquote : loc:location -> expression
  val quote : loc:location -> tag -> expression
  val reflect : expression -> t option

  module Set :
    Token_set with type t = set and type tag := tag and type interval = interval

  module Interval : Interval with type t = interval and type tag := tag
end

(** Similar to [Signatures.Token_stream] but for code generation. *)
module type Token_stream = sig
  type token
  type token_tag
  type token_set
  type stream

  module Token :
    Token with type t = token and type tag = token_tag and type set = token_set

  module Stream : Stream with type element = token and type t = stream
end

module type Ir = sig
  type token
  type token_tag
  type token_set
  type 'a t
  type 'a stream

  module Token :
    Token with type t = token and type tag = token_tag and type set = token_set

  val return : 'a code -> 'a t
  val ( >>= ) : 'a t -> ('a code -> 'b t) -> 'b t
  val fail : string -> 'a t
  val junk : unit t
  val peek_mem : token_set -> ([ `Yes | `No | `Eof ] -> 'b t) -> 'b t
  val peek : token_tag list -> ([ `Yes of Uchar.t code | `No | `Eof ] -> 'b t) -> 'b t
  val fix : ('b t -> 'b t) -> 'b t

  module Codegen : sig
    val generate : 'a t -> 'a stream code
  end
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

module type Parser = sig
  type token
  type token_tag
  type stream
  type 'a parser
  type 'a v

  module Token : Token with type t = token and type tag = token_tag
  module Stream : Stream with type element = Token.t and type t = stream
  module Type : Type with module Token = Token
  module Type_env : Signatures.Env
  module Parse_env : Signatures.Env

  module Grammar :
    Signatures.Grammar with type 'a type_env := 'a Type_env.t and type type_ := Type.t

  type 'a typechecked = (unit, 'a, Type.t) Grammar.t

  module Construction : sig
    module Ctx : Signatures.Env

    include
      Signatures.Construction
        with type 'a ctx = 'a Ctx.t
         and type ('ctx, 'a, 'd) grammar = ('ctx, 'a, 'd) Grammar.t
         and type token = token
         and type token_tag = token_tag
         and type 'a v = 'a v
  end

  val typeof : 'ctx Type_env.t -> ('ctx, 'a, 'd) Grammar.t -> Type.t
  val parse : ('ctx, 'a, Type.t) Grammar.t -> 'ctx Parse_env.t -> 'a parser
  val typecheck : 'a Construction.t -> 'a typechecked
  val compile : 'a typechecked -> 'a Caml.Stream.t code
end
