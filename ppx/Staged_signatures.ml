open Ppxlib

type _ code = expression

module type Ir = sig
  type 'a t
  type 'a stream

  module Token :
    Signatures.Token
      with type t = Uchar.t
       and type tag = Uchar.t
       and type set = Char_class.t

  val return : expression -> 'a t
  val ( >>= ) : 'a t -> (expression -> 'b t) -> 'b t
  val fail : string -> 'a t
  val junk : unit t
  val peek_mem : Token.Set.t -> ([ `Yes | `No | `Eof ] -> 'b t) -> 'b t
  val peek : Token.tag list -> ([ `Yes of Uchar.t code | `No | `Eof ] -> 'b t) -> 'b t
  val fix : ('b t -> 'b t) -> 'b t

  module Codegen : sig
    val generate : 'a t -> 'a stream code
  end
end

module type Parser = sig
  include Signatures.Parser

  type 'a typechecked = (unit, 'a, Type.t) Grammar.t

  val typecheck : 'a Construction.t -> 'a typechecked

  module Compile (Ast : Ast_builder.S) : sig
    val compile : 'a typechecked -> 'a Caml.Stream.t code
  end
end
