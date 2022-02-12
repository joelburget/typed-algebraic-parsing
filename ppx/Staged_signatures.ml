open Ppxlib

type _ code = expression

(** Same as [Signatures.Token] but must support [within] as well. *)
module type Token = sig
  include Signatures.Token

  val within : tag -> tag -> t code -> bool code
end

(*
module type Token_stream = sig
  include Signatures.Token_stream

  module Token :
    Token with type t = token and type tag = token_tag and type set = token_set
end
   *)

(** Same as [Signatures.Token_stream] but must support [within] as well. *)
module type Token_stream = Signatures.Token_stream

module type Ir = sig
  type token
  type token_tag
  type token_set
  type 'a t
  type 'a stream

  module Token :
    Signatures.Token with type t = token and type tag = token_tag and type set = token_set

  val return : expression -> 'a t
  val ( >>= ) : 'a t -> (expression -> 'b t) -> 'b t
  val fail : string -> 'a t
  val junk : unit t
  val peek_mem : token_set -> ([ `Yes | `No | `Eof ] -> 'b t) -> 'b t
  val peek : token_tag list -> ([ `Yes of Uchar.t code | `No | `Eof ] -> 'b t) -> 'b t
  val fix : ('b t -> 'b t) -> 'b t

  module Codegen : sig
    val generate : 'a t -> 'a stream code
  end
end

module type Parser = sig
  include Signatures.Parser

  type 'a typechecked = (unit, 'a, Type.t) Grammar.t

  val typecheck : 'a Construction.t -> 'a typechecked
  val compile : 'a typechecked -> 'a Caml.Stream.t code
end
