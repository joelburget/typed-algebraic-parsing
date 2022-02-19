module Uchar_token :
  Signatures.Token
    with type t = Uchar.t
     and type tag = Uchar.t
     and type set = Char_class.t
     and type interval = Char_class.interval

module Char_token :
  Signatures.Token
    with type t = char
     and type tag = char
     and type set = Char_class.t
     and type interval = Char_class.interval

module Uchar : sig
  include
    Signatures.Token_stream
      with type token = Uchar.t
       and type token_tag = Uchar.t
       and type stream = (Uutf.decoder * Uchar.t option) ref

  val of_decoder : Uutf.decoder -> stream
end

module Char :
  Signatures.Token_stream
    with type token = char
     and type token_tag = char
     and type stream = char Stdlib.Stream.t