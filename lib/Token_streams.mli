module Uchar_token : Signatures.Token with type t = Uchar.t
module Char_token : Signatures.Token with type t = char

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
