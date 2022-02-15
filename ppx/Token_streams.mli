module Char_token :
  Staged_signatures.Token
    with type t = char
     and type tag = char
     and type set = Char_class.t
     and type interval = Char_class.interval

module Char :
  Staged_signatures.Token_stream
    with type token = char
     and type token_tag = char
     and type stream = char Stdlib.Stream.t
